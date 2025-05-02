;; Complete marketplace with payment and refund features
(define-constant ADMIN tx-sender)
(define-constant ERROR-NOT-PERMITTED (err u403))
(define-constant ERROR-ITEM-NOT-FOUND (err u404))
(define-constant ERROR-BIDDING-ENDED (err u405))
(define-constant ERROR-BID-TOO-LOW (err u406))
(define-constant ERROR-SALE-IN-PROGRESS (err u407))
(define-constant ERROR-PAYMENT-FAILED (err u408))
(define-constant ERROR-BAD-INPUT (err u409))
(define-constant ERROR-BAD-IDENTIFIER (err u410))

;; Status codes for marketplace items
(define-constant STATUS-DRAFT u0)
(define-constant STATUS-LIVE u1)
(define-constant STATUS-FINISHED u2)
(define-constant STATUS-REVOKED u3)

;; System parameters
(define-constant MIN-PRICE-INCREASE u10)
(define-constant MAX-LISTING-BLOCKS u144000) ;; Approximately 1000 days
(define-constant MIN-LISTING-BLOCKS u1440) ;; Approximately 10 days

;; Marketplace listings storage
(define-map listings 
  { listing-id: uint }
  {
    title: (string-utf8 100),
    details: (optional (string-utf8 500)),
    base-price: uint,
    min-acceptable: (optional uint),
    top-offer: uint,
    top-buyer: (optional principal),
    open-block: uint,
    close-block: uint,
    status: uint,
    seller: principal,
    final-price: uint
  }
)

;; Offer history
(define-map offers
  { listing-id: uint, buyer: principal }
  { 
    amount: uint,
    block-height: uint
  }
)

;; Refund tracking
(define-map refunds
  { listing-id: uint, buyer: principal }
  { value: uint }
)

;; Listing counter
(define-data-var listing-counter uint u0)

;; Check if price increase is sufficient
(define-private (is-enough-increase 
  (current-amount uint) 
  (new-amount uint)
)
  (let 
    (
      (required-increase (+ current-amount 
        (/ (* current-amount MIN-PRICE-INCREASE) u100)
      ))
    )
    (>= new-amount required-increase)
  )
)

;; Verify listing configuration
(define-private (check-listing-config
  (base-price uint)
  (min-acceptable (optional uint))
  (open-block uint)
  (close-block uint)
)
  (and
    (> base-price u0)
    (< open-block close-block)
    (<= (- close-block open-block) MAX-LISTING-BLOCKS)
    (>= (- close-block open-block) MIN-LISTING-BLOCKS)
    (match min-acceptable
      price-floor (> price-floor base-price)
      true)
  )
)

;; Check if listing exists
(define-private (listing-exists? (id uint))
  (is-some (map-get? listings { listing-id: id }))
)

;; Create new marketplace listing
(define-public (publish-listing
  (title (string-utf8 100))
  (details (optional (string-utf8 500)))
  (base-price uint)
  (min-acceptable (optional uint))
  (open-block uint)
  (close-block uint)
)
  (begin
    ;; Validate listing parameters
    (asserts! 
      (check-listing-config 
        base-price 
        min-acceptable 
        open-block 
        close-block
      ) 
      ERROR-BAD-INPUT
    )

    ;; Get new listing ID
    (let 
      (
        (validated-id (var-get listing-counter))
      )
      ;; Store listing data
      (map-set listings 
        { listing-id: validated-id }
        {
          title: title,
          details: details,
          base-price: base-price,
          min-acceptable: min-acceptable,
          top-offer: base-price,
          top-buyer: none,
          open-block: open-block,
          close-block: close-block,
          status: STATUS-DRAFT,
          seller: tx-sender,
          final-price: u0
        }
      )

      ;; Update counter
      (var-set listing-counter (+ validated-id u1))

      ;; Return new ID
      (ok validated-id)
    )
  )
)

;; Submit an offer
(define-public (submit-offer
  (listing-id uint)
  (offer-amount uint)
  (current-block uint)
)
  (begin
    ;; Verify listing exists
    (asserts! (listing-exists? listing-id) ERROR-ITEM-NOT-FOUND)
    
    (let 
      (
        (listing (unwrap! 
          (map-get? listings { listing-id: listing-id }) 
          ERROR-ITEM-NOT-FOUND
        ))
        (current-top (get top-offer listing))
        (validated-id listing-id)
      )
      ;; Check listing is active
      (asserts! 
        (is-eq (get status listing) STATUS-LIVE) 
        ERROR-BIDDING-ENDED
      )
      (asserts! 
        (< current-block (get close-block listing)) 
        ERROR-BIDDING-ENDED
      )

      ;; Check offer meets minimum increase
      (asserts! 
        (is-enough-increase current-top offer-amount) 
        ERROR-BID-TOO-LOW
      )

      ;; Check against reserve price if set
      (match (get min-acceptable listing)
        min-price (asserts! (>= offer-amount min-price) ERROR-BID-TOO-LOW)
        true
      )

      ;; Update listing with new top offer
      (map-set listings 
        { listing-id: validated-id }
        (merge listing {
          top-offer: offer-amount,
          top-buyer: (some tx-sender)
        })
      )

      ;; Record the offer
      (map-set offers
        { listing-id: validated-id, buyer: tx-sender }
        { 
          amount: offer-amount,
          block-height: current-block
        }
      )

      (ok true)
    )
  )
)

;; Complete a sale
(define-public (complete-sale 
  (listing-id uint)
  (current-block uint)
)
  (begin
    ;; Verify listing exists
    (asserts! (listing-exists? listing-id) ERROR-ITEM-NOT-FOUND)
    
    (let 
      (
        (listing (unwrap! 
          (map-get? listings { listing-id: listing-id }) 
          ERROR-ITEM-NOT-FOUND
        ))
        (winning-buyer 
          (unwrap! (get top-buyer listing) ERROR-ITEM-NOT-FOUND)
        )
        (validated-id listing-id)
      )
      ;; Verify listing has ended
      (asserts! 
        (>= current-block (get close-block listing)) 
        ERROR-SALE-IN-PROGRESS
      )
      (asserts! 
        (is-eq (get status listing) STATUS-LIVE) 
        ERROR-BIDDING-ENDED
      )

      ;; Mark as completed
      (map-set listings
        { listing-id: validated-id }
        (merge listing {
          status: STATUS-FINISHED,
          final-price: (get top-offer listing)
        })
      )

      ;; Process payment to seller
      (try! (as-contract 
        (stx-transfer? 
          (get top-offer listing)
          tx-sender 
          (get seller listing)
        )
      ))

      (ok true)
    )
  )
)

;; Start accepting offers
(define-public (open-for-offers 
  (listing-id uint)
  (current-block uint)
)
  (begin
    ;; Verify listing exists
    (asserts! (listing-exists? listing-id) ERROR-ITEM-NOT-FOUND)
    
    (let 
      (
        (listing (unwrap! 
          (map-get? listings { listing-id: listing-id }) 
          ERROR-ITEM-NOT-FOUND
        ))
        (validated-id listing-id)
      )
      ;; Verify seller authorization
      (asserts! 
        (is-eq tx-sender (get seller listing)) 
        ERROR-NOT-PERMITTED
      )
      ;; Verify correct state
      (asserts! 
        (is-eq (get status listing) STATUS-DRAFT) 
        ERROR-SALE-IN-PROGRESS
      )
      (asserts! 
        (>= current-block (get open-block listing)) 
        ERROR-ITEM-NOT-FOUND
      )

      ;; Change status to live
      (map-set listings
        { listing-id: validated-id }
        (merge listing {
          status: STATUS-LIVE
        })
      )

      (ok true)
    )
  )
)

;; Request refund for outbid offer
(define-public (request-refund (listing-id uint))
  (begin
    ;; Verify listing exists
    (asserts! (listing-exists? listing-id) ERROR-ITEM-NOT-FOUND)
    
    (let 
      (
        (offer (unwrap! 
          (map-get? offers { listing-id: listing-id, buyer: tx-sender }) 
          ERROR-ITEM-NOT-FOUND
        ))
        (listing (unwrap! 
          (map-get? listings { listing-id: listing-id }) 
          ERROR-ITEM-NOT-FOUND
        ))
        (validated-id listing-id)
      )
      ;; Verify not top offer
      (asserts! 
        (not (is-eq 
          (some tx-sender) 
          (get top-buyer listing)
        )) 
        ERROR-NOT-PERMITTED
      )

      ;; Return funds
      (try! (as-contract 
        (stx-transfer? 
          (get amount offer)
          tx-sender 
          tx-sender
        )
      ))

      (ok true)
    )
  )
)

;; Query functions
(define-read-only (get-listing-info (listing-id uint))
  (begin
    ;; Safely check if listing exists
    (if (listing-exists? listing-id)
      (map-get? listings { listing-id: listing-id })
      none
    )
  )
)

(define-read-only (get-listing-status (listing-id uint))
  (begin
    ;; Safely check if listing exists
    (if (listing-exists? listing-id)
      (match (map-get? listings { listing-id: listing-id })
        listing-data (some (get status listing-data))
        none
      )
      none
    )
  )
)

(define-read-only (get-offer-info 
  (listing-id uint)
  (buyer principal)
)
  (map-get? offers { listing-id: listing-id, buyer: buyer })
)

(define-read-only (get-refund-status
  (listing-id uint) 
  (buyer principal)
)
  (map-get? refunds { listing-id: listing-id, buyer: buyer })
)