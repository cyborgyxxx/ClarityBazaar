;; Stage 1: Basic marketplace data structure and listing creation
(define-constant ADMIN tx-sender)
(define-constant ERROR-NOT-PERMITTED (err u403))
(define-constant ERROR-ITEM-NOT-FOUND (err u404))
(define-constant ERROR-BAD-INPUT (err u409))

;; Status codes for marketplace items
(define-constant STATUS-DRAFT u0)
(define-constant STATUS-LIVE u1)
(define-constant STATUS-FINISHED u2)

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

;; Listing counter
(define-data-var listing-counter uint u0)

;; Check if listing exists
(define-private (listing-exists? (id uint))
  (is-some (map-get? listings { listing-id: id }))
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
    (match min-acceptable
      price-floor (> price-floor base-price)
      true)
  )
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