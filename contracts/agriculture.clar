;; Agriculture Marketplace Contract

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-not-authorized (err u100))
(define-constant err-invalid-amount (err u101))

;; Data Maps
(define-map farmers 
    principal 
    {
        crop-type: (string-utf8 50),
        quantity: uint,
        price-per-unit: uint,
        available: bool
    }
)

(define-map investments
    { investor: principal, farmer: principal }
    {
        amount: uint,
        quantity: uint
    }
)

;; Public Functions
(define-public (register-crop (crop-type (string-utf8 50)) (quantity uint) (price-per-unit uint))
    (begin
        (map-set farmers tx-sender {
            crop-type: crop-type,
            quantity: quantity,
            price-per-unit: price-per-unit,
            available: true
        })
        (ok true)
    )
)

(define-public (invest-in-crop (farmer principal) (quantity uint))
    (let (
        (listing (unwrap! (map-get? farmers farmer) (err u102)))
        (total-cost (* quantity (get price-per-unit listing)))
    )
        (asserts! (<= quantity (get quantity listing)) (err u103))
        (asserts! (get available listing) (err u104))
        (try! (stx-transfer? total-cost tx-sender farmer))
        (map-set investments {investor: tx-sender, farmer: farmer}
            {
                amount: total-cost,
                quantity: quantity
            }
        )
        (ok true)
    )
)

;; Read Only Functions
(define-read-only (get-farmer-listing (farmer principal))
    (map-get? farmers farmer)
)

(define-read-only (get-investment (investor principal) (farmer principal))
    (map-get? investments {investor: investor, farmer: farmer})
)



;; Add to Data Maps
(define-map crop-ratings
    { farmer: principal, reviewer: principal }
    {
        rating: uint,  ;; 1-5 rating
        review: (string-utf8 100)
    }
)

;; Add Public Function
(define-public (rate-farmer (farmer principal) (rating uint) (review (string-utf8 100)))
    (begin
        (asserts! (and (>= rating u1) (<= rating u5)) (err u105))
        (map-set crop-ratings {farmer: farmer, reviewer: tx-sender}
            {
                rating: rating,
                review: review
            }
        )
        (ok true)
    )
)




;; Add to Data Maps
(define-map crop-seasons
    (string-utf8 50)  ;; crop type
    {
        planting-month: uint,
        harvest-month: uint
    }
)

;; Add Public Function
(define-public (add-crop-season (crop-type (string-utf8 50)) (plant-month uint) (harvest-month uint))
    (begin
        (asserts! (and (>= plant-month u1) (<= plant-month u12)) (err u106))
        (asserts! (and (>= harvest-month u1) (<= harvest-month u12)) (err u107))
        (map-set crop-seasons crop-type
            {
                planting-month: plant-month,
                harvest-month: harvest-month
            }
        )
        (ok true)
    )
)
