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




;; Add to Data Maps
(define-map bulk-discounts
    principal
    {
        min-quantity: uint,
        discount-percentage: uint
    }
)

;; Add Public Function
(define-public (set-bulk-discount (min-qty uint) (discount uint))
    (begin
        (asserts! (<= discount u50) (err u108))  ;; Max 50% discount
        (map-set bulk-discounts tx-sender
            {
                min-quantity: min-qty,
                discount-percentage: discount
            }
        )
        (ok true)
    )
)



;; Add to Data Maps
(define-map insurance-policies
    { farmer: principal, investor: principal }
    {
        coverage-amount: uint,
        premium-paid: uint,
        active: bool
    }
)

;; Add Public Function
(define-public (purchase-insurance (farmer principal) (coverage uint))
    (let ((premium (* coverage u5)))  ;; 5% premium
        (try! (stx-transfer? premium tx-sender contract-owner))
        (map-set insurance-policies {farmer: farmer, investor: tx-sender}
            {
                coverage-amount: coverage,
                premium-paid: premium,
                active: true
            }
        )
        (ok true)
    )
)




;; Add to Data Maps
(define-map organic-certifications
    principal
    {
        certified: bool,
        certification-date: uint,
        expiry-date: uint
    }
)

;; Add Public Function
(define-public (certify-organic (farmer principal) (valid-until uint))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-not-authorized)
        (map-set organic-certifications farmer
            {
                certified: true,
                certification-date: stacks-block-height,
                expiry-date: valid-until
            }
        )
        (ok true)
    )
)



;; Add to Data Maps
(define-map escrow-holdings
    { buyer: principal, seller: principal }
    {
        amount: uint,
        released: bool
    }
)

;; Add Public Function
(define-public (create-escrow (seller principal) (amount uint))
    (begin
        (try! (stx-transfer? amount tx-sender contract-owner))
        (map-set escrow-holdings {buyer: tx-sender, seller: seller}
            {
                amount: amount,
                released: false
            }
        )
        (ok true)
    )
)
