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


;; Add to Data Maps
(define-map weather-subscriptions
    principal
    {
        region-code: (string-utf8 10),
        active: bool,
        last-alert: uint
    }
)

;; Add Public Function
(define-public (subscribe-to-weather-alerts (region-code (string-utf8 10)))
    (begin
        (map-set weather-subscriptions tx-sender
            {
                region-code: region-code,
                active: true,
                last-alert: u0
            }
        )
        (ok true)
    )
)

(define-public (send-weather-alert (region-code (string-utf8 10)) (alert-message (string-utf8 200)))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-not-authorized)
        ;; TODO: Send alert to all subscribed farmers
        (print alert-message)
        (ok true)
    )
)

;; Read-only function to check subscription
(define-read-only (get-weather-subscription (farmer principal))
    (map-get? weather-subscriptions farmer)
)




;; Add Constants
(define-constant fee-percentage u2)  ;; 2% fee

;; Add to Data Maps
(define-map marketplace-fees
    uint  ;; block height as key
    {
        total-fees: uint,
        transactions: uint
    }
)

;; Modify the invest-in-crop function to include fees
(define-public (invest-in-crop-with-fee (farmer principal) (quantity uint))
    (let (
        (listing (unwrap! (map-get? farmers farmer) (err u102)))
        (base-cost (* quantity (get price-per-unit listing)))
        (fee-amount (/ (* base-cost fee-percentage) u100))
        (total-cost (+ base-cost fee-amount))
    )
        (asserts! (<= quantity (get quantity listing)) (err u103))
        (asserts! (get available listing) (err u104))
        
        ;; Transfer payment to farmer
        (try! (stx-transfer? base-cost tx-sender farmer))
        
        ;; Transfer fee to contract owner
        (try! (stx-transfer? fee-amount tx-sender contract-owner))
        
        ;; Record the investment
        (map-set investments {investor: tx-sender, farmer: farmer}
            {
                amount: base-cost,
                quantity: quantity
            }
        )
        
        ;; Update fee records
        (match (map-get? marketplace-fees stacks-block-height)
            prev-fees (map-set marketplace-fees stacks-block-height
                {
                    total-fees: (+ fee-amount (get total-fees prev-fees)),
                    transactions: (+ u1 (get transactions prev-fees))
                })
            (map-set marketplace-fees stacks-block-height
                {
                    total-fees: fee-amount,
                    transactions: u1
                })
        )
        
        (ok true)
    )
)

;; Read-only function to get fee data
(define-read-only (get-marketplace-fees (current-block-height uint))
    (map-get? marketplace-fees stacks-block-height)
)



;; Add Constants
(define-constant err-not-verified (err u110))

;; Add to Data Maps
(define-map verifiers
    principal
    {
        name: (string-utf8 50),
        active: bool,
        verification-count: uint
    }
)

(define-map crop-verifications
    { farmer: principal, crop-type: (string-utf8 50) }
    {
        verified: bool,
        verifier: principal,
        verification-date: uint,
        quality-score: uint  ;; 1-10 score
    }
)

;; Add Public Functions
(define-public (register-verifier (verifier-name (string-utf8 50)))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-not-authorized)
        (map-set verifiers tx-sender
            {
                name: verifier-name,
                active: true,
                verification-count: u0
            }
        )
        (ok true)
    )
)

(define-public (verify-crop (farmer principal) (crop-type (string-utf8 50)) (quality-score uint))
    (let (
        (verifier-info (unwrap! (map-get? verifiers tx-sender) err-not-authorized))
        (current-count (get verification-count verifier-info))
    )
        (asserts! (get active verifier-info) err-not-authorized)
        (asserts! (and (>= quality-score u1) (<= quality-score u10)) (err u111))
        
        ;; Update verification record
        (map-set crop-verifications {farmer: farmer, crop-type: crop-type}
            {
                verified: true,
                verifier: tx-sender,
                verification-date: stacks-block-height,
                quality-score: quality-score
            }
        )
        
        ;; Update verifier stats
        (map-set verifiers tx-sender
            {
                name: (get name verifier-info),
                active: true,
                verification-count: (+ current-count u1)
            }
        )
        
        (ok true)
    )
)

;; Read-only function
(define-read-only (get-crop-verification (farmer principal) (crop-type (string-utf8 50)))
    (map-get? crop-verifications {farmer: farmer, crop-type: crop-type})
)


;; Add Constants
(define-constant status-harvested u1)
(define-constant status-processed u2)
(define-constant status-packaged u3)
(define-constant status-shipped u4)
(define-constant status-delivered u5)

;; Add to Data Maps
(define-map supply-chain-items
    { batch-id: (string-utf8 20), farmer: principal }
    {
        crop-type: (string-utf8 50),
        quantity: uint,
        current-status: uint,
        last-updated: uint
    }
)

(define-map supply-chain-history
    { batch-id: (string-utf8 20), status: uint }
    {
        timestamp: uint,
        handler: principal,
        notes: (string-ascii 100)
    }
)

;; Add Public Functions
(define-public (create-supply-chain-item (batch-id (string-utf8 20)) (crop-type (string-utf8 50)) (quantity uint))
    (begin
        (map-set supply-chain-items {batch-id: batch-id, farmer: tx-sender}
            {
                crop-type: crop-type,
                quantity: quantity,
                current-status: status-harvested,
                last-updated: stacks-block-height
            }
        )
        
        (map-set supply-chain-history {batch-id: batch-id, status: status-harvested}
            {
                timestamp: stacks-block-height,
                handler: tx-sender,
                notes: "Initial harvest recorded"
            }
        )
        
        (ok true)
    )
)

(define-public (update-supply-chain-status (batch-id (string-utf8 20)) (farmer principal) (new-status uint) (notes (string-ascii 100)))
    (let (
        (item (unwrap! (map-get? supply-chain-items {batch-id: batch-id, farmer: farmer}) (err u112)))
    )
        (asserts! (> new-status (get current-status item)) (err u113))
        (asserts! (<= new-status status-delivered) (err u114))
        
        ;; Update current status
        (map-set supply-chain-items {batch-id: batch-id, farmer: farmer}
            {
                crop-type: (get crop-type item),
                quantity: (get quantity item),
                current-status: new-status,
                last-updated: stacks-block-height
            }
        )
        
        ;; Record in history
        (map-set supply-chain-history {batch-id: batch-id, status: new-status}
            {
                timestamp: stacks-block-height,
                handler: tx-sender,
                notes: notes
            }
        )
        
        (ok true)
    )
)

;; Read-only functions
(define-read-only (get-supply-chain-item (batch-id (string-utf8 20)) (farmer principal))
    (map-get? supply-chain-items {batch-id: batch-id, farmer: farmer})
)

(define-read-only (get-supply-chain-history (batch-id (string-utf8 20)) (status uint))
    (map-get? supply-chain-history {batch-id: batch-id, status: status})
)



;; Add Constants
(define-constant err-pool-closed (err u120))
(define-constant err-min-contribution (err u121))

;; Add to Data Maps
(define-map farming-pools
    (string-utf8 30)  ;; pool-id
    {
        creator: principal,
        crop-type: (string-utf8 50),
        target-amount: uint,
        current-amount: uint,
        min-contribution: uint,
        active: bool,
        contributors: uint
    }
)

(define-map pool-contributions
    { pool-id: (string-utf8 30), contributor: principal }
    {
        amount: uint,
        timestamp: uint
    }
)

;; Add Public Functions
(define-public (create-farming-pool (pool-id (string-utf8 30)) (crop-type (string-utf8 50)) (target-amount uint) (min-contribution uint))
    (begin
        (asserts! (> target-amount u0) (err u122))
        (asserts! (> min-contribution u0) (err u123))
        
        (map-set farming-pools pool-id
            {
                creator: tx-sender,
                crop-type: crop-type,
                target-amount: target-amount,
                current-amount: u0,
                min-contribution: min-contribution,
                active: true,
                contributors: u0
            }
        )
        
        (ok true)
    )
)

(define-public (contribute-to-pool (pool-id (string-utf8 30)) (amount uint))
    (let (
        (pool (unwrap! (map-get? farming-pools pool-id) (err u124)))
        (current-total (get current-amount pool))
        (new-total (+ current-total amount))
    )
        (asserts! (get active pool) err-pool-closed)
        (asserts! (>= amount (get min-contribution pool)) err-min-contribution)
        
        ;; Transfer funds to pool creator
        (try! (stx-transfer? amount tx-sender (get creator pool)))
        
        ;; Update pool stats
        (map-set farming-pools pool-id
            {
                creator: (get creator pool),
                crop-type: (get crop-type pool),
                target-amount: (get target-amount pool),
                current-amount: new-total,
                min-contribution: (get min-contribution pool),
                active: (< new-total (get target-amount pool)),
                contributors: (+ (get contributors pool) u1)
            }
        )
        
        ;; Record contribution
        (map-set pool-contributions {pool-id: pool-id, contributor: tx-sender}
            {
                amount: amount,
                timestamp: stacks-block-height
            }
        )
        
        (ok true)
    )
)

;; Read-only functions
(define-read-only (get-farming-pool (pool-id (string-utf8 30)))
    (map-get? farming-pools pool-id)
)

(define-read-only (get-pool-contribution (pool-id (string-utf8 30)) (contributor principal))
    (map-get? pool-contributions {pool-id: pool-id, contributor: contributor})
)



;; Add Constants
(define-constant err-future-expired (err u130))
(define-constant err-future-not-mature (err u131))

;; Add to Data Maps
(define-map crop-futures
    { future-id: (string-utf8 30), farmer: principal }
    {
        crop-type: (string-utf8 50),
        quantity: uint,
        price-per-unit: uint,
        planting-block: uint,
        maturity-block: uint,
        buyer: (optional principal),
        fulfilled: bool
    }
)

;; Add Public Functions
(define-public (create-crop-future (future-id (string-utf8 30)) (crop-type (string-utf8 50)) (quantity uint) (price-per-unit uint) (maturity-blocks uint))
    (begin
        (asserts! (> quantity u0) (err u132))
        (asserts! (> price-per-unit u0) (err u133))
        (asserts! (> maturity-blocks u0) (err u134))
        
        (map-set crop-futures {future-id: future-id, farmer: tx-sender}
            {
                crop-type: crop-type,
                quantity: quantity,
                price-per-unit: price-per-unit,
                planting-block: stacks-block-height,
                maturity-block: (+ stacks-block-height maturity-blocks),
                buyer: none,
                fulfilled: false
            }
        )
        
        (ok true)
    )
)

(define-public (buy-crop-future (future-id (string-utf8 30)) (farmer principal))
    (let (
        (future (unwrap! (map-get? crop-futures {future-id: future-id, farmer: farmer}) (err u135)))
        (total-cost (* (get quantity future) (get price-per-unit future)))
    )
        (asserts! (is-none (get buyer future)) (err u136))
        (asserts! (not (get fulfilled future)) (err u137))
        (asserts! (< stacks-block-height (get maturity-block future)) err-future-expired)
        
        ;; Transfer payment to farmer
        (try! (stx-transfer? total-cost tx-sender farmer))
        
        ;; Update future contract
        (map-set crop-futures {future-id: future-id, farmer: farmer}
            {
                crop-type: (get crop-type future),
                quantity: (get quantity future),
                price-per-unit: (get price-per-unit future),
                planting-block: (get planting-block future),
                maturity-block: (get maturity-block future),
                buyer: (some tx-sender),
                fulfilled: false
            }
        )
        
        (ok true)
    )
)

(define-public (fulfill-crop-future (future-id (string-utf8 30)))
    (let (
        (future (unwrap! (map-get? crop-futures {future-id: future-id, farmer: tx-sender}) (err u135)))
    )
        (asserts! (is-some (get buyer future)) (err u138))
        (asserts! (not (get fulfilled future)) (err u139))
        (asserts! (>= stacks-block-height (get maturity-block future)) err-future-not-mature)
        
        ;; Mark as fulfilled
        (map-set crop-futures {future-id: future-id, farmer: tx-sender}
            {
                crop-type: (get crop-type future),
                quantity: (get quantity future),
                price-per-unit: (get price-per-unit future),
                planting-block: (get planting-block future),
                maturity-block: (get maturity-block future),
                buyer: (get buyer future),
                fulfilled: true
            }
        )
        
        (ok true)
    )
)

;; Read-only function
(define-read-only (get-crop-future (future-id (string-utf8 30)) (farmer principal))
    (map-get? crop-futures {future-id: future-id, farmer: farmer})
)



;; Add to Data Maps
(define-map farmer-reputation
    principal
    {
        total-sales: uint,
        completed-contracts: uint,
        failed-contracts: uint,
        average-rating: uint,  ;; Out of 100 for precision
        review-count: uint
    }
)

;; Add Public Functions
(define-public (initialize-reputation)
    (begin
        (map-set farmer-reputation tx-sender
            {
                total-sales: u0,
                completed-contracts: u0,
                failed-contracts: u0,
                average-rating: u0,
                review-count: u0
            }
        )
        (ok true)
    )
)

(define-public (update-farmer-reputation (farmer principal) (completed bool) (sale-amount uint) (rating uint))
    (let (
        (current-rep (default-to 
            {
                total-sales: u0,
                completed-contracts: u0,
                failed-contracts: u0,
                average-rating: u0,
                review-count: u0
            } 
            (map-get? farmer-reputation farmer)))
        (current-total-rating (* (get average-rating current-rep) (get review-count current-rep)))
        (new-review-count (+ (get review-count current-rep) u1))
        (new-total-rating (+ current-total-rating rating))
        (new-average-rating (/ new-total-rating new-review-count))
    )
        (asserts! (and (>= rating u0) (<= rating u100)) (err u140))
        
        (map-set farmer-reputation farmer
            {
                total-sales: (+ (get total-sales current-rep) sale-amount),
                completed-contracts: (+ (get completed-contracts current-rep) (if completed u1 u0)),
                failed-contracts: (+ (get failed-contracts current-rep) (if completed u0 u1)),
                average-rating: new-average-rating,
                review-count: new-review-count
            }
        )
        
        (ok true)
    )
)

;; Read-only function
(define-read-only (get-farmer-reputation (farmer principal))
    (map-get? farmer-reputation farmer)
)



(define-map yield-history
    { farmer: principal, crop-type: (string-utf8 50), season: uint }
    {
        planted-amount: uint,
        harvested-amount: uint,
        success-rate: uint
    }
)

(define-map yield-predictions
    { farmer: principal, crop-type: (string-utf8 50) }
    {
        predicted-yield: uint,
        confidence-score: uint,
        last-updated: uint
    }
)

(define-public (record-yield-results (crop-type (string-utf8 50)) (planted uint) (harvested uint))
    (let
        (
            (success-percentage (* (/ harvested planted) u100))
            (current-season (/ stacks-block-height u144))
        )
        (map-set yield-history 
            { farmer: tx-sender, crop-type: crop-type, season: current-season }
            {
                planted-amount: planted,
                harvested-amount: harvested,
                success-rate: success-percentage
            }
        )
        (ok true)
    )
)

(define-public (calculate-yield-prediction (crop-type (string-utf8 50)))
    (let
        (
            (current-season (/ stacks-block-height u144))
            (last-season (- current-season u1))
            (previous-yield (unwrap! (map-get? yield-history 
                { farmer: tx-sender, crop-type: crop-type, season: last-season }) 
                (err u200)))
        )
        (map-set yield-predictions
            { farmer: tx-sender, crop-type: crop-type }
            {
                predicted-yield: (get harvested-amount previous-yield),
                confidence-score: (get success-rate previous-yield),
                last-updated: stacks-block-height
            }
        )
        (ok true)
    )
)


(define-map market-metrics
    (string-utf8 50)
    {
        total-supply: uint,
        total-demand: uint,
        base-price: uint,
        last-updated: uint
    }
)

(define-map price-adjustments
    { crop-type: (string-utf8 50), timestamp: uint }
    {
        old-price: uint,
        new-price: uint,
        change-percentage: uint
    }
)

(define-public (update-market-metrics (crop-type (string-utf8 50)) (supply uint) (demand uint))
    (let
        (
            (current-metrics (default-to
                { total-supply: u0, total-demand: u0, base-price: u0, last-updated: u0 }
                (map-get? market-metrics crop-type)))
        )
        (map-set market-metrics crop-type
            {
                total-supply: supply,
                total-demand: demand,
                base-price: (get base-price current-metrics),
                last-updated: stacks-block-height
            }
        )
        (ok true)
    )
)

(define-public (calculate-dynamic-price (crop-type (string-utf8 50)))
    (let
        (
            (metrics (unwrap! (map-get? market-metrics crop-type) (err u300)))
            (supply (get total-supply metrics))
            (demand (get total-demand metrics))
            (base-price (get base-price metrics))
            (new-price (if (> demand supply)
                (* base-price u12 u10)
                (* base-price u8 u10)))
        )
        (map-set price-adjustments 
            { crop-type: crop-type, timestamp: stacks-block-height }
            {
                old-price: base-price,
                new-price: new-price,
                change-percentage: (/ (* (- new-price base-price) u100) base-price)
            }
        )
        (ok true)
    )
)



(define-map quality-checkpoints
    { batch-id: (string-utf8 30), stage: (string-utf8 20) }
    {
        inspector: principal,
        timestamp: uint,
        score: uint,
        moisture-level: uint,
        temperature: uint,
        certification-hash: (buff 32)
    }
)

(define-map quality-thresholds
    (string-utf8 50)
    {
        min-score: uint,
        max-moisture: uint,
        min-temperature: uint,
        max-temperature: uint
    }
)

(define-public (record-quality-check 
    (batch-id (string-utf8 30)) 
    (stage (string-utf8 20)) 
    (score uint) 
    (moisture uint) 
    (temp uint) 
    (cert-hash (buff 32)))
    (begin
        (asserts! (and (>= score u0) (<= score u100)) (err u501))
        (map-set quality-checkpoints
            { batch-id: batch-id, stage: stage }
            {
                inspector: tx-sender,
                timestamp: stacks-block-height,
                score: score,
                moisture-level: moisture,
                temperature: temp,
                certification-hash: cert-hash
            }
        )
        (ok true)
    )
)

(define-read-only (verify-quality-compliance (batch-id (string-utf8 30)) (stage (string-utf8 20)))
    (match (map-get? quality-checkpoints { batch-id: batch-id, stage: stage })
        checkpoint (ok checkpoint)
        (err u502)
    )
)

(define-map weather-risk-parameters
    (string-utf8 50)  ;; crop-type
    {
        max-temperature: uint,
        min-temperature: uint,
        max-rainfall: uint,
        min-rainfall: uint,
        coverage-multiplier: uint
    }
)

(define-map insurance-policies-v2
    { policy-id: (string-utf8 30), farmer: principal }
    {
        crop-type: (string-utf8 50),
        coverage-amount: uint,
        start-block: uint,
        end-block: uint,
        premium-paid: uint,
        claim-paid: bool,
        risk-score: uint
    }
)

(define-map weather-events
    uint  ;; block height
    {
        temperature: uint,
        rainfall: uint,
        wind-speed: uint,
        reported-by: principal
    }
)

(define-public (create-parametric-insurance 
    (policy-id (string-utf8 30)) 
    (crop-type (string-utf8 50)) 
    (coverage uint) 
    (duration uint))
    (let
        (
            (premium-amount (/ (* coverage u5) u100))
            (risk-params (unwrap! (map-get? weather-risk-parameters crop-type) (err u601)))
        )
        (try! (stx-transfer? premium-amount tx-sender contract-owner))
        (map-set insurance-policies-v2
            { policy-id: policy-id, farmer: tx-sender }
            {
                crop-type: crop-type,
                coverage-amount: coverage,
                start-block: stacks-block-height,
                end-block: (+ stacks-block-height duration),
                premium-paid: premium-amount,
                claim-paid: false,
                risk-score: u0
            }
        )
        (ok true)
    )
)

(define-public (process-weather-claim (policy-id (string-utf8 30)))
    (let
        (
            (policy (unwrap! (map-get? insurance-policies-v2 
                { policy-id: policy-id, farmer: tx-sender }) 
                (err u602)))
            (weather-data (unwrap! (map-get? weather-events stacks-block-height) 
                (err u603)))
            (risk-params (unwrap! (map-get? weather-risk-parameters 
                (get crop-type policy)) 
                (err u604)))
        )
        (asserts! (not (get claim-paid policy)) (err u605))
        (if (or
            (> (get temperature weather-data) (get max-temperature risk-params))
            (< (get temperature weather-data) (get min-temperature risk-params)))
            (begin
                (try! (stx-transfer? (get coverage-amount policy) 
                    contract-owner 
                    tx-sender))
                (map-set insurance-policies-v2
                    { policy-id: policy-id, farmer: tx-sender }
                    (merge policy { claim-paid: true }))
                (ok true))
            (ok false)
        )
    )
)

(define-constant err-insufficient-credits (err u700))
(define-constant err-invalid-practice (err u701))
(define-constant err-credit-not-verified (err u702))
(define-constant err-invalid-credit-amount (err u703))

(define-map carbon-credits
    principal
    {
        total-credits: uint,
        available-credits: uint,
        credits-sold: uint,
        last-updated: uint
    }
)

(define-map sustainable-practices
    { farmer: principal, practice-id: (string-utf8 30) }
    {
        practice-type: (string-utf8 50),
        implementation-date: uint,
        area-covered: uint,
        credits-per-period: uint,
        verified: bool,
        verifier: (optional principal)
    }
)

(define-map carbon-credit-trades
    { trade-id: (string-utf8 30), seller: principal }
    {
        buyer: principal,
        credits-amount: uint,
        price-per-credit: uint,
        total-price: uint,
        trade-date: uint,
        completed: bool
    }
)

(define-map carbon-verifiers
    principal
    {
        name: (string-utf8 50),
        active: bool,
        verifications-completed: uint
    }
)

(define-map practice-credit-rates
    (string-utf8 50)
    {
        credits-per-hectare: uint,
        minimum-area: uint,
        verification-required: bool
    }
)

(define-public (register-carbon-verifier (verifier-name (string-utf8 50)))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-not-authorized)
        (map-set carbon-verifiers tx-sender
            {
                name: verifier-name,
                active: true,
                verifications-completed: u0
            }
        )
        (ok true)
    )
)

(define-public (set-practice-credit-rate (practice-type (string-utf8 50)) (credits-per-hectare uint) (min-area uint) (requires-verification bool))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-not-authorized)
        (map-set practice-credit-rates practice-type
            {
                credits-per-hectare: credits-per-hectare,
                minimum-area: min-area,
                verification-required: requires-verification
            }
        )
        (ok true)
    )
)

(define-public (register-sustainable-practice (practice-id (string-utf8 30)) (practice-type (string-utf8 50)) (area-covered uint))
    (let (
        (rate-info (unwrap! (map-get? practice-credit-rates practice-type) err-invalid-practice))
        (credits-earned (* area-covered (get credits-per-hectare rate-info)))
    )
        (asserts! (>= area-covered (get minimum-area rate-info)) (err u704))
        
        (map-set sustainable-practices 
            { farmer: tx-sender, practice-id: practice-id }
            {
                practice-type: practice-type,
                implementation-date: stacks-block-height,
                area-covered: area-covered,
                credits-per-period: credits-earned,
                verified: (not (get verification-required rate-info)),
                verifier: none
            }
        )
        
        (if (not (get verification-required rate-info))
            (match (map-get? carbon-credits tx-sender)
                existing-credits (map-set carbon-credits tx-sender
                    {
                        total-credits: (+ (get total-credits existing-credits) credits-earned),
                        available-credits: (+ (get available-credits existing-credits) credits-earned),
                        credits-sold: (get credits-sold existing-credits),
                        last-updated: stacks-block-height
                    })
                (map-set carbon-credits tx-sender
                    {
                        total-credits: credits-earned,
                        available-credits: credits-earned,
                        credits-sold: u0,
                        last-updated: stacks-block-height
                    })
            )
            true
        )
        
        (ok true)
    )
)

(define-public (verify-sustainable-practice (farmer principal) (practice-id (string-utf8 30)))
    (let (
        (verifier-info (unwrap! (map-get? carbon-verifiers tx-sender) err-not-authorized))
        (practice (unwrap! (map-get? sustainable-practices { farmer: farmer, practice-id: practice-id }) (err u705)))
        (credits-to-add (get credits-per-period practice))
    )
        (asserts! (get active verifier-info) err-not-authorized)
        (asserts! (not (get verified practice)) (err u706))
        
        (map-set sustainable-practices 
            { farmer: farmer, practice-id: practice-id }
            {
                practice-type: (get practice-type practice),
                implementation-date: (get implementation-date practice),
                area-covered: (get area-covered practice),
                credits-per-period: (get credits-per-period practice),
                verified: true,
                verifier: (some tx-sender)
            }
        )
        
        (match (map-get? carbon-credits farmer)
            existing-credits (map-set carbon-credits farmer
                {
                    total-credits: (+ (get total-credits existing-credits) credits-to-add),
                    available-credits: (+ (get available-credits existing-credits) credits-to-add),
                    credits-sold: (get credits-sold existing-credits),
                    last-updated: stacks-block-height
                })
            (map-set carbon-credits farmer
                {
                    total-credits: credits-to-add,
                    available-credits: credits-to-add,
                    credits-sold: u0,
                    last-updated: stacks-block-height
                })
        )
        
        (map-set carbon-verifiers tx-sender
            {
                name: (get name verifier-info),
                active: true,
                verifications-completed: (+ (get verifications-completed verifier-info) u1)
            }
        )
        
        (ok true)
    )
)

(define-public (create-carbon-credit-listing (trade-id (string-utf8 30)) (credits-amount uint) (price-per-credit uint))
    (let (
        (farmer-credits (unwrap! (map-get? carbon-credits tx-sender) err-insufficient-credits))
    )
        (asserts! (> credits-amount u0) err-invalid-credit-amount)
        (asserts! (> price-per-credit u0) err-invalid-credit-amount)
        (asserts! (<= credits-amount (get available-credits farmer-credits)) err-insufficient-credits)
        
        (map-set carbon-credit-trades 
            { trade-id: trade-id, seller: tx-sender }
            {
                buyer: tx-sender,
                credits-amount: credits-amount,
                price-per-credit: price-per-credit,
                total-price: (* credits-amount price-per-credit),
                trade-date: stacks-block-height,
                completed: false
            }
        )
        
        (map-set carbon-credits tx-sender
            {
                total-credits: (get total-credits farmer-credits),
                available-credits: (- (get available-credits farmer-credits) credits-amount),
                credits-sold: (get credits-sold farmer-credits),
                last-updated: stacks-block-height
            }
        )
        
        (ok true)
    )
)

(define-public (purchase-carbon-credits (trade-id (string-utf8 30)) (seller principal))
    (let (
        (trade (unwrap! (map-get? carbon-credit-trades { trade-id: trade-id, seller: seller }) (err u707)))
        (seller-credits (unwrap! (map-get? carbon-credits seller) err-insufficient-credits))
        (total-cost (get total-price trade))
    )
        (asserts! (not (get completed trade)) (err u708))
        (asserts! (not (is-eq tx-sender seller)) (err u709))
        
        (try! (stx-transfer? total-cost tx-sender seller))
        
        (map-set carbon-credit-trades 
            { trade-id: trade-id, seller: seller }
            {
                buyer: tx-sender,
                credits-amount: (get credits-amount trade),
                price-per-credit: (get price-per-credit trade),
                total-price: total-cost,
                trade-date: stacks-block-height,
                completed: true
            }
        )
        
        (map-set carbon-credits seller
            {
                total-credits: (get total-credits seller-credits),
                available-credits: (get available-credits seller-credits),
                credits-sold: (+ (get credits-sold seller-credits) (get credits-amount trade)),
                last-updated: stacks-block-height
            }
        )
        
        (ok true)
    )
)

(define-public (generate-periodic-credits)
    (let (
        (farmer-credits (default-to 
            { total-credits: u0, available-credits: u0, credits-sold: u0, last-updated: u0 }
            (map-get? carbon-credits tx-sender)))
        (blocks-since-update (- stacks-block-height (get last-updated farmer-credits)))
        (periods-elapsed (/ blocks-since-update u144))
    )
        (asserts! (> periods-elapsed u0) (err u710))
        
        (map-set carbon-credits tx-sender
            {
                total-credits: (get total-credits farmer-credits),
                available-credits: (get available-credits farmer-credits),
                credits-sold: (get credits-sold farmer-credits),
                last-updated: stacks-block-height
            }
        )
        
        (ok periods-elapsed)
    )
)

(define-read-only (get-carbon-credits (farmer principal))
    (map-get? carbon-credits farmer)
)

(define-read-only (get-sustainable-practice (farmer principal) (practice-id (string-utf8 30)))
    (map-get? sustainable-practices { farmer: farmer, practice-id: practice-id })
)

(define-read-only (get-carbon-trade (trade-id (string-utf8 30)) (seller principal))
    (map-get? carbon-credit-trades { trade-id: trade-id, seller: seller })
)

(define-read-only (get-practice-rates (practice-type (string-utf8 50)))
    (map-get? practice-credit-rates practice-type)
)

(define-read-only (get-carbon-verifier (verifier principal))
    (map-get? carbon-verifiers verifier)
)


(define-constant err-equipment-unavailable (err u800))
(define-constant err-invalid-rental-period (err u801))
(define-constant err-equipment-not-owned (err u802))
(define-constant err-rental-already-active (err u803))
(define-constant err-rental-not-found (err u804))
(define-constant err-invalid-equipment-type (err u805))
(define-constant err-insufficient-deposit (err u806))
(define-constant err-equipment-not-returned (err u807))
(define-constant err-invalid-location (err u808))
(define-constant err-rental-expired (err u809))

(define-map equipment-registry
    { equipment-id: (string-utf8 30), owner: principal }
    {
        equipment-type: (string-utf8 50),
        equipment-name: (string-utf8 100),
        hourly-rate: uint,
        daily-rate: uint,
        location-code: (string-utf8 20),
        available: bool,
        condition-score: uint,
        last-maintenance: uint,
        usage-hours: uint,
        deposit-required: uint
    }
)

(define-map equipment-categories
    (string-utf8 50)
    {
        min-deposit: uint,
        max-rental-days: uint,
        insurance-rate: uint,
        maintenance-interval: uint
    }
)

(define-map equipment-rentals
    { rental-id: (string-utf8 30), equipment-id: (string-utf8 30) }
    {
        renter: principal,
        owner: principal,
        start-block: uint,
        end-block: uint,
        total-cost: uint,
        deposit-paid: uint,
        rental-active: bool,
        equipment-returned: bool,
        damage-reported: bool,
        final-payment-made: bool
    }
)

(define-map farmer-sharing-profiles
    principal
    {
        location-code: (string-utf8 20),
        equipment-owned: uint,
        successful-rentals: uint,
        failed-rentals: uint,
        average-condition-score: uint,
        trust-score: uint,
        last-activity: uint
    }
)

(define-map equipment-availability-schedule
    { equipment-id: (string-utf8 30), date-block: uint }
    {
        available-hours: uint,
        booked-hours: uint,
        maintenance-scheduled: bool
    }
)

(define-map rental-reviews
    { rental-id: (string-utf8 30), reviewer: principal }
    {
        rating: uint,
        equipment-condition: uint,
        owner-reliability: uint,
        review-text: (string-utf8 200),
        verified: bool
    }
)

(define-map location-networks
    (string-utf8 20)
    {
        active-farmers: uint,
        total-equipment: uint,
        network-score: uint,
        coordinator: (optional principal)
    }
)

(define-public (register-equipment-category (category (string-utf8 50)) (min-deposit uint) (max-days uint) (insurance-rate uint) (maintenance-interval uint))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-not-authorized)
        (map-set equipment-categories category
            {
                min-deposit: min-deposit,
                max-rental-days: max-days,
                insurance-rate: insurance-rate,
                maintenance-interval: maintenance-interval
            }
        )
        (ok true)
    )
)

(define-public (register-equipment (equipment-id (string-utf8 30)) (equipment-type (string-utf8 50)) (equipment-name (string-utf8 100)) (hourly-rate uint) (daily-rate uint) (location-code (string-utf8 20)))
    (let (
        (category-info (unwrap! (map-get? equipment-categories equipment-type) err-invalid-equipment-type))
        (required-deposit (get min-deposit category-info))
    )
        (asserts! (> hourly-rate u0) err-invalid-amount)
        (asserts! (> daily-rate u0) err-invalid-amount)
        
        (map-set equipment-registry { equipment-id: equipment-id, owner: tx-sender }
            {
                equipment-type: equipment-type,
                equipment-name: equipment-name,
                hourly-rate: hourly-rate,
                daily-rate: daily-rate,
                location-code: location-code,
                available: true,
                condition-score: u100,
                last-maintenance: stacks-block-height,
                usage-hours: u0,
                deposit-required: required-deposit
            }
        )
        
        (match (map-get? farmer-sharing-profiles tx-sender)
            existing-profile (map-set farmer-sharing-profiles tx-sender
                {
                    location-code: location-code,
                    equipment-owned: (+ (get equipment-owned existing-profile) u1),
                    successful-rentals: (get successful-rentals existing-profile),
                    failed-rentals: (get failed-rentals existing-profile),
                    average-condition-score: (get average-condition-score existing-profile),
                    trust-score: (get trust-score existing-profile),
                    last-activity: stacks-block-height
                })
            (map-set farmer-sharing-profiles tx-sender
                {
                    location-code: location-code,
                    equipment-owned: u1,
                    successful-rentals: u0,
                    failed-rentals: u0,
                    average-condition-score: u100,
                    trust-score: u100,
                    last-activity: stacks-block-height
                })
        )
        
        (match (map-get? location-networks location-code)
            existing-network (map-set location-networks location-code
                {
                    active-farmers: (get active-farmers existing-network),
                    total-equipment: (+ (get total-equipment existing-network) u1),
                    network-score: (get network-score existing-network),
                    coordinator: (get coordinator existing-network)
                })
            (map-set location-networks location-code
                {
                    active-farmers: u1,
                    total-equipment: u1,
                    network-score: u50,
                    coordinator: (some tx-sender)
                })
        )
        
        (ok true)
    )
)

(define-public (create-equipment-rental (rental-id (string-utf8 30)) (equipment-id (string-utf8 30)) (owner principal) (rental-days uint))
    (let (
        (equipment (unwrap! (map-get? equipment-registry { equipment-id: equipment-id, owner: owner }) err-equipment-not-owned))
        (category-info (unwrap! (map-get? equipment-categories (get equipment-type equipment)) err-invalid-equipment-type))
        (daily-cost (get daily-rate equipment))
        (total-cost (* daily-cost rental-days))
        (deposit-amount (get deposit-required equipment))
        (total-payment (+ total-cost deposit-amount))
        (rental-end-block (+ stacks-block-height (* rental-days u144)))
    )
        (asserts! (get available equipment) err-equipment-unavailable)
        (asserts! (> rental-days u0) err-invalid-rental-period)
        (asserts! (<= rental-days (get max-rental-days category-info)) err-invalid-rental-period)
        (asserts! (not (is-eq tx-sender owner)) (err u810))
        
        (try! (stx-transfer? total-payment tx-sender owner))
        
        (map-set equipment-rentals { rental-id: rental-id, equipment-id: equipment-id }
            {
                renter: tx-sender,
                owner: owner,
                start-block: stacks-block-height,
                end-block: rental-end-block,
                total-cost: total-cost,
                deposit-paid: deposit-amount,
                rental-active: true,
                equipment-returned: false,
                damage-reported: false,
                final-payment-made: false
            }
        )
        
        (map-set equipment-registry { equipment-id: equipment-id, owner: owner }
            (merge equipment { available: false })
        )
        
        (ok true)
    )
)

(define-public (return-equipment (rental-id (string-utf8 30)) (equipment-id (string-utf8 30)) (condition-score uint) (actual-hours uint))
    (let (
        (rental (unwrap! (map-get? equipment-rentals { rental-id: rental-id, equipment-id: equipment-id }) err-rental-not-found))
        (equipment (unwrap! (map-get? equipment-registry { equipment-id: equipment-id, owner: (get owner rental) }) err-equipment-not-owned))
        (owner (get owner rental))
        (deposit-amount (get deposit-paid rental))
        (damage-penalty (if (< condition-score u80) (/ deposit-amount u2) u0))
        (deposit-return (- deposit-amount damage-penalty))
    )
        (asserts! (is-eq tx-sender (get renter rental)) err-not-authorized)
        (asserts! (get rental-active rental) err-rental-not-found)
        (asserts! (not (get equipment-returned rental)) err-equipment-not-returned)
        (asserts! (and (>= condition-score u1) (<= condition-score u100)) (err u811))
        
        (if (> deposit-return u0)
            (try! (stx-transfer? deposit-return owner tx-sender))
            true
        )
        
        (map-set equipment-rentals { rental-id: rental-id, equipment-id: equipment-id }
            (merge rental { 
                equipment-returned: true,
                rental-active: false,
                final-payment-made: true
            })
        )
        
        (map-set equipment-registry { equipment-id: equipment-id, owner: owner }
            {
                equipment-type: (get equipment-type equipment),
                equipment-name: (get equipment-name equipment),
                hourly-rate: (get hourly-rate equipment),
                daily-rate: (get daily-rate equipment),
                location-code: (get location-code equipment),
                available: true,
                condition-score: condition-score,
                last-maintenance: (get last-maintenance equipment),
                usage-hours: (+ (get usage-hours equipment) actual-hours),
                deposit-required: (get deposit-required equipment)
            }
        )
        
        (match (map-get? farmer-sharing-profiles owner)
            owner-profile (map-set farmer-sharing-profiles owner
                {
                    location-code: (get location-code owner-profile),
                    equipment-owned: (get equipment-owned owner-profile),
                    successful-rentals: (+ (get successful-rentals owner-profile) u1),
                    failed-rentals: (get failed-rentals owner-profile),
                    average-condition-score: (/ (+ (* (get average-condition-score owner-profile) (get successful-rentals owner-profile)) condition-score) (+ (get successful-rentals owner-profile) u1)),
                    trust-score: (if (> (+ (get trust-score owner-profile) u2) u100) u100 (+ (get trust-score owner-profile) u2)),
                    last-activity: stacks-block-height
                })
            true
        )
        
        (ok true)
    )
)

(define-public (rate-equipment-rental (rental-id (string-utf8 30)) (equipment-id (string-utf8 30)) (overall-rating uint) (condition-rating uint) (reliability-rating uint) (review-text (string-utf8 200)))
    (let (
        (rental (unwrap! (map-get? equipment-rentals { rental-id: rental-id, equipment-id: equipment-id }) err-rental-not-found))
    )
        (asserts! (is-eq tx-sender (get renter rental)) err-not-authorized)
        (asserts! (get equipment-returned rental) err-equipment-not-returned)
        (asserts! (and (>= overall-rating u1) (<= overall-rating u5)) (err u812))
        (asserts! (and (>= condition-rating u1) (<= condition-rating u5)) (err u813))
        (asserts! (and (>= reliability-rating u1) (<= reliability-rating u5)) (err u814))
        
        (map-set rental-reviews { rental-id: rental-id, reviewer: tx-sender }
            {
                rating: overall-rating,
                equipment-condition: condition-rating,
                owner-reliability: reliability-rating,
                review-text: review-text,
                verified: true
            }
        )
        
        (ok true)
    )
)

(define-public (schedule-equipment-maintenance (equipment-id (string-utf8 30)) (maintenance-blocks uint))
    (let (
        (equipment (unwrap! (map-get? equipment-registry { equipment-id: equipment-id, owner: tx-sender }) err-equipment-not-owned))
        (maintenance-date (+ stacks-block-height maintenance-blocks))
    )
        (asserts! (get available equipment) err-equipment-unavailable)
        (asserts! (> maintenance-blocks u0) err-invalid-rental-period)
        
        (map-set equipment-availability-schedule { equipment-id: equipment-id, date-block: maintenance-date }
            {
                available-hours: u0,
                booked-hours: u0,
                maintenance-scheduled: true
            }
        )
        
        (map-set equipment-registry { equipment-id: equipment-id, owner: tx-sender }
            (merge equipment { last-maintenance: maintenance-date })
        )
        
        (ok true)
    )
)

(define-read-only (get-equipment-details (equipment-id (string-utf8 30)) (owner principal))
    (map-get? equipment-registry { equipment-id: equipment-id, owner: owner })
)

(define-read-only (get-farmer-sharing-profile (farmer principal))
    (map-get? farmer-sharing-profiles farmer)
)

(define-read-only (get-equipment-rental (rental-id (string-utf8 30)) (equipment-id (string-utf8 30)))
    (map-get? equipment-rentals { rental-id: rental-id, equipment-id: equipment-id })
)

(define-read-only (get-location-network (location-code (string-utf8 20)))
    (map-get? location-networks location-code)
)

(define-read-only (get-rental-review (rental-id (string-utf8 30)) (reviewer principal))
    (map-get? rental-reviews { rental-id: rental-id, reviewer: reviewer })
)

(define-read-only (get-equipment-category (category (string-utf8 50)))
    (map-get? equipment-categories category)
)

(define-read-only (get-equipment-schedule (equipment-id (string-utf8 30)) (date-block uint))
    (map-get? equipment-availability-schedule { equipment-id: equipment-id, date-block: date-block })
)