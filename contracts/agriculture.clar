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