;; Weather Insurance Claims System
;; Provides parametric insurance coverage for farmers based on weather conditions

;; Constants
(define-constant ERR-NOT-AUTHORIZED (err u300))
(define-constant ERR-POLICY-NOT-FOUND (err u301))
(define-constant ERR-INVALID-POLICY (err u302))
(define-constant ERR-POLICY-EXPIRED (err u303))
(define-constant ERR-CLAIM-EXISTS (err u304))
(define-constant ERR-INVALID-WEATHER-DATA (err u305))
(define-constant ERR-CLAIM-NOT-FOUND (err u306))
(define-constant ERR-INSUFFICIENT-FUNDS (err u307))
(define-constant ERR-ORACLE-NOT-AUTHORIZED (err u308))

;; Weather condition constants
(define-constant CONDITION-DROUGHT u1)
(define-constant CONDITION-FLOOD u2)
(define-constant CONDITION-FROST u3)
(define-constant CONDITION-HAIL u4)
(define-constant CONDITION-WINDSTORM u5)

;; Claim status constants
(define-constant CLAIM-STATUS-PENDING u1)
(define-constant CLAIM-STATUS-VERIFIED u2)
(define-constant CLAIM-STATUS-PAID u3)
(define-constant CLAIM-STATUS-DENIED u4)

;; Data variables
(define-data-var policy-count uint u0)
(define-data-var claim-count uint u0)
(define-data-var contract-owner principal tx-sender)
(define-data-var base-premium-rate uint u50) ;; 5% of coverage

;; Maps
(define-map weather-insurance-policies
  { policy-id: uint }
  {
    farmer: principal,
    crop-type: (string-utf8 50),
    coverage-amount: uint,
    premium-paid: uint,
    coverage-start: uint,
    coverage-end: uint,
    location-hash: (buff 32),
    weather-conditions: (list 5 uint),
    active: bool
  }
)

(define-map weather-claims
  { claim-id: uint }
  {
    policy-id: uint,
    farmer: principal,
    weather-condition: uint,
    impact-percentage: uint,
    weather-data-hash: (buff 32),
    claim-amount: uint,
    submission-block: uint,
    verification-block: uint,
    status: uint
  }
)

(define-map weather-oracle-data
  { location-hash: (buff 32), date-block: uint }
  {
    temperature-min: int,
    temperature-max: int,
    precipitation: uint,
    wind-speed: uint,
    condition-type: uint,
    verified: bool
  }
)

(define-map authorized-oracles
  { oracle: principal }
  { active: bool }
)

;; Public functions

;; Purchase weather insurance policy
(define-public (purchase-weather-policy 
    (crop-type (string-utf8 50))
    (coverage-amount uint)
    (coverage-duration uint)
    (location-hash (buff 32))
    (weather-conditions (list 5 uint)))
  (let
    ((policy-id (+ (var-get policy-count) u1))
     (premium (/ (* coverage-amount (var-get base-premium-rate)) u1000))
     (coverage-end (+ stacks-block-height coverage-duration)))
    
    (asserts! (> coverage-amount u0) ERR-INVALID-POLICY)
    (asserts! (> coverage-duration u144) ERR-INVALID-POLICY) ;; Min 1 day
    (asserts! (> (len location-hash) u0) ERR-INVALID-POLICY)
    
    ;; Transfer premium to contract
    (try! (stx-transfer? premium tx-sender (as-contract tx-sender)))
    
    (map-set weather-insurance-policies
      { policy-id: policy-id }
      {
        farmer: tx-sender,
        crop-type: crop-type,
        coverage-amount: coverage-amount,
        premium-paid: premium,
        coverage-start: stacks-block-height,
        coverage-end: coverage-end,
        location-hash: location-hash,
        weather-conditions: weather-conditions,
        active: true
      }
    )
    
    (var-set policy-count policy-id)
    (ok policy-id)
  )
)

;; Submit weather insurance claim
(define-public (submit-weather-claim 
    (policy-id uint)
    (weather-condition uint)
    (impact-percentage uint)
    (weather-data-hash (buff 32)))
  (let
    ((policy-data (unwrap! (map-get? weather-insurance-policies { policy-id: policy-id }) ERR-POLICY-NOT-FOUND))
     (claim-id (+ (var-get claim-count) u1))
     (claim-amount (/ (* (get coverage-amount policy-data) impact-percentage) u100)))
    
    (asserts! (is-eq tx-sender (get farmer policy-data)) ERR-NOT-AUTHORIZED)
    (asserts! (get active policy-data) ERR-POLICY-EXPIRED)
    (asserts! (<= stacks-block-height (get coverage-end policy-data)) ERR-POLICY-EXPIRED)
    (asserts! (>= stacks-block-height (get coverage-start policy-data)) ERR-INVALID-POLICY)
    (asserts! (and (>= impact-percentage u10) (<= impact-percentage u100)) ERR-INVALID-WEATHER-DATA)
    (asserts! (is-some (index-of (get weather-conditions policy-data) weather-condition)) ERR-INVALID-WEATHER-DATA)
    
    (map-set weather-claims
      { claim-id: claim-id }
      {
        policy-id: policy-id,
        farmer: tx-sender,
        weather-condition: weather-condition,
        impact-percentage: impact-percentage,
        weather-data-hash: weather-data-hash,
        claim-amount: claim-amount,
        submission-block: stacks-block-height,
        verification-block: u0,
        status: CLAIM-STATUS-PENDING
      }
    )
    
    (var-set claim-count claim-id)
    (ok claim-id)
  )
)

;; Oracle submits weather data
(define-public (submit-weather-data 
    (location-hash (buff 32))
    (date-block uint)
    (temp-min int)
    (temp-max int)
    (precipitation uint)
    (wind-speed uint)
    (condition-type uint))
  (begin
    (asserts! (is-authorized-oracle tx-sender) ERR-ORACLE-NOT-AUTHORIZED)
    (asserts! (and (>= condition-type u0) (<= condition-type u5)) ERR-INVALID-WEATHER-DATA)
    
    (map-set weather-oracle-data
      { location-hash: location-hash, date-block: date-block }
      {
        temperature-min: temp-min,
        temperature-max: temp-max,
        precipitation: precipitation,
        wind-speed: wind-speed,
        condition-type: condition-type,
        verified: true
      }
    )
    
    (ok true)
  )
)

;; Verify and process claim
(define-public (verify-weather-claim (claim-id uint))
  (let
    ((claim-data (unwrap! (map-get? weather-claims { claim-id: claim-id }) ERR-CLAIM-NOT-FOUND))
     (policy-data (unwrap! (map-get? weather-insurance-policies { policy-id: (get policy-id claim-data) }) ERR-POLICY-NOT-FOUND))
     (weather-data (map-get? weather-oracle-data { location-hash: (get location-hash policy-data), date-block: (get submission-block claim-data) })))
    
    (asserts! (is-authorized-oracle tx-sender) ERR-ORACLE-NOT-AUTHORIZED)
    (asserts! (is-eq (get status claim-data) CLAIM-STATUS-PENDING) ERR-INVALID-POLICY)
    
    (match weather-data
      verified-data
        (let
          ((condition-matches (is-eq (get condition-type verified-data) (get weather-condition claim-data)))
           (payout-amount (if condition-matches (get claim-amount claim-data) u0)))
          
          (map-set weather-claims
            { claim-id: claim-id }
            (merge claim-data {
              verification-block: stacks-block-height,
              status: (if condition-matches CLAIM-STATUS-VERIFIED CLAIM-STATUS-DENIED)
            })
          )
          
          (if condition-matches
            (try! (as-contract (stx-transfer? payout-amount tx-sender (get farmer claim-data))))
            true
          )
          
          (ok condition-matches)
        )
      ;; No weather data available
      (begin
        (map-set weather-claims
          { claim-id: claim-id }
          (merge claim-data { status: CLAIM-STATUS-DENIED })
        )
        (ok false)
      )
    )
  )
)

;; Add authorized weather oracle
(define-public (add-weather-oracle (oracle principal))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
    (map-set authorized-oracles
      { oracle: oracle }
      { active: true }
    )
    (ok true)
  )
)

;; Read-only functions
(define-read-only (get-weather-policy (policy-id uint))
  (map-get? weather-insurance-policies { policy-id: policy-id })
)

(define-read-only (get-weather-claim (claim-id uint))
  (map-get? weather-claims { claim-id: claim-id })
)

(define-read-only (get-weather-data (location-hash (buff 32)) (date-block uint))
  (map-get? weather-oracle-data { location-hash: location-hash, date-block: date-block })
)

(define-read-only (is-authorized-oracle (oracle principal))
  (default-to false (get active (map-get? authorized-oracles { oracle: oracle })))
)

(define-read-only (get-policy-count)
  (var-get policy-count)
)

(define-read-only (get-claim-count)
  (var-get claim-count)
)

(define-read-only (calculate-premium (coverage-amount uint))
  (/ (* coverage-amount (var-get base-premium-rate)) u1000)
)
