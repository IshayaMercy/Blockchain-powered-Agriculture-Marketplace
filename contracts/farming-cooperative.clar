;; Farming Cooperative Management Contract
;; Enables farmers to form cooperatives, pool resources, and make collective decisions

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-not-authorized (err u200))
(define-constant err-invalid-amount (err u201))
(define-constant err-coop-not-found (err u202))
(define-constant err-already-member (err u203))
(define-constant err-not-member (err u204))
(define-constant err-insufficient-votes (err u205))
(define-constant err-proposal-not-found (err u206))
(define-constant err-voting-ended (err u207))
(define-constant err-proposal-not-passed (err u208))
(define-constant err-insufficient-funds (err u209))
(define-constant err-invalid-percentage (err u210))
(define-constant err-coop-exists (err u211))
(define-constant err-min-members-required (err u212))
(define-constant err-invalid-voting-period (err u213))

;; Data Maps
(define-map cooperatives
    (string-utf8 50)  ;; cooperative-name
    {
        founder: principal,
        description: (string-utf8 200),
        member-count: uint,
        total-funds: uint,
        created-at: uint,
        active: bool,
        min-member-contribution: uint,
        voting-threshold: uint  ;; percentage required to pass proposals
    }
)

(define-map cooperative-members
    { coop-name: (string-utf8 50), member: principal }
    {
        contribution-amount: uint,
        join-date: uint,
        voting-power: uint,
        reputation-score: uint,
        active: bool
    }
)

(define-map cooperative-proposals
    { coop-name: (string-utf8 50), proposal-id: (string-utf8 30) }
    {
        proposer: principal,
        proposal-type: (string-utf8 20),  ;; purchase, infrastructure, policy
        description: (string-utf8 300),
        amount-requested: uint,
        voting-deadline: uint,
        votes-for: uint,
        votes-against: uint,
        executed: bool,
        passed: bool
    }
)

(define-map proposal-votes
    { coop-name: (string-utf8 50), proposal-id: (string-utf8 30), voter: principal }
    {
        vote: bool,  ;; true for yes, false for no
        voting-power-used: uint,
        timestamp: uint
    }
)

(define-map resource-pools
    { coop-name: (string-utf8 50), resource-type: (string-utf8 30) }
    {
        total-quantity: uint,
        available-quantity: uint,
        cost-per-unit: uint,
        coordinator: principal,
        last-updated: uint
    }
)

(define-map member-resource-contributions
    { coop-name: (string-utf8 50), member: principal, resource-type: (string-utf8 30) }
    {
        quantity-contributed: uint,
        contribution-value: uint,
        last-contribution: uint
    }
)

(define-map cooperative-purchases
    { coop-name: (string-utf8 50), purchase-id: (string-utf8 30) }
    {
        item-description: (string-utf8 100),
        total-cost: uint,
        coordinator: principal,
        participants: uint,
        completed: bool,
        purchase-date: uint
    }
)

(define-map member-purchase-shares
    { coop-name: (string-utf8 50), purchase-id: (string-utf8 30), member: principal }
    {
        share-percentage: uint,
        amount-paid: uint,
        items-received: bool
    }
)

;; Public Functions
(define-public (create-cooperative (coop-name (string-utf8 50)) (description (string-utf8 200)) (min-contribution uint) (voting-threshold uint))
    (begin
        (asserts! (is-none (map-get? cooperatives coop-name)) err-coop-exists)
        (asserts! (and (>= voting-threshold u51) (<= voting-threshold u100)) err-invalid-percentage)
        (asserts! (> min-contribution u0) err-invalid-amount)
        
        (map-set cooperatives coop-name
            {
                founder: tx-sender,
                description: description,
                member-count: u1,
                total-funds: u0,
                created-at: stacks-block-height,
                active: true,
                min-member-contribution: min-contribution,
                voting-threshold: voting-threshold
            }
        )
        
        (map-set cooperative-members { coop-name: coop-name, member: tx-sender }
            {
                contribution-amount: u0,
                join-date: stacks-block-height,
                voting-power: u100,
                reputation-score: u100,
                active: true
            }
        )
        
        (ok true)
    )
)

(define-public (join-cooperative (coop-name (string-utf8 50)) (initial-contribution uint))
    (let (
        (coop (unwrap! (map-get? cooperatives coop-name) err-coop-not-found))
        (existing-member (map-get? cooperative-members { coop-name: coop-name, member: tx-sender }))
    )
        (asserts! (is-none existing-member) err-already-member)
        (asserts! (get active coop) err-coop-not-found)
        (asserts! (>= initial-contribution (get min-member-contribution coop)) err-invalid-amount)
        
        (try! (stx-transfer? initial-contribution tx-sender contract-owner))
        
        (map-set cooperative-members { coop-name: coop-name, member: tx-sender }
            {
                contribution-amount: initial-contribution,
                join-date: stacks-block-height,
                voting-power: (if (< (/ (* initial-contribution u100) (get min-member-contribution coop)) u100) 
                            (/ (* initial-contribution u100) (get min-member-contribution coop)) 
                            u100),
                reputation-score: u50,
                active: true
            }
        )
        
        (map-set cooperatives coop-name
            (merge coop {
                member-count: (+ (get member-count coop) u1),
                total-funds: (+ (get total-funds coop) initial-contribution)
            })
        )
        
        (ok true)
    )
)

(define-public (contribute-to-cooperative (coop-name (string-utf8 50)) (amount uint))
    (let (
        (coop (unwrap! (map-get? cooperatives coop-name) err-coop-not-found))
        (member (unwrap! (map-get? cooperative-members { coop-name: coop-name, member: tx-sender }) err-not-member))
    )
        (asserts! (get active member) err-not-member)
        (asserts! (> amount u0) err-invalid-amount)
        
        (try! (stx-transfer? amount tx-sender contract-owner))
        
        (map-set cooperative-members { coop-name: coop-name, member: tx-sender }
            (merge member {
                contribution-amount: (+ (get contribution-amount member) amount),
                voting-power: (if (< (+ (get voting-power member) (/ (* amount u10) (get min-member-contribution coop))) u200)
                            (+ (get voting-power member) (/ (* amount u10) (get min-member-contribution coop)))
                            u200)
            })
        )
        
        (map-set cooperatives coop-name
            (merge coop {
                total-funds: (+ (get total-funds coop) amount)
            })
        )
        
        (ok true)
    )
)

(define-public (create-proposal (coop-name (string-utf8 50)) (proposal-id (string-utf8 30)) (proposal-type (string-utf8 20)) (description (string-utf8 300)) (amount-requested uint) (voting-period uint))
    (let (
        (coop (unwrap! (map-get? cooperatives coop-name) err-coop-not-found))
        (member (unwrap! (map-get? cooperative-members { coop-name: coop-name, member: tx-sender }) err-not-member))
        (voting-deadline (+ stacks-block-height voting-period))
    )
        (asserts! (get active member) err-not-member)
        (asserts! (> voting-period u144) err-invalid-voting-period)  ;; Minimum 1 day
        (asserts! (is-none (map-get? cooperative-proposals { coop-name: coop-name, proposal-id: proposal-id })) (err u214))
        
        (map-set cooperative-proposals { coop-name: coop-name, proposal-id: proposal-id }
            {
                proposer: tx-sender,
                proposal-type: proposal-type,
                description: description,
                amount-requested: amount-requested,
                voting-deadline: voting-deadline,
                votes-for: u0,
                votes-against: u0,
                executed: false,
                passed: false
            }
        )
        
        (ok true)
    )
)

(define-public (vote-on-proposal (coop-name (string-utf8 50)) (proposal-id (string-utf8 30)) (vote bool))
    (let (
        (proposal (unwrap! (map-get? cooperative-proposals { coop-name: coop-name, proposal-id: proposal-id }) err-proposal-not-found))
        (member (unwrap! (map-get? cooperative-members { coop-name: coop-name, member: tx-sender }) err-not-member))
        (existing-vote (map-get? proposal-votes { coop-name: coop-name, proposal-id: proposal-id, voter: tx-sender }))
        (voting-power (get voting-power member))
    )
        (asserts! (get active member) err-not-member)
        (asserts! (< stacks-block-height (get voting-deadline proposal)) err-voting-ended)
        (asserts! (is-none existing-vote) (err u215))
        
        (map-set proposal-votes { coop-name: coop-name, proposal-id: proposal-id, voter: tx-sender }
            {
                vote: vote,
                voting-power-used: voting-power,
                timestamp: stacks-block-height
            }
        )
        
        (map-set cooperative-proposals { coop-name: coop-name, proposal-id: proposal-id }
            (merge proposal {
                votes-for: (if vote (+ (get votes-for proposal) voting-power) (get votes-for proposal)),
                votes-against: (if vote (get votes-against proposal) (+ (get votes-against proposal) voting-power))
            })
        )
        
        (ok true)
    )
)

(define-public (execute-proposal (coop-name (string-utf8 50)) (proposal-id (string-utf8 30)))
    (let (
        (proposal (unwrap! (map-get? cooperative-proposals { coop-name: coop-name, proposal-id: proposal-id }) err-proposal-not-found))
        (coop (unwrap! (map-get? cooperatives coop-name) err-coop-not-found))
        (total-votes (+ (get votes-for proposal) (get votes-against proposal)))
        (approval-percentage (if (> total-votes u0) (/ (* (get votes-for proposal) u100) total-votes) u0))
    )
        (asserts! (>= stacks-block-height (get voting-deadline proposal)) err-voting-ended)
        (asserts! (not (get executed proposal)) (err u216))
        (asserts! (>= approval-percentage (get voting-threshold coop)) err-proposal-not-passed)
        (asserts! (>= (get total-funds coop) (get amount-requested proposal)) err-insufficient-funds)
        
        (map-set cooperative-proposals { coop-name: coop-name, proposal-id: proposal-id }
            (merge proposal {
                executed: true,
                passed: true
            })
        )
        
        (map-set cooperatives coop-name
            (merge coop {
                total-funds: (- (get total-funds coop) (get amount-requested proposal))
            })
        )
        
        (ok true)
    )
)

(define-public (add-resource-to-pool (coop-name (string-utf8 50)) (resource-type (string-utf8 30)) (quantity uint) (cost-per-unit uint))
    (let (
        (member (unwrap! (map-get? cooperative-members { coop-name: coop-name, member: tx-sender }) err-not-member))
        (existing-pool (map-get? resource-pools { coop-name: coop-name, resource-type: resource-type }))
        (contribution-value (* quantity cost-per-unit))
    )
        (asserts! (get active member) err-not-member)
        (asserts! (> quantity u0) err-invalid-amount)
        
        (match existing-pool
            pool (map-set resource-pools { coop-name: coop-name, resource-type: resource-type }
                {
                    total-quantity: (+ (get total-quantity pool) quantity),
                    available-quantity: (+ (get available-quantity pool) quantity),
                    cost-per-unit: (/ (+ (* (get cost-per-unit pool) (get total-quantity pool)) contribution-value) (+ (get total-quantity pool) quantity)),
                    coordinator: (get coordinator pool),
                    last-updated: stacks-block-height
                })
            (map-set resource-pools { coop-name: coop-name, resource-type: resource-type }
                {
                    total-quantity: quantity,
                    available-quantity: quantity,
                    cost-per-unit: cost-per-unit,
                    coordinator: tx-sender,
                    last-updated: stacks-block-height
                })
        )
        
        (match (map-get? member-resource-contributions { coop-name: coop-name, member: tx-sender, resource-type: resource-type })
            existing-contribution (map-set member-resource-contributions { coop-name: coop-name, member: tx-sender, resource-type: resource-type }
                {
                    quantity-contributed: (+ (get quantity-contributed existing-contribution) quantity),
                    contribution-value: (+ (get contribution-value existing-contribution) contribution-value),
                    last-contribution: stacks-block-height
                })
            (map-set member-resource-contributions { coop-name: coop-name, member: tx-sender, resource-type: resource-type }
                {
                    quantity-contributed: quantity,
                    contribution-value: contribution-value,
                    last-contribution: stacks-block-height
                })
        )
        
        (ok true)
    )
)

(define-public (organize-group-purchase (coop-name (string-utf8 50)) (purchase-id (string-utf8 30)) (item-description (string-utf8 100)) (total-cost uint))
    (let (
        (member (unwrap! (map-get? cooperative-members { coop-name: coop-name, member: tx-sender }) err-not-member))
        (coop (unwrap! (map-get? cooperatives coop-name) err-coop-not-found))
    )
        (asserts! (get active member) err-not-member)
        (asserts! (> total-cost u0) err-invalid-amount)
        (asserts! (is-none (map-get? cooperative-purchases { coop-name: coop-name, purchase-id: purchase-id })) (err u217))
        
        (map-set cooperative-purchases { coop-name: coop-name, purchase-id: purchase-id }
            {
                item-description: item-description,
                total-cost: total-cost,
                coordinator: tx-sender,
                participants: u0,
                completed: false,
                purchase-date: stacks-block-height
            }
        )
        
        (ok true)
    )
)

(define-public (participate-in-group-purchase (coop-name (string-utf8 50)) (purchase-id (string-utf8 30)) (share-percentage uint))
    (let (
        (member (unwrap! (map-get? cooperative-members { coop-name: coop-name, member: tx-sender }) err-not-member))
        (purchase (unwrap! (map-get? cooperative-purchases { coop-name: coop-name, purchase-id: purchase-id }) err-proposal-not-found))
        (payment-amount (/ (* (get total-cost purchase) share-percentage) u100))
    )
        (asserts! (get active member) err-not-member)
        (asserts! (not (get completed purchase)) (err u218))
        (asserts! (and (> share-percentage u0) (<= share-percentage u100)) err-invalid-percentage)
        (asserts! (is-none (map-get? member-purchase-shares { coop-name: coop-name, purchase-id: purchase-id, member: tx-sender })) (err u219))
        
        (try! (stx-transfer? payment-amount tx-sender contract-owner))
        
        (map-set member-purchase-shares { coop-name: coop-name, purchase-id: purchase-id, member: tx-sender }
            {
                share-percentage: share-percentage,
                amount-paid: payment-amount,
                items-received: false
            }
        )
        
        (map-set cooperative-purchases { coop-name: coop-name, purchase-id: purchase-id }
            (merge purchase {
                participants: (+ (get participants purchase) u1)
            })
        )
        
        (ok true)
    )
)

;; Read-only functions
(define-read-only (get-cooperative (coop-name (string-utf8 50)))
    (map-get? cooperatives coop-name)
)

(define-read-only (get-member-info (coop-name (string-utf8 50)) (member principal))
    (map-get? cooperative-members { coop-name: coop-name, member: member })
)

(define-read-only (get-proposal (coop-name (string-utf8 50)) (proposal-id (string-utf8 30)))
    (map-get? cooperative-proposals { coop-name: coop-name, proposal-id: proposal-id })
)

(define-read-only (get-resource-pool (coop-name (string-utf8 50)) (resource-type (string-utf8 30)))
    (map-get? resource-pools { coop-name: coop-name, resource-type: resource-type })
)

(define-read-only (get-group-purchase (coop-name (string-utf8 50)) (purchase-id (string-utf8 30)))
    (map-get? cooperative-purchases { coop-name: coop-name, purchase-id: purchase-id })
)

(define-read-only (get-member-vote (coop-name (string-utf8 50)) (proposal-id (string-utf8 30)) (voter principal))
    (map-get? proposal-votes { coop-name: coop-name, proposal-id: proposal-id, voter: voter })
)

(define-read-only (get-member-purchase-share (coop-name (string-utf8 50)) (purchase-id (string-utf8 30)) (member principal))
    (map-get? member-purchase-shares { coop-name: coop-name, purchase-id: purchase-id, member: member })
)
