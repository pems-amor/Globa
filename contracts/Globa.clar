;; Decentralized Prediction Markets & Information Discovery Platform
;; A protocol for creating and trading on any future event or outcome

;; Constants
(define-constant contract-owner tx-sender)
(define-constant min-bet-amount u100000) ;; 0.1 STX minimum bet (100k micro-STX)
(define-constant max-bet-amount u100000000000) ;; 100K STX maximum bet
(define-constant market-creation-fee u10000000) ;; 10 STX fee to create market
(define-constant trading-fee-rate u200) ;; 2% trading fee (200/10000)
(define-constant oracle-fee-rate u100) ;; 1% oracle fee (100/10000)
(define-constant liquidity-provider-reward u5000) ;; 50% of fees to LPs (5000/10000)
(define-constant resolution-window u1440) ;; ~10 days for dispute resolution (1440 blocks)
(define-constant min-liquidity-requirement u50000000) ;; 50 STX minimum initial liquidity

;; Market types
(define-constant market-type-binary u1) ;; Yes/No questions
(define-constant market-type-scalar u2) ;; Ranged predictions (0-100, prices, etc.)
(define-constant market-type-categorical u3) ;; Multiple choice (A, B, C, D)

;; Market status
(define-constant market-status-active u1)
(define-constant market-status-closed u2)
(define-constant market-status-resolved u3)
(define-constant market-status-disputed u4)

;; Error codes
(define-constant err-not-authorized (err u100))
(define-constant err-market-not-found (err u101))
(define-constant err-market-closed (err u102))
(define-constant err-insufficient-balance (err u103))
(define-constant err-invalid-outcome (err u104))
(define-constant err-market-not-resolved (err u105))
(define-constant err-already-resolved (err u106))
(define-constant err-dispute-period-ended (err u107))
(define-constant err-insufficient-liquidity (err u108))
(define-constant err-invalid-market-type (err u109))
(define-constant err-oracle-not-authorized (err u110))

;; Prediction market definitions
(define-map prediction-markets
  { market-id: uint }
  {
    market-title: (string-ascii 128),
    market-description: (string-ascii 256),
    market-type: uint,
    creator: principal,
    creation-block: uint,
    closing-block: uint,
    resolution-block: (optional uint),
    total-volume: uint,
    total-liquidity: uint,
    yes-shares: uint, ;; For binary markets
    no-shares: uint,  ;; For binary markets
    current-price: uint, ;; Current market price (0-10000 for binary, actual value for scalar)
    status: uint,
    winning-outcome: (optional uint),
    oracle-source: (string-ascii 64),
    category: (string-ascii 32), ;; "sports", "politics", "crypto", "weather", etc.
    is-featured: bool
  }
)

;; User positions in prediction markets
(define-map user-positions
  { user: principal, market-id: uint }
  {
    yes-shares: uint,
    no-shares: uint,
    scalar-shares: uint,
    average-buy-price: uint,
    total-invested: uint,
    realized-pnl: int,
    unrealized-pnl: int,
    last-trade-block: uint
  }
)

;; Market maker pools for providing liquidity
(define-map amm-pools
  { market-id: uint }
  {
    yes-liquidity: uint,
    no-liquidity: uint,
    total-lp-tokens: uint,
    k-constant: uint, ;; x * y = k for AMM
    fee-accumulator: uint,
    last-price-update: uint
  }
)

(define-map lp-positions
  { lp: principal, market-id: uint }
  {
    lp-tokens: uint,
    deposited-amount: uint,
    entry-price: uint,
    fees-earned: uint,
    entry-block: uint
  }
)

;; Oracle system for market resolution
(define-map authorized-oracles
  { oracle: principal }
  {
    reputation-score: uint,
    markets-resolved: uint,
    correct-resolutions: uint,
    is-active: bool,
    specialization: (string-ascii 32) ;; "sports", "politics", etc.
  }
)

(define-map market-resolutions
  { market-id: uint }
  {
    resolver: principal,
    resolution-data: (string-ascii 128),
    resolution-block: uint,
    is-disputed: bool,
    dispute-count: uint,
    final-outcome: uint
  }
)

;; Dispute system
(define-map resolution-disputes
  { dispute-id: uint }
  {
    market-id: uint,
    disputer: principal,
    disputed-outcome: uint,
    proposed-outcome: uint,
    dispute-bond: uint,
    dispute-block: uint,
    is-resolved: bool,
    dispute-winner: (optional principal)
  }
)

;; Social features - following successful predictors
(define-map predictor-profiles
  { predictor: principal }
  {
    total-predictions: uint,
    correct-predictions: uint,
    total-profit: uint,
    win-rate: uint,
    followers: uint,
    reputation-score: uint,
    specialization: (string-ascii 32)
  }
)

(define-map social-follows
  { follower: principal, predictor: principal }
  { 
    follow-date: uint,
    copy-trade-enabled: bool,
    copy-trade-allocation: uint
  }
)

;; Leaderboards and analytics
(define-map daily-stats
  { day: uint }
  {
    markets-created: uint,
    total-volume: uint,
    unique-traders: uint,
    total-resolved: uint,
    average-accuracy: uint
  }
)

(define-map category-stats
  { category: (string-ascii 32) }
  {
    total-markets: uint,
    total-volume: uint,
    average-accuracy: uint,
    top-predictor: (optional principal)
  }
)

;; Global state variables
(define-data-var next-market-id uint u1)
(define-data-var next-dispute-id uint u1)
(define-data-var total-markets-created uint u0)
(define-data-var total-prediction-volume uint u0)
(define-data-var total-protocol-fees uint u0)
(define-data-var total-oracle-fees uint u0)
(define-data-var emergency-pause bool false)

;; Helper functions for AMM calculations
(define-private (min-uint (a uint) (b uint))
  (if (<= a b) a b))

(define-private (calculate-amm-price (yes-liquidity uint) (no-liquidity uint))
  ;; Price = yes_liquidity / (yes_liquidity + no_liquidity) * 10000
  (/ (* yes-liquidity u10000) (+ yes-liquidity no-liquidity)))

(define-private (calculate-shares-out (liquidity-in uint) (current-liquidity uint) (k-constant uint))
  ;; Simplified AMM calculation - in production would use more sophisticated math
  (/ (* liquidity-in current-liquidity) (+ current-liquidity liquidity-in)))

(define-private (calculate-trading-fee (amount uint))
  (/ (* amount trading-fee-rate) u10000))

(define-private (calculate-slippage (amount-in uint) (liquidity uint))
  ;; Simple slippage calculation
  (/ (* amount-in u100) liquidity))

(define-private (update-predictor-stats (user principal) (profit int) (was-correct bool))
  (let ((current-profile (default-to
                           { total-predictions: u0, correct-predictions: u0, total-profit: u0,
                             win-rate: u0, followers: u0, reputation-score: u1000, specialization: "general" }
                           (map-get? predictor-profiles { predictor: user }))))
    
    (map-set predictor-profiles { predictor: user }
      {
        total-predictions: (+ (get total-predictions current-profile) u1),
        correct-predictions: (if was-correct 
                               (+ (get correct-predictions current-profile) u1)
                               (get correct-predictions current-profile)),
        total-profit: (if (> profit 0)
                        (+ (get total-profit current-profile) (to-uint profit))
                        (if (>= (get total-profit current-profile) (to-uint (- profit)))
                          (- (get total-profit current-profile) (to-uint (- profit)))
                          u0)),
        win-rate: (if (> (+ (get total-predictions current-profile) u1) u0)
                    (/ (* (if was-correct 
                            (+ (get correct-predictions current-profile) u1)
                            (get correct-predictions current-profile)) u10000) 
                       (+ (get total-predictions current-profile) u1))
                    u0),
        followers: (get followers current-profile),
        reputation-score: (if was-correct
                            (min-uint u10000 (+ (get reputation-score current-profile) u100))
                            (if (>= (get reputation-score current-profile) u50)
                              (- (get reputation-score current-profile) u50)
                              u0)),
        specialization: (get specialization current-profile)
      })))

;; Core Market Functions

;; 1. Create new prediction market
(define-public (create-market
                (market-title (string-ascii 128))
                (market-description (string-ascii 256))
                (market-type uint)
                (blocks-until-close uint)
                (oracle-source (string-ascii 64))
                (category (string-ascii 32))
                (initial-liquidity uint))
  (let ((market-id (var-get next-market-id))
        (closing-block (+ stacks-block-height blocks-until-close)))
    
    (asserts! (not (var-get emergency-pause)) err-not-authorized)
    (asserts! (or (is-eq market-type market-type-binary)
                  (or (is-eq market-type market-type-scalar)
                      (is-eq market-type market-type-categorical))) err-invalid-market-type)
    (asserts! (>= initial-liquidity min-liquidity-requirement) err-insufficient-liquidity)
    
    ;; Transfer market creation fee + initial liquidity
    (try! (stx-transfer? (+ market-creation-fee initial-liquidity) tx-sender (as-contract tx-sender)))
    
    ;; Create prediction market
    (map-set prediction-markets { market-id: market-id }
      {
        market-title: market-title,
        market-description: market-description,
        market-type: market-type,
        creator: tx-sender,
        creation-block: stacks-block-height,
        closing-block: closing-block,
        resolution-block: none,
        total-volume: u0,
        total-liquidity: initial-liquidity,
        yes-shares: (/ initial-liquidity u2), ;; Split initial liquidity 50/50
        no-shares: (/ initial-liquidity u2),
        current-price: u5000, ;; 50% initial probability
        status: market-status-active,
        winning-outcome: none,
        oracle-source: oracle-source,
        category: category,
        is-featured: false
      })
    
    ;; Initialize AMM pool
    (map-set amm-pools { market-id: market-id }
      {
        yes-liquidity: (/ initial-liquidity u2),
        no-liquidity: (/ initial-liquidity u2),
        total-lp-tokens: initial-liquidity,
        k-constant: (/ (* initial-liquidity initial-liquidity) u4), ;; (x * y)
        fee-accumulator: u0,
        last-price-update: stacks-block-height
      })
    
    ;; Give creator initial LP tokens
    (map-set lp-positions { lp: tx-sender, market-id: market-id }
      {
        lp-tokens: initial-liquidity,
        deposited-amount: initial-liquidity,
        entry-price: u5000,
        fees-earned: u0,
        entry-block: stacks-block-height
      })
    
    ;; Update global stats
    (var-set next-market-id (+ market-id u1))
    (var-set total-markets-created (+ (var-get total-markets-created) u1))
    (var-set total-protocol-fees (+ (var-get total-protocol-fees) market-creation-fee))
    
    (print {
      event: "market-created",
      market-id: market-id,
      creator: tx-sender,
      title: market-title,
      category: category,
      closing-block: closing-block
    })
    
    (ok market-id)
  )
)

;; 2. Buy prediction shares (YES or NO for binary markets)
(define-public (buy-shares (market-id uint) (outcome uint) (stx-amount uint))
  (let ((market (unwrap! (map-get? prediction-markets { market-id: market-id }) err-market-not-found))
        (pool (unwrap! (map-get? amm-pools { market-id: market-id }) err-market-not-found)))
    
    (asserts! (not (var-get emergency-pause)) err-not-authorized)
    (asserts! (is-eq (get status market) market-status-active) err-market-closed)
    (asserts! (< stacks-block-height (get closing-block market)) err-market-closed)
    (asserts! (>= stx-amount min-bet-amount) err-insufficient-balance)
    (asserts! (<= stx-amount max-bet-amount) err-insufficient-balance)
    (asserts! (or (is-eq outcome u0) (is-eq outcome u1)) err-invalid-outcome) ;; 0 = NO, 1 = YES
    
    ;; Calculate shares to receive using AMM formula
    (let ((trading-fee (calculate-trading-fee stx-amount))
          (net-amount (- stx-amount trading-fee))
          (current-yes-liquidity (get yes-liquidity pool))
          (current-no-liquidity (get no-liquidity pool))
          (shares-out (if (is-eq outcome u1)
                        ;; Buying YES shares
                        (calculate-shares-out net-amount current-yes-liquidity (get k-constant pool))
                        ;; Buying NO shares  
                        (calculate-shares-out net-amount current-no-liquidity (get k-constant pool)))))
      
      ;; Transfer payment from user
      (try! (stx-transfer? stx-amount tx-sender (as-contract tx-sender)))
      
      ;; Update user position
      (let ((current-position (default-to
                                { yes-shares: u0, no-shares: u0, scalar-shares: u0,
                                  average-buy-price: u0, total-invested: u0, realized-pnl: 0,
                                  unrealized-pnl: 0, last-trade-block: u0 }
                                (map-get? user-positions { user: tx-sender, market-id: market-id }))))
        
        (map-set user-positions { user: tx-sender, market-id: market-id }
          {
            yes-shares: (if (is-eq outcome u1) (+ (get yes-shares current-position) shares-out) (get yes-shares current-position)),
            no-shares: (if (is-eq outcome u0) (+ (get no-shares current-position) shares-out) (get no-shares current-position)),
            scalar-shares: (get scalar-shares current-position),
            average-buy-price: (/ (+ (* (get average-buy-price current-position) (get total-invested current-position)) (* (calculate-amm-price current-yes-liquidity current-no-liquidity) net-amount)) (+ (get total-invested current-position) net-amount)),
            total-invested: (+ (get total-invested current-position) net-amount),
            realized-pnl: (get realized-pnl current-position),
            unrealized-pnl: (get unrealized-pnl current-position),
            last-trade-block: stacks-block-height
          }))
      
      ;; Update AMM pool
      (map-set amm-pools { market-id: market-id }
        (merge pool {
          yes-liquidity: (if (is-eq outcome u1) (+ current-yes-liquidity net-amount) current-yes-liquidity),
          no-liquidity: (if (is-eq outcome u0) (+ current-no-liquidity net-amount) current-no-liquidity),
          fee-accumulator: (+ (get fee-accumulator pool) trading-fee),
          last-price-update: stacks-block-height
        }))
      
      ;; Update market stats
      (let ((new-price (calculate-amm-price 
                         (if (is-eq outcome u1) (+ current-yes-liquidity net-amount) current-yes-liquidity)
                         (if (is-eq outcome u0) (+ current-no-liquidity net-amount) current-no-liquidity))))
        (map-set prediction-markets { market-id: market-id }
          (merge market {
            total-volume: (+ (get total-volume market) stx-amount),
            yes-shares: (if (is-eq outcome u1) (+ (get yes-shares market) shares-out) (get yes-shares market)),
            no-shares: (if (is-eq outcome u0) (+ (get no-shares market) shares-out) (get no-shares market)),
            current-price: new-price
          })))
      
      ;; Update global stats
      (var-set total-prediction-volume (+ (var-get total-prediction-volume) stx-amount))
      (var-set total-protocol-fees (+ (var-get total-protocol-fees) (/ trading-fee u2))) ;; 50% to protocol
      
      (print {
        event: "shares-purchased",
        user: tx-sender,
        market-id: market-id,
        outcome: outcome,
        shares: shares-out,
        amount-paid: stx-amount,
        new-price: (calculate-amm-price 
                     (if (is-eq outcome u1) (+ current-yes-liquidity net-amount) current-yes-liquidity)
                     (if (is-eq outcome u0) (+ current-no-liquidity net-amount) current-no-liquidity))
      })
      
      (ok shares-out)
    )
  )
)

;; 3. Sell prediction shares
(define-public (sell-shares (market-id uint) (outcome uint) (shares uint))
  (let ((market (unwrap! (map-get? prediction-markets { market-id: market-id }) err-market-not-found))
        (position (unwrap! (map-get? user-positions { user: tx-sender, market-id: market-id }) err-insufficient-balance))
        (pool (unwrap! (map-get? amm-pools { market-id: market-id }) err-market-not-found)))
    
    (asserts! (not (var-get emergency-pause)) err-not-authorized)
    (asserts! (is-eq (get status market) market-status-active) err-market-closed)
    
    ;; Check user has enough shares
    (let ((user-shares (if (is-eq outcome u1) (get yes-shares position) (get no-shares position))))
      (asserts! (>= user-shares shares) err-insufficient-balance)
      
      ;; Calculate STX to receive (simplified AMM calculation)
      (let ((current-yes-liquidity (get yes-liquidity pool))
            (current-no-liquidity (get no-liquidity pool))
            (stx-out (if (is-eq outcome u1)
                       ;; Selling YES shares
                       (/ (* shares current-yes-liquidity) (+ (get yes-shares market) shares))
                       ;; Selling NO shares
                       (/ (* shares current-no-liquidity) (+ (get no-shares market) shares))))
            (trading-fee (calculate-trading-fee stx-out))
            (net-stx-out (- stx-out trading-fee)))
        
        ;; Transfer proceeds to user
        (try! (as-contract (stx-transfer? net-stx-out tx-sender tx-sender)))
        
        ;; Update user position
        (map-set user-positions { user: tx-sender, market-id: market-id }
          (merge position {
            yes-shares: (if (is-eq outcome u1) (- (get yes-shares position) shares) (get yes-shares position)),
            no-shares: (if (is-eq outcome u0) (- (get no-shares position) shares) (get no-shares position)),
            realized-pnl: (+ (get realized-pnl position) (to-int (- net-stx-out (/ (* shares (get average-buy-price position)) u10000)))),
            last-trade-block: stacks-block-height
          }))
        
        ;; Update AMM pool
        (map-set amm-pools { market-id: market-id }
          (merge pool {
            yes-liquidity: (if (is-eq outcome u1) (- current-yes-liquidity stx-out) current-yes-liquidity),
            no-liquidity: (if (is-eq outcome u0) (- current-no-liquidity stx-out) current-no-liquidity),
            fee-accumulator: (+ (get fee-accumulator pool) trading-fee)
          }))
        
        ;; Update market
        (map-set prediction-markets { market-id: market-id }
          (merge market {
            total-volume: (+ (get total-volume market) stx-out),
            yes-shares: (if (is-eq outcome u1) (- (get yes-shares market) shares) (get yes-shares market)),
            no-shares: (if (is-eq outcome u0) (- (get no-shares market) shares) (get no-shares market)),
            current-price: (calculate-amm-price 
                             (if (is-eq outcome u1) (- current-yes-liquidity stx-out) current-yes-liquidity)
                             (if (is-eq outcome u0) (- current-no-liquidity stx-out) current-no-liquidity))
          }))
        
        (print {
          event: "shares-sold",
          user: tx-sender,
          market-id: market-id,
          outcome: outcome,
          shares: shares,
          stx-received: net-stx-out
        })
        
        (ok net-stx-out)
      )
    )
  )
)

;; 4. Resolve market (oracle function)
(define-public (resolve-market (market-id uint) (winning-outcome uint) (resolution-data (string-ascii 128)))
  (let ((market (unwrap! (map-get? prediction-markets { market-id: market-id }) err-market-not-found)))
    
    (asserts! (>= stacks-block-height (get closing-block market)) err-market-closed)
    (asserts! (is-eq (get status market) market-status-active) err-already-resolved)
    (asserts! (or (is-eq winning-outcome u0) (is-eq winning-outcome u1)) err-invalid-outcome)
    
    ;; For now, allow anyone to resolve - in production would verify oracle authorization
    
    ;; Mark market as resolved
    (map-set prediction-markets { market-id: market-id }
      (merge market {
        status: market-status-resolved,
        winning-outcome: (some winning-outcome),
        resolution-block: (some stacks-block-height)
      }))
    
    ;; Create resolution record
    (map-set market-resolutions { market-id: market-id }
      {
        resolver: tx-sender,
        resolution-data: resolution-data,
        resolution-block: stacks-block-height,
        is-disputed: false,
        dispute-count: u0,
        final-outcome: winning-outcome
      })
    
    (print {
      event: "market-resolved",
      market-id: market-id,
      winning-outcome: winning-outcome,
      resolver: tx-sender
    })
    
    (ok winning-outcome)
  )
)

;; 5. Claim winnings from resolved market
(define-public (claim-winnings (market-id uint))
  (let ((market (unwrap! (map-get? prediction-markets { market-id: market-id }) err-market-not-found))
        (position (unwrap! (map-get? user-positions { user: tx-sender, market-id: market-id }) err-insufficient-balance)))
    
    (asserts! (is-eq (get status market) market-status-resolved) err-market-not-resolved)
    
    (let ((winning-outcome (unwrap! (get winning-outcome market) err-market-not-resolved))
          (winning-shares (if (is-eq winning-outcome u1) (get yes-shares position) (get no-shares position)))
          (total-winning-shares (if (is-eq winning-outcome u1) (get yes-shares market) (get no-shares market))))
      
      (asserts! (> winning-shares u0) err-insufficient-balance)
      
      ;; Calculate payout (winner takes all from pool)
      (let ((total-pool (get total-liquidity market))
            (user-payout (/ (* winning-shares total-pool) total-winning-shares))
            (oracle-fee (/ (* user-payout oracle-fee-rate) u10000))
            (net-payout (- user-payout oracle-fee)))
        
        ;; Transfer winnings to user
        (try! (as-contract (stx-transfer? net-payout tx-sender tx-sender)))
        
        ;; Update predictor stats
        (update-predictor-stats tx-sender (to-int net-payout) true)
        
        ;; Clear user position
        (map-delete user-positions { user: tx-sender, market-id: market-id })
        
        ;; Update oracle fees
        (var-set total-oracle-fees (+ (var-get total-oracle-fees) oracle-fee))
        
        (print {
          event: "winnings-claimed",
          user: tx-sender,
          market-id: market-id,
          payout: net-payout,
          winning-shares: winning-shares
        })
        
        (ok net-payout)
      )
    )
  )
)

;; Read-only functions

(define-read-only (get-market (market-id uint))
  (map-get? prediction-markets { market-id: market-id })
)

(define-read-only (get-user-position (user principal) (market-id uint))
  (map-get? user-positions { user: user, market-id: market-id })
)

(define-read-only (get-amm-pool (market-id uint))
  (map-get? amm-pools { market-id: market-id })
)

(define-read-only (get-current-price (market-id uint))
  (match (map-get? prediction-markets { market-id: market-id })
    market (some (get current-price market))
    none)
)

(define-read-only (calculate-shares-for-stx (market-id uint) (outcome uint) (stx-amount uint))
  (match (map-get? amm-pools { market-id: market-id })
    pool
      (let ((net-amount (- stx-amount (calculate-trading-fee stx-amount))))
        (some (if (is-eq outcome u1)
                (calculate-shares-out net-amount (get yes-liquidity pool) (get k-constant pool))
                (calculate-shares-out net-amount (get no-liquidity pool) (get k-constant pool)))))
    none)
)

(define-read-only (get-predictor-profile (predictor principal))
  (map-get? predictor-profiles { predictor: predictor })
)

(define-read-only (get-protocol-stats)
  {
    total-markets: (var-get total-markets-created),
    total-volume: (var-get total-prediction-volume),
    total-fees: (var-get total-protocol-fees),
    oracle-fees: (var-get total-oracle-fees),
    active-markets: u0, ;; Would calculate this in production
    emergency-pause: (var-get emergency-pause)
  }
)

(define-read-only (get-market-categories)
  (list "sports" "politics" "crypto" "weather" "economics" "entertainment" "technology")
)

;; Social features

(define-public (follow-predictor (predictor principal))
  (begin
    (map-set social-follows { follower: tx-sender, predictor: predictor }
      {
        follow-date: stacks-block-height,
        copy-trade-enabled: false,
        copy-trade-allocation: u0
      })
    
    ;; Update predictor's follower count
    (let ((current-profile (default-to
                             { total-predictions: u0, correct-predictions: u0, total-profit: u0,
                               win-rate: u0, followers: u0, reputation-score: u1000, specialization: "general" }
                             (map-get? predictor-profiles { predictor: predictor }))))
      (map-set predictor-profiles { predictor: predictor }
        (merge current-profile {
          followers: (+ (get followers current-profile) u1)
        })))
    
    (ok true)
  )
)

;; Admin functions

(define-public (add-authorized-oracle (oracle principal) (specialization (string-ascii 32)))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-not-authorized)
    
    (map-set authorized-oracles { oracle: oracle }
      {
        reputation-score: u1000,
        markets-resolved: u0,
        correct-resolutions: u0,
        is-active: true,
        specialization: specialization
      })
    
    (ok true)
  )
)

(define-public (withdraw-protocol-fees)
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-not-authorized)
    (let ((fees (var-get total-protocol-fees)))
      (var-set total-protocol-fees u0)
      (try! (as-contract (stx-transfer? fees tx-sender contract-owner)))
      (ok fees)
    )
  )
)

(define-public (emergency-pause-toggle)
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-not-authorized)
    (var-set emergency-pause (not (var-get emergency-pause)))
    (ok (var-get emergency-pause))
  )
)