;; CarbonVault: Decentralized carbon credit trading platform
;; Enables organizations to issue carbon credits, trade them, and get environmental authority verification

(define-data-var environmental-authority principal tx-sender)

(define-map credit-registry
  { credit-id: uint }
  {
    issuer: principal,
    price-per-ton: uint,
    project-name: (string-ascii 50),
    impact-description: (string-ascii 500),
    carbon-tons: uint,
    verified: bool
  }
)

(define-map transaction-ledger
  { credit-id: uint, ledger-id: uint }
  {
    trader: principal,
    trade-date: uint,
    action: (string-ascii 20)
  }
)

(define-data-var next-credit-id uint u1)

(define-map ledger-counter
  { credit-id: uint }
  { transactions: uint }
)

;; Issue new carbon credits
(define-public (issue-credits (name-input (string-ascii 50)) (impact-input (string-ascii 500)) (tons-input uint) (price-input uint))
  (let
    (
      (credit-id (var-get next-credit-id))
      (ledger-id u0)
      (name name-input)
      (impact impact-input)
      (tons tons-input)
      (price price-input)
    )
    ;; Input validation
    (asserts! (> price u0) (err u1))
    (asserts! (> (len name) u0) (err u5))
    (asserts! (> (len impact) u0) (err u6))
    (asserts! (> tons u0) (err u7))
    
    (map-set credit-registry
      { credit-id: credit-id }
      {
        issuer: tx-sender,
        price-per-ton: price,
        project-name: name,
        impact-description: impact,
        carbon-tons: tons,
        verified: false
      }
    )
    (map-set transaction-ledger
      { credit-id: credit-id, ledger-id: ledger-id }
      {
        trader: tx-sender,
        trade-date: credit-id,
        action: "issued"
      }
    )
    (map-set ledger-counter
      { credit-id: credit-id }
      { transactions: u1 }
    )
    (var-set next-credit-id (+ credit-id u1))
    (ok credit-id)
  )
)

;; Trade carbon credits
(define-public (trade-credits (credit-id-input uint))
  (let
    (
      (credit-id credit-id-input)
      (credit-info (unwrap! (map-get? credit-registry { credit-id: credit-id }) (err u2)))
      (price (get price-per-ton credit-info))
      (issuer (get issuer credit-info))
      (ledger-data (default-to { transactions: u0 } (map-get? ledger-counter { credit-id: credit-id })))
      (ledger-id (get transactions ledger-data))
      (new-ledger-id (+ ledger-id u1))
    )
    ;; Input validation
    (asserts! (> credit-id u0) (err u8))
    (asserts! (not (is-eq tx-sender issuer)) (err u3))
    
    (try! (stx-transfer? price tx-sender issuer))
    (map-set transaction-ledger
      { credit-id: credit-id, ledger-id: ledger-id }
      {
        trader: tx-sender,
        trade-date: (var-get next-credit-id),
        action: "purchased"
      }
    )
    (map-set ledger-counter
      { credit-id: credit-id }
      { transactions: new-ledger-id }
    )
    (ok true)
  )
)

;; Verify carbon credits (authority only)
(define-public (verify-credits (credit-id-input uint))
  (let
    (
      (credit-id credit-id-input)
      (credit-info (unwrap! (map-get? credit-registry { credit-id: credit-id }) (err u2)))
      (ledger-data (default-to { transactions: u0 } (map-get? ledger-counter { credit-id: credit-id })))
      (ledger-id (get transactions ledger-data))
      (new-ledger-id (+ ledger-id u1))
    )
    ;; Input validation
    (asserts! (> credit-id u0) (err u8))
    (asserts! (is-eq tx-sender (var-get environmental-authority)) (err u4))
    
    (map-set credit-registry
      { credit-id: credit-id }
      (merge credit-info { verified: true })
    )
    (map-set transaction-ledger
      { credit-id: credit-id, ledger-id: ledger-id }
      {
        trader: (get issuer credit-info),
        trade-date: (var-get next-credit-id),
        action: "verified"
      }
    )
    (map-set ledger-counter
      { credit-id: credit-id }
      { transactions: new-ledger-id }
    )
    (ok true)
  )
)

;; Get credit details
(define-read-only (get-credit (credit-id uint))
  (map-get? credit-registry { credit-id: credit-id })
)

;; Get transaction ledger entry
(define-read-only (get-transaction-ledger (credit-id uint) (ledger-id uint))
  (map-get? transaction-ledger { credit-id: credit-id, ledger-id: ledger-id })
)

;; Get total transactions
(define-read-only (get-transaction-count (credit-id uint))
  (let
    (
      (ledger-data (default-to { transactions: u0 } (map-get? ledger-counter { credit-id: credit-id })))
    )
    (get transactions ledger-data)
  )
)