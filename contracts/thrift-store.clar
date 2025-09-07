
;; title: thrift-store
;; version: 1.0.0
;; summary: Thrift Store Operations Management System
;; description: A smart contract for managing thrift store operations including donations, pricing, inventory, and volunteer coordination

;; constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u401))
(define-constant ERR_NOT_FOUND (err u404))
(define-constant ERR_ALREADY_EXISTS (err u409))
(define-constant ERR_INVALID_AMOUNT (err u400))
(define-constant ERR_INSUFFICIENT_INVENTORY (err u402))

;; data vars
(define-data-var next-item-id uint u1)
(define-data-var next-donation-id uint u1)
(define-data-var next-volunteer-id uint u1)

;; data maps
(define-map inventory-items
  uint
  {
    name: (string-ascii 50),
    category: (string-ascii 30),
    condition: (string-ascii 20),
    price: uint,
    quantity: uint,
    donated-by: principal,
    donation-date: uint,
    status: (string-ascii 20)
  }
)

(define-map donations
  uint
  {
    donor: principal,
    total-items: uint,
    donation-date: uint,
    estimated-value: uint,
    processed: bool
  }
)

(define-map volunteers
  uint
  {
    volunteer: principal,
    name: (string-ascii 50),
    role: (string-ascii 30),
    hours-worked: uint,
    registration-date: uint,
    active: bool
  }
)

(define-map store-stats
  (string-ascii 20)
  uint
)

;; public functions
(define-public (add-donation (donor principal) (total-items uint) (estimated-value uint))
  (let (
    (donation-id (var-get next-donation-id))
  )
    (map-set donations donation-id
      {
        donor: donor,
        total-items: total-items,
        donation-date: stacks-block-height,
        estimated-value: estimated-value,
        processed: false
      }
    )
    (var-set next-donation-id (+ donation-id u1))
    (map-set store-stats "total-donations" 
      (+ (default-to u0 (map-get? store-stats "total-donations")) u1)
    )
    (ok donation-id)
  )
)

(define-public (add-inventory-item 
  (name (string-ascii 50))
  (category (string-ascii 30))
  (condition (string-ascii 20))
  (price uint)
  (quantity uint)
  (donated-by principal)
)
  (let (
    (item-id (var-get next-item-id))
  )
    (asserts! (> price u0) ERR_INVALID_AMOUNT)
    (asserts! (> quantity u0) ERR_INVALID_AMOUNT)
    (map-set inventory-items item-id
      {
        name: name,
        category: category,
        condition: condition,
        price: price,
        quantity: quantity,
        donated-by: donated-by,
        donation-date: stacks-block-height,
        status: "available"
      }
    )
    (var-set next-item-id (+ item-id u1))
    (map-set store-stats "total-inventory" 
      (+ (default-to u0 (map-get? store-stats "total-inventory")) quantity)
    )
    (ok item-id)
  )
)

(define-public (update-item-price (item-id uint) (new-price uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (asserts! (> new-price u0) ERR_INVALID_AMOUNT)
    (match (map-get? inventory-items item-id)
      item
      (begin
        (map-set inventory-items item-id
          (merge item { price: new-price })
        )
        (ok true)
      )
      ERR_NOT_FOUND
    )
  )
)

(define-public (sell-item (item-id uint) (quantity-sold uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (match (map-get? inventory-items item-id)
      item
      (begin
        (asserts! (>= (get quantity item) quantity-sold) ERR_INSUFFICIENT_INVENTORY)
        (let (
          (remaining-quantity (- (get quantity item) quantity-sold))
          (sale-amount (* (get price item) quantity-sold))
        )
          (if (is-eq remaining-quantity u0)
            (map-set inventory-items item-id
              (merge item { quantity: u0, status: "sold" })
            )
            (map-set inventory-items item-id
              (merge item { quantity: remaining-quantity })
            )
          )
          (map-set store-stats "total-sales" 
            (+ (default-to u0 (map-get? store-stats "total-sales")) sale-amount)
          )
          (map-set store-stats "items-sold" 
            (+ (default-to u0 (map-get? store-stats "items-sold")) quantity-sold)
          )
          (ok sale-amount)
        )
      )
      ERR_NOT_FOUND
    )
  )
)

(define-public (register-volunteer (volunteer principal) (name (string-ascii 50)) (role (string-ascii 30)))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (let (
      (volunteer-id (var-get next-volunteer-id))
    )
      (map-set volunteers volunteer-id
        {
          volunteer: volunteer,
          name: name,
          role: role,
          hours-worked: u0,
          registration-date: stacks-block-height,
          active: true
        }
      )
      (var-set next-volunteer-id (+ volunteer-id u1))
      (map-set store-stats "total-volunteers" 
        (+ (default-to u0 (map-get? store-stats "total-volunteers")) u1)
      )
      (ok volunteer-id)
    )
  )
)

(define-public (log-volunteer-hours (volunteer-id uint) (hours uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (match (map-get? volunteers volunteer-id)
      volunteer
      (begin
        (map-set volunteers volunteer-id
          (merge volunteer { hours-worked: (+ (get hours-worked volunteer) hours) })
        )
        (map-set store-stats "volunteer-hours" 
          (+ (default-to u0 (map-get? store-stats "volunteer-hours")) hours)
        )
        (ok true)
      )
      ERR_NOT_FOUND
    )
  )
)

(define-public (process-donation (donation-id uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (match (map-get? donations donation-id)
      donation
      (begin
        (asserts! (not (get processed donation)) ERR_ALREADY_EXISTS)
        (map-set donations donation-id
          (merge donation { processed: true })
        )
        (ok true)
      )
      ERR_NOT_FOUND
    )
  )
)

;; read only functions
(define-read-only (get-inventory-item (item-id uint))
  (map-get? inventory-items item-id)
)

(define-read-only (get-donation (donation-id uint))
  (map-get? donations donation-id)
)

(define-read-only (get-volunteer (volunteer-id uint))
  (map-get? volunteers volunteer-id)
)

(define-read-only (get-store-stat (stat-name (string-ascii 20)))
  (default-to u0 (map-get? store-stats stat-name))
)

(define-read-only (get-next-item-id)
  (var-get next-item-id)
)

(define-read-only (get-next-donation-id)
  (var-get next-donation-id)
)

(define-read-only (get-next-volunteer-id)
  (var-get next-volunteer-id)
)

(define-read-only (is-contract-owner (user principal))
  (is-eq user CONTRACT_OWNER)
)

;; private functions
(define-private (calculate-item-value (price uint) (quantity uint))
  (* price quantity)
)
