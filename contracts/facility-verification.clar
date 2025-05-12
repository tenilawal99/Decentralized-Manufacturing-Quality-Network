;; Facility Verification Contract
;; Validates production sites in the manufacturing network

(define-data-var admin principal tx-sender)

;; Facility status: 0 = unverified, 1 = pending, 2 = verified, 3 = suspended
(define-map facilities
  { facility-id: (string-ascii 64) }
  {
    owner: principal,
    name: (string-ascii 100),
    location: (string-ascii 100),
    status: uint,
    verification-date: uint,
    verifier: principal
  }
)

;; List of authorized verifiers
(define-map verifiers
  { verifier: principal }
  { authorized: bool }
)

;; Read-only function to check if a facility exists
(define-read-only (facility-exists (facility-id (string-ascii 64)))
  (is-some (map-get? facilities { facility-id: facility-id }))
)

;; Read-only function to get facility details
(define-read-only (get-facility (facility-id (string-ascii 64)))
  (map-get? facilities { facility-id: facility-id })
)

;; Read-only function to check if a principal is an authorized verifier
(define-read-only (is-verifier (verifier principal))
  (default-to false (get authorized (map-get? verifiers { verifier: verifier })))
)

;; Register a new facility (only unverified status)
(define-public (register-facility
    (facility-id (string-ascii 64))
    (name (string-ascii 100))
    (location (string-ascii 100)))
  (let ((sender tx-sender))
    (asserts! (not (facility-exists facility-id)) (err u1)) ;; Facility ID already exists
    (map-set facilities
      { facility-id: facility-id }
      {
        owner: sender,
        name: name,
        location: location,
        status: u1, ;; pending status
        verification-date: u0,
        verifier: sender
      }
    )
    (ok true)
  )
)

;; Verify a facility (only authorized verifiers)
(define-public (verify-facility (facility-id (string-ascii 64)))
  (let ((sender tx-sender))
    (asserts! (is-verifier sender) (err u2)) ;; Not an authorized verifier
    (asserts! (facility-exists facility-id) (err u3)) ;; Facility does not exist

    (let ((facility (unwrap! (get-facility facility-id) (err u4))))
      (map-set facilities
        { facility-id: facility-id }
        (merge facility {
          status: u2, ;; verified status
          verification-date: block-height,
          verifier: sender
        })
      )
      (ok true)
    )
  )
)

;; Suspend a facility (only admin)
(define-public (suspend-facility (facility-id (string-ascii 64)))
  (let ((sender tx-sender))
    (asserts! (is-eq sender (var-get admin)) (err u5)) ;; Not admin
    (asserts! (facility-exists facility-id) (err u3)) ;; Facility does not exist

    (let ((facility (unwrap! (get-facility facility-id) (err u4))))
      (map-set facilities
        { facility-id: facility-id }
        (merge facility {
          status: u3 ;; suspended status
        })
      )
      (ok true)
    )
  )
)

;; Add a verifier (only admin)
(define-public (add-verifier (verifier principal))
  (let ((sender tx-sender))
    (asserts! (is-eq sender (var-get admin)) (err u5)) ;; Not admin
    (map-set verifiers
      { verifier: verifier }
      { authorized: true }
    )
    (ok true)
  )
)

;; Remove a verifier (only admin)
(define-public (remove-verifier (verifier principal))
  (let ((sender tx-sender))
    (asserts! (is-eq sender (var-get admin)) (err u5)) ;; Not admin
    (map-set verifiers
      { verifier: verifier }
      { authorized: false }
    )
    (ok true)
  )
)

;; Transfer admin rights (only current admin)
(define-public (transfer-admin (new-admin principal))
  (let ((sender tx-sender))
    (asserts! (is-eq sender (var-get admin)) (err u5)) ;; Not admin
    (var-set admin new-admin)
    (ok true)
  )
)
