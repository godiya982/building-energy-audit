
;; title: Building Energy Audit System
;; version: 1.0.0
;; summary: Energy efficiency assessment platform with consumption analysis and retrofit tracking
;; description: Smart contract system for managing building energy audits, tracking consumption, and managing retrofit projects

;; constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-already-exists (err u102))
(define-constant err-invalid-data (err u103))
(define-constant err-unauthorized (err u104))

;; data vars
(define-data-var audit-counter uint u0)
(define-data-var retrofit-counter uint u0)

;; data maps
(define-map energy-audits
    uint
    {
        building-id: (string-ascii 50),
        auditor: principal,
        audit-date: uint,
        energy-rating: uint,
        annual-consumption: uint,
        efficiency-score: uint,
        recommendations: (string-ascii 500),
        status: (string-ascii 20)
    }
)

(define-map building-profiles
    (string-ascii 50)
    {
        owner: principal,
        building-type: (string-ascii 30),
        floor-area: uint,
        year-built: uint,
        location: (string-ascii 100),
        registered-date: uint
    }
)

(define-map consumption-data
    { building-id: (string-ascii 50), period: (string-ascii 20) }
    {
        electricity-usage: uint,
        gas-usage: uint,
        water-usage: uint,
        cost: uint,
        recorded-date: uint
    }
)

(define-map retrofit-projects
    uint
    {
        building-id: (string-ascii 50),
        project-type: (string-ascii 50),
        contractor: principal,
        estimated-cost: uint,
        actual-cost: uint,
        start-date: uint,
        completion-date: uint,
        energy-savings-target: uint,
        status: (string-ascii 20)
    }
)

(define-map authorized-auditors principal bool)

;; public functions
(define-public (register-building (building-id (string-ascii 50)) (building-type (string-ascii 30)) 
                                 (floor-area uint) (year-built uint) (location (string-ascii 100)))
    (let ((existing-building (map-get? building-profiles building-id)))
        (if (is-some existing-building)
            err-already-exists
            (begin
                (map-set building-profiles building-id {
                    owner: tx-sender,
                    building-type: building-type,
                    floor-area: floor-area,
                    year-built: year-built,
                    location: location,
                    registered-date: stacks-block-height
                })
                (ok building-id)
            )
        )
    )
)

(define-public (authorize-auditor (auditor principal))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (map-set authorized-auditors auditor true)
        (ok auditor)
    )
)

(define-public (create-energy-audit (building-id (string-ascii 50)) (energy-rating uint) 
                                   (annual-consumption uint) (efficiency-score uint) 
                                   (recommendations (string-ascii 500)))
    (let (
        (audit-id (+ (var-get audit-counter) u1))
        (building-exists (is-some (map-get? building-profiles building-id)))
        (is-authorized (default-to false (map-get? authorized-auditors tx-sender)))
    )
        (asserts! building-exists err-not-found)
        (asserts! is-authorized err-unauthorized)
        (asserts! (and (>= energy-rating u1) (<= energy-rating u10)) err-invalid-data)
        (asserts! (and (>= efficiency-score u0) (<= efficiency-score u100)) err-invalid-data)
        
        (map-set energy-audits audit-id {
            building-id: building-id,
            auditor: tx-sender,
            audit-date: stacks-block-height,
            energy-rating: energy-rating,
            annual-consumption: annual-consumption,
            efficiency-score: efficiency-score,
            recommendations: recommendations,
            status: "completed"
        })
        (var-set audit-counter audit-id)
        (ok audit-id)
    )
)

(define-public (record-consumption (building-id (string-ascii 50)) (period (string-ascii 20))
                                  (electricity-usage uint) (gas-usage uint) 
                                  (water-usage uint) (cost uint))
    (let (
        (building-profile (map-get? building-profiles building-id))
        (building-owner (get owner (unwrap! building-profile err-not-found)))
    )
        (asserts! (is-eq tx-sender building-owner) err-unauthorized)
        (map-set consumption-data { building-id: building-id, period: period } {
            electricity-usage: electricity-usage,
            gas-usage: gas-usage,
            water-usage: water-usage,
            cost: cost,
            recorded-date: stacks-block-height
        })
        (ok true)
    )
)

(define-public (create-retrofit-project (building-id (string-ascii 50)) (project-type (string-ascii 50))
                                       (contractor principal) (estimated-cost uint) 
                                       (energy-savings-target uint))
    (let (
        (project-id (+ (var-get retrofit-counter) u1))
        (building-profile (map-get? building-profiles building-id))
        (building-owner (get owner (unwrap! building-profile err-not-found)))
    )
        (asserts! (is-eq tx-sender building-owner) err-unauthorized)
        (map-set retrofit-projects project-id {
            building-id: building-id,
            project-type: project-type,
            contractor: contractor,
            estimated-cost: estimated-cost,
            actual-cost: u0,
            start-date: u0,
            completion-date: u0,
            energy-savings-target: energy-savings-target,
            status: "planned"
        })
        (var-set retrofit-counter project-id)
        (ok project-id)
    )
)

(define-public (update-retrofit-status (project-id uint) (status (string-ascii 20)) 
                                      (actual-cost uint) (completion-date uint))
    (let (
        (project (unwrap! (map-get? retrofit-projects project-id) err-not-found))
        (building-id (get building-id project))
        (building-profile (unwrap! (map-get? building-profiles building-id) err-not-found))
        (building-owner (get owner building-profile))
    )
        (asserts! (is-eq tx-sender building-owner) err-unauthorized)
        (map-set retrofit-projects project-id (merge project {
            actual-cost: actual-cost,
            completion-date: completion-date,
            status: status
        }))
        (ok true)
    )
)

;; read only functions
(define-read-only (get-building-profile (building-id (string-ascii 50)))
    (map-get? building-profiles building-id)
)

(define-read-only (get-energy-audit (audit-id uint))
    (map-get? energy-audits audit-id)
)

(define-read-only (get-consumption-data (building-id (string-ascii 50)) (period (string-ascii 20)))
    (map-get? consumption-data { building-id: building-id, period: period })
)

(define-read-only (get-retrofit-project (project-id uint))
    (map-get? retrofit-projects project-id)
)

(define-read-only (is-authorized-auditor (auditor principal))
    (default-to false (map-get? authorized-auditors auditor))
)

(define-read-only (get-audit-count)
    (var-get audit-counter)
)

(define-read-only (get-retrofit-count)
    (var-get retrofit-counter)
)

(define-read-only (calculate-efficiency-improvement (building-id (string-ascii 50)) (current-period (string-ascii 20)) (previous-period (string-ascii 20)))
    (let (
        (current-data (map-get? consumption-data { building-id: building-id, period: current-period }))
        (previous-data (map-get? consumption-data { building-id: building-id, period: previous-period }))
    )
        (match current-data
            current-consumption
            (match previous-data
                previous-consumption
                (let (
                    (current-total (+ (+ (get electricity-usage current-consumption) (get gas-usage current-consumption)) (get water-usage current-consumption)))
                    (previous-total (+ (+ (get electricity-usage previous-consumption) (get gas-usage previous-consumption)) (get water-usage previous-consumption)))
                )
                    (if (> previous-total u0)
                        (ok (/ (* (- previous-total current-total) u100) previous-total))
                        (ok u0)
                    )
                )
                (err u404)
            )
            (err u404)
        )
    )
)
