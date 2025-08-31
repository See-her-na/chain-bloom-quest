;; ChainBloom Quest - Living Digital Art Ecosystem
;; A symbiotic relationship between artistic creation and gameplay mechanics

;; Error Constants
(define-constant ERR_NOT_AUTHORIZED (err u1001))
(define-constant ERR_GARDEN_NOT_FOUND (err u1002))
(define-constant ERR_INSUFFICIENT_ENERGY (err u1003))
(define-constant ERR_INVALID_COORDINATES (err u1004))
(define-constant ERR_PLANT_NOT_FOUND (err u1005))
(define-constant ERR_GARDEN_ALREADY_EXISTS (err u1006))
(define-constant ERR_INVALID_PLANT_TYPE (err u1007))
(define-constant ERR_PLANT_NOT_MATURE (err u1008))
(define-constant ERR_INVALID_EVOLUTION_STAGE (err u1009))
(define-constant ERR_COOLDOWN_ACTIVE (err u1010))
(define-constant ERR_INVALID_AMOUNT (err u1011))
(define-constant ERR_GARDEN_FULL (err u1012))
(define-constant ERR_INVALID_SEED_TYPE (err u1013))

;; Contract Constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant MAX_GARDEN_SIZE u25)
(define-constant BASE_ENERGY_COST u10)
(define-constant EVOLUTION_THRESHOLD u100)
(define-constant NURTURE_COOLDOWN u144) ;; ~1 day in blocks

;; Data Variables
(define-data-var next-garden-id uint u1)
(define-data-var next-plant-id uint u1)
(define-data-var base-seed-price uint u1000)
(define-data-var evolution-multiplier uint u150)
(define-data-var contract-paused bool false)

;; Garden Structure
(define-map gardens 
  uint 
  {
    owner: principal,
    name: (string-ascii 64),
    size: uint,
    energy: uint,
    last-tended: uint,
    environment-type: (string-ascii 20),
    total-plants: uint,
    prosperity-score: uint
  })

;; Plant Structure with Evolution Data
(define-map plants 
  uint 
  {
    garden-id: uint,
    owner: principal,
    x-coord: uint,
    y-coord: uint,
    plant-type: (string-ascii 20),
    growth-stage: uint,
    evolution-points: uint,
    art-dna: (string-ascii 128),
    created-at: uint,
    last-evolved: uint,
    rarity-score: uint,
    interaction-count: uint
  })

;; Player Energy and Resources
(define-map player-resources 
  principal 
  {
    energy: uint,
    seeds: uint,
    evolution-tokens: uint,
    total-gardens: uint,
    last-energy-regen: uint,
    experience-points: uint
  })

;; Garden Plant Positions
(define-map garden-positions 
  {garden-id: uint, x: uint, y: uint} 
  uint) ;; plant-id

;; Plant Evolution History
(define-map evolution-history 
  uint 
  {
    previous-stage: uint,
    evolution-timestamp: uint,
    catalyst-used: (optional (string-ascii 20)),
    community-votes: uint
  })

;; Environmental Modifiers
(define-map environment-bonuses 
  (string-ascii 20) 
  {
    growth-multiplier: uint,
    energy-efficiency: uint,
    rarity-bonus: uint
  })

;; Owner Functions
(define-public (set-base-seed-price (new-price uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
    (asserts! (> new-price u0) ERR_INVALID_AMOUNT)
    (ok (var-set base-seed-price new-price))))

(define-public (set-evolution-multiplier (new-multiplier uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
    (asserts! (and (>= new-multiplier u100) (<= new-multiplier u300)) ERR_INVALID_AMOUNT)
    (ok (var-set evolution-multiplier new-multiplier))))

(define-public (pause-contract)
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
    (ok (var-set contract-paused true))))

(define-public (unpause-contract)
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
    (ok (var-set contract-paused false))))

(define-public (set-environment-bonus (env-type (string-ascii 20)) (growth-mult uint) (energy-eff uint) (rarity-bonus uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
    (ok (map-set environment-bonuses env-type {
      growth-multiplier: growth-mult,
      energy-efficiency: energy-eff,
      rarity-bonus: rarity-bonus
    }))))

;; Public Functions
(define-public (create-garden (garden-name (string-ascii 64)) (environment-type (string-ascii 20)))
  (let 
    (
      (garden-id (var-get next-garden-id))
      (current-resources (default-to 
        {energy: u100, seeds: u5, evolution-tokens: u0, total-gardens: u0, last-energy-regen: burn-block-height, experience-points: u0}
        (map-get? player-resources tx-sender)))
    )
    (asserts! (not (var-get contract-paused)) ERR_NOT_AUTHORIZED)
    (asserts! (>= (get energy current-resources) u50) ERR_INSUFFICIENT_ENERGY)
    
    ;; Create garden
    (map-set gardens garden-id {
      owner: tx-sender,
      name: garden-name,
      size: u5,
      energy: u100,
      last-tended: burn-block-height,
      environment-type: environment-type,
      total-plants: u0,
      prosperity-score: u0
    })
    
    ;; Update player resources
    (map-set player-resources tx-sender 
      (merge current-resources {
        energy: (- (get energy current-resources) u50),
        total-gardens: (+ (get total-gardens current-resources) u1)
      }))
    
    (var-set next-garden-id (+ garden-id u1))
    (ok garden-id)))

(define-public (plant-seed (garden-id uint) (x-coord uint) (y-coord uint) (seed-type (string-ascii 20)))
  (let 
    (
      (garden (unwrap! (map-get? gardens garden-id) ERR_GARDEN_NOT_FOUND))
      (plant-id (var-get next-plant-id))
      (current-resources (unwrap! (map-get? player-resources tx-sender) ERR_NOT_AUTHORIZED))
      (art-dna (generate-art-dna seed-type x-coord y-coord))
    )
    (asserts! (not (var-get contract-paused)) ERR_NOT_AUTHORIZED)
    (asserts! (is-eq (get owner garden) tx-sender) ERR_NOT_AUTHORIZED)
    (asserts! (>= (get seeds current-resources) u1) ERR_INSUFFICIENT_ENERGY)
    (asserts! (and (< x-coord u5) (< y-coord u5)) ERR_INVALID_COORDINATES)
    (asserts! (is-none (map-get? garden-positions {garden-id: garden-id, x: x-coord, y: y-coord})) ERR_INVALID_COORDINATES)
    (asserts! (< (get total-plants garden) MAX_GARDEN_SIZE) ERR_GARDEN_FULL)
    
    ;; Create plant
    (map-set plants plant-id {
      garden-id: garden-id,
      owner: tx-sender,
      x-coord: x-coord,
      y-coord: y-coord,
      plant-type: seed-type,
      growth-stage: u1,
      evolution-points: u0,
      art-dna: art-dna,
      created-at: burn-block-height,
      last-evolved: burn-block-height,
      rarity-score: u10,
      interaction-count: u0
    })
    
    ;; Set position
    (map-set garden-positions {garden-id: garden-id, x: x-coord, y: y-coord} plant-id)
    
    ;; Update garden
    (map-set gardens garden-id 
      (merge garden {total-plants: (+ (get total-plants garden) u1)}))
    
    ;; Update player resources
    (map-set player-resources tx-sender 
      (merge current-resources {seeds: (- (get seeds current-resources) u1)}))
    
    (var-set next-plant-id (+ plant-id u1))
    (ok plant-id)))

(define-public (nurture-plant (plant-id uint))
  (let 
    (
      (plant (unwrap! (map-get? plants plant-id) ERR_PLANT_NOT_FOUND))
      (garden (unwrap! (map-get? gardens (get garden-id plant)) ERR_GARDEN_NOT_FOUND))
      (current-resources (unwrap! (map-get? player-resources tx-sender) ERR_NOT_AUTHORIZED))
      (blocks-since-last (- burn-block-height (get last-evolved plant)))
    )
    (asserts! (not (var-get contract-paused)) ERR_NOT_AUTHORIZED)
    (asserts! (is-eq (get owner plant) tx-sender) ERR_NOT_AUTHORIZED)
    (asserts! (>= (get energy current-resources) BASE_ENERGY_COST) ERR_INSUFFICIENT_ENERGY)
    (asserts! (>= blocks-since-last NURTURE_COOLDOWN) ERR_COOLDOWN_ACTIVE)
    
    (let ((evolution-gain (calculate-evolution-gain plant garden)))
      ;; Update plant
      (map-set plants plant-id 
        (merge plant {
          evolution-points: (+ (get evolution-points plant) evolution-gain),
          last-evolved: burn-block-height,
          interaction-count: (+ (get interaction-count plant) u1)
        }))
      
      ;; Update player resources
      (map-set player-resources tx-sender 
        (merge current-resources {
          energy: (- (get energy current-resources) BASE_ENERGY_COST),
          experience-points: (+ (get experience-points current-resources) u5)
        }))
      
      ;; Update garden prosperity
      (map-set gardens (get garden-id plant)
        (merge garden {prosperity-score: (+ (get prosperity-score garden) u1)}))
      
      (ok evolution-gain))))

(define-public (evolve-plant (plant-id uint))
  (let 
    (
      (plant (unwrap! (map-get? plants plant-id) ERR_PLANT_NOT_FOUND))
      (current-resources (unwrap! (map-get? player-resources tx-sender) ERR_NOT_AUTHORIZED))
      (new-stage (+ (get growth-stage plant) u1))
      (evolution-cost (* (get growth-stage plant) EVOLUTION_THRESHOLD))
    )
    (asserts! (not (var-get contract-paused)) ERR_NOT_AUTHORIZED)