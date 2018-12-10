#lang rosette

(require rosette/lib/synthax)

(current-bitwidth 8)

(define (set-subleq regs r1 r2)
  (vector-set! regs r2 (bvsub (vector-ref regs r2) (vector-ref regs r1)))
  )

(define (loop-template a b regs)
  (vector-set! regs 1 a)
  (vector-set! regs 2 b)
  (define line10 (?? boolean?))
  (define line11 #f) ;;(?? boolean?))
  (define line12 #f) ;;(?? boolean?))
  (define line13 #f) ;;(?? boolean?))
  (define line3 #t) ;;(?? boolean?))
  (define line4 #t) ;;(?? boolean?))
  (define r1_0 (choose 0 1 2 3 4 5 6 7 8 9))
  (define r2_0 (choose 0 3 4 5 6 7 8 9))
  (define r1_10 (choose 0 1 2 3 4 5 6 7 8 9))
  (define r2_10 (choose 0 3 4 5 6 7 8 9))
  (define r1_11 (choose 0 1 2 3 4 5 6 7 8 9))
  (define r2_11 (choose 0 3 4 5 6 7 8 9))
  (define r1_12 (choose 0 1 2 3 4 5 6 7 8 9))
  (define r2_12 (choose 0 3 4 5 6 7 8 9))
  (define r1_13 (choose 0 1 2 3 4 5 6 7 8 9))
  (define r2_13 (choose 0 3 4 5 6 7 8 9))
  (define r1_2 (choose 0 1 2 3 4 5 6 7 8 9))
  (define r2_2 (choose 0 3 4 5 6 7 8 9))
  (define r1_3 (choose 0 1 2 3 4 5 6 7 8 9))
  (define r2_3 (choose 0 3 4 5 6 7 8 9))
  (define r1_4 (choose 0 1 2 3 4 5 6 7 8 9))
  (define r2_4 (choose 0 3 4 5 6 7 8 9))
  (define r1_5 (choose 0 1 2 3 4 5 6 7 8 9))
  (define r2_5 (choose 0 3 4 5 6 7 8 9))
  ;; L0
  (set-subleq regs r1_0 r2_0)
  ;; L1_0
  (cond [line10 (set-subleq regs r1_10 r2_10)])
  ;; L1_1
  (cond [line11 (set-subleq regs r1_11 r2_11)])
  ;; L1_2
  (cond [line12 (set-subleq regs r1_12 r2_12)])
  ;; L1_3
  (cond [line13 (set-subleq regs r1_13 r2_13)])
  ;; L2
  (set-subleq regs r1_2 r2_2)
  ;; L3
  (cond [line3 (set-subleq regs r1_3 r2_3)])
  ;; L4
  (cond [line4 (set-subleq regs r1_4 r2_4)])
  ;; L5
  (set-subleq regs r1_5 r2_5)
  (cond
    [(bvsgt (vector-ref regs r2_5) (bv 0 8))
     ;; Exit
     (vector-ref regs 0)]
    [else
     ;; L2
     (set-subleq regs r1_2 r2_2)
     ;; L3
     (cond [line3 (set-subleq regs r1_3 r2_3)])
     ;; L4
     (cond [line4 (set-subleq regs r1_4 r2_4)])
     ;; L5
     (set-subleq regs r1_5 r2_5)
     (cond
       [(bvsgt (vector-ref regs r2_5) (bv 0 8))
        ;; Exit
        (vector-ref regs 0)]
       [else
        ;; L2
        (set-subleq regs r1_2 r2_2)
        ;; L3
        (cond [line3 (set-subleq regs r1_3 r2_3)])
        ;; L4
        (cond [line4 (set-subleq regs r1_4 r2_4)])
        ;; L5
        (set-subleq regs r1_5 r2_5)
        (cond
          [(bvsgt (vector-ref regs r2_5) (bv 0 8))
           ;; Exit
           (vector-ref regs 0)]
          [else
           ;; L2
           (set-subleq regs r1_2 r2_2)
           ;; L3
           (cond [line3 (set-subleq regs r1_3 r2_3)])
           ;; L4
           (cond [line4 (set-subleq regs r1_4 r2_4)])
           ;; L5
           (set-subleq regs r1_5 r2_5)
           (cond
             [(bvsgt (vector-ref regs r2_5) (bv 0 8))
              ;; Exit
              (vector-ref regs 0)]
             [else
              ;; L2
              (set-subleq regs r1_2 r2_2)
              ;; L3
              (cond [line3 (set-subleq regs r1_3 r2_3)])
              ;; L4
              (cond [line4 (set-subleq regs r1_4 r2_4)])
              ;; L5
              (set-subleq regs r1_5 r2_5)
              (cond
                [(bvsgt (vector-ref regs r2_5) (bv 0 8))
                 ;; Exit
                 (vector-ref regs 0)]
                [else
                 ;; L2
                 (set-subleq regs r1_2 r2_2)
                 ;; L3
                 (cond [line3 (set-subleq regs r1_3 r2_3)])
                 ;; L4
                 (cond [line4 (set-subleq regs r1_4 r2_4)])
                 ;; L5
                 (set-subleq regs r1_5 r2_5)
                 ;; Exit
                 (vector-ref regs 0)
                 ])
              ])
           ])
        ])
     ])
  )

(define (simple-mult a b regs)
  (if (bvsle b (bv 5 8)) (bvmul a b) (bvmul a (bv 6 8)))
  )

(define (same-mult original example a b)
  ;; (define b (bv 2 8))
  (define regs (make-vector 10 (bv 0 8)))
  (vector-set! regs 9 (bv 1 8))
  (assert (bveq (original a b regs) (example a b regs)))
  )
;; (define line10 #f)
;; (define line11 #t)
;; (define line12 #f)
;; (define line13 #f)
;; (define line3 #t)
;; (define line4 #t)
;; (define r1_0 1)
;; (define r2_0 3)
;; (define r1_10 (choose 0 1 2 3 4 5 6 7 8 9))
;; (define r2_10 (choose 0 3 4 5 6 7 8 9))
;; (define r1_11 2)
;; (define r2_11 6)
;; (define r1_12 (choose 0 1 2 3 4 5 6 7 8 9))
;; (define r2_12 (choose 0 3 4 5 6 7 8 9))
;; (define r1_13 (choose 0 1 2 3 4 5 6 7 8 9))
;; (define r2_13 (choose 0 3 4 5 6 7 8 9))
;; (define r1_2 2)
;; (define r2_2 5)
;; (define r1_3 3)
;; (define r2_3 0)
;; (define r1_4 5)
;; (define r2_4 7)
;; (define r1_5 7)
;; (define r2_5 5)

;; Synthesis examples
(define-symbolic a (bitvector 8))
(define-symbolic b (bitvector 8))

(define sol
  (synthesize #:forall (list a b)
              #:guarantee (same-mult simple-mult loop-template a b)))

(print-forms sol)

