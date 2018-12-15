#lang rosette

(require rosette/lib/synthax)

(current-bitwidth 8)

(define (set-subleq regs r1 r2)
  (vector-set! regs r2 (bvsub (vector-ref regs r2) (vector-ref regs r1)))
  )

(define (branch-template a b c d next label1 regs)
  ;; regs[0] is Z (always 0)
  (vector-set! regs 1 a)
  (vector-set! regs 2 b)
  (vector-set! regs 3 c)
  (vector-set! regs 4 d)
  (define line0 (?? boolean?))
  (define line1 (?? boolean?))
  (define line2 (?? boolean?))
  (define jump3 (choose 4 5 6 7 8 9))
  (define jump4 (choose 5 6 7 8 9))
  (define jump5 (choose 6 7 8 9))
  (define jump6 (choose 7 8 9))
  (define jump7 (choose 8 9))
  (define r1_0 (choose 0 1 2 3 4 5 6 7 8 9 10 11 12 13))
  (define r2_0 (choose 0 1 2 3 4 5 6 7 8 9 10 11 12 13))
  (define r1_1 (choose 0 1 2 3 4 5 6 7 8 9 10 11 12 13))
  (define r2_1 (choose 0 1 2 3 4 5 6 7 8 9 10 11 12 13))
  (define r1_2 (choose 0 1 2 3 4 5 6 7 8 9 10 11 12 13))
  (define r2_2 (choose 0 1 2 3 4 5 6 7 8 9 10 11 12 13))
  (define r1_3 (choose 0 1 2 3 4 5 6 7 8 9 10 11 12 13))
  (define r2_3 (choose 0 1 2 3 4 5 6 7 8 9 10 11 12 13))
  (define r1_4 (choose 0 1 2 3 4 5 6 7 8 9 10 11 12 13))
  (define r2_4 (choose 0 1 2 3 4 5 6 7 8 9 10 11 12 13))
  (define r1_5 (choose 0 1 2 3 4 5 6 7 8 9 10 11 12 13))
  (define r2_5 (choose 0 1 2 3 4 5 6 7 8 9 10 11 12 13))
  (define r1_6 (choose 0 1 2 3 4 5 6 7 8 9 10 11 12 13))
  (define r2_6 (choose 0 1 2 3 4 5 6 7 8 9 10 11 12 13))
  (define r1_7 (choose 0 1 2 3 4 5 6 7 8 9 10 11 12 13))
  (define r2_7 (choose 0 1 2 3 4 5 6 7 8 9 10 11 12 13))
  ;; L0
  (cond [line0 (set-subleq regs r1_0 r2_0)])
  ;; L1
  (cond [line1 (set-subleq regs r1_1 r2_1)])
  ;; L2
  (cond [line2 (set-subleq regs r1_2 r2_2)])
  ;; L3
  (set-subleq regs r1_3 r2_3)
  (cond
    [(bvsgt (vector-ref regs r2_3) (bv 0 8))
     ;; L4
     (set-subleq regs r1_4 r2_4)
     (cond
       [(bvsgt (vector-ref regs r2_4) (bv 0 8))
        ;; L5
        (set-subleq regs r1_5 r2_5)
        (cond
          [(bvsgt (vector-ref regs r2_5) (bv 0 8))
           ;; L6
           (set-subleq regs r1_6 r2_6)
           (cond
             [(bvsgt (vector-ref regs r2_6) (bv 0 8))
              ;; L7
              (set-subleq regs r1_7 r2_7)
              (cond
                [(bvsgt (vector-ref regs r2_7) (bv 0 8))
                 next]
                [else
                 (cond
                   [(equal? jump7 8)
                    next]
                   [else
                    label1])
                 ])
              ]
             [else
              ;; L6 -> L7 || next || label1
              (cond
                ;; L7
                [(equal? jump6 7)
                 (set-subleq regs r1_7 r2_7)
                 (cond
                   [(bvsgt (vector-ref regs r2_7) (bv 0 8))
                    next]
                   [else
                    (cond
                      [(equal? jump7 8)
                       next]
                      [else
                       label1])
                    ])]
                ;; next
                [(equal? jump6 8) next]
                ;; label1
                [else label1])
              ])
           ]
          [else
           ;; L5 -> L6 || L7 || next || label1
           (cond
             ;; L6
             [(equal? jump5 6)
              (set-subleq regs r1_6 r2_6)
              (cond
                [(bvsgt (vector-ref regs r2_6) (bv 0 8))
                 ;; L7
                 (set-subleq regs r1_7 r2_7)
                 (cond
                   [(bvsgt (vector-ref regs r2_7) (bv 0 8))
                    next]
                   [else
                    (cond
                      [(equal? jump7 8)
                       next]
                      [else
                       label1])
                    ])]
                [else
                 ;; L6 -> L7 || next || label1
                 (cond
                   ;; L7
                   [(equal? jump6 7)
                    (set-subleq regs r1_7 r2_7)
                    (cond
                      [(bvsgt (vector-ref regs r2_7) (bv 0 8))
                       next]
                      [else
                       (cond
                         [(equal? jump7 8)
                          next]
                         [else
                          label1])
                       ])]
                   ;; next
                   [(equal? jump6 8) next]
                   ;; label1
                   [else label1])
                 ])]
             ;; L7
             [(equal? jump5 7)
              (set-subleq regs r1_7 r2_7)
              (cond
                [(bvsgt (vector-ref regs r2_7) (bv 0 8))
                 next]
                [else
                 (cond
                   [(equal? jump7 8)
                    next]
                   [else
                    label1])
                 ])]
             ;; next
             [(equal? jump5 8) next]
             ;; label1
             [else label1])
           ])
        ]
       [else
        ;; L4 -> L5 || L6 || L7 || next || label1
        (cond
          ;; L5
          [(equal? jump4 5)
           (set-subleq regs r1_5 r2_5)
           (cond
             [(bvsgt (vector-ref regs r2_5) (bv 0 8))
              ;; L6
              (set-subleq regs r1_6 r2_6)
              (cond
                [(bvsgt (vector-ref regs r2_6) (bv 0 8))
                 ;; L7
                 (set-subleq regs r1_7 r2_7)
                 (cond
                   [(bvsgt (vector-ref regs r2_7) (bv 0 8))
                    next]
                   [else
                    (cond
                      [(equal? jump7 8)
                       next]
                      [else
                       label1])
                    ])
                 ]
                [else
                 ;; L6 -> L7 || next || label1
                 (cond
                   ;; L7
                   [(equal? jump6 7)
                    (set-subleq regs r1_7 r2_7)
                    (cond
                      [(bvsgt (vector-ref regs r2_7) (bv 0 8))
                       next]
                      [else
                       (cond
                         [(equal? jump7 8)
                          next]
                         [else
                          label1])
                       ])]
                   ;; next
                   [(equal? jump6 8) next]
                   ;; label1
                   [else label1])
                 ])
              ]
             [else
              ;; L5 -> L6 || L7 || next || label1
              (cond
                ;; L6
                [(equal? jump5 6)
                 (set-subleq regs r1_6 r2_6)
                 (cond
                   [(bvsgt (vector-ref regs r2_6) (bv 0 8))
                    ;; L7
                    (set-subleq regs r1_7 r2_7)
                    (cond
                      [(bvsgt (vector-ref regs r2_7) (bv 0 8))
                       next]
                      [else
                       (cond
                         [(equal? jump7 8)
                          next]
                         [else
                          label1])
                       ])]
                   [else
                    ;; L6 -> L7 || next || label1
                    (cond
                      ;; L7
                      [(equal? jump6 7)
                       (set-subleq regs r1_7 r2_7)
                       (cond
                         [(bvsgt (vector-ref regs r2_7) (bv 0 8))
                          next]
                         [else
                          (cond
                            [(equal? jump7 8)
                             next]
                            [else
                             label1])
                          ])]
                      ;; next
                      [(equal? jump6 8) next]
                      ;; label1
                      [else label1])
                    ])]
                ;; L7
                [(equal? jump5 7)
                 (set-subleq regs r1_7 r2_7)
                 (cond
                   [(bvsgt (vector-ref regs r2_7) (bv 0 8))
                    next]
                   [else
                    (cond
                      [(equal? jump7 8)
                       next]
                      [else
                       label1])
                    ])]
                ;; next
                [(equal? jump5 8) next]
                ;; label1
                [else label1])
              ])
           ]
          ;; L6
          [(equal? jump4 6)
           (set-subleq regs r1_6 r2_6)
           (cond
             [(bvsgt (vector-ref regs r2_6) (bv 0 8))
              ;; L7
              (set-subleq regs r1_7 r2_7)
              (cond
                [(bvsgt (vector-ref regs r2_7) (bv 0 8))
                 next]
                [else
                 (cond
                   [(equal? jump7 8)
                    next]
                   [else
                    label1])
                 ])
              ]
             [else
              ;; L6 -> L7 || next || label1
              (cond
                ;; L7
                [(equal? jump6 7)
                 (set-subleq regs r1_7 r2_7)
                 (cond
                   [(bvsgt (vector-ref regs r2_7) (bv 0 8))
                    next]
                   [else
                    (cond
                      [(equal? jump7 8)
                       next]
                      [else
                       label1])
                    ])]
                ;; next
                [(equal? jump6 8) next]
                ;; label1
                [else label1])
              ])
           ]
          ;; L7
          [(equal? jump4 7)
           (set-subleq regs r1_7 r2_7)
           (cond
             [(bvsgt (vector-ref regs r2_7) (bv 0 8))
              next]
             [else
              (cond
                [(equal? jump7 8)
                 next]
                [else
                 label1])
              ])]
          ;; next
          [(equal? jump4 8) next]
          ;; label1
          [else label1])
        ])
     ]
    [else
     ;; L3 -> L4 || L5 || L6 || L7 || next || label1
     (cond
       ;; L4
       [(equal? jump3 4)
        (set-subleq regs r1_4 r2_4)
        (cond
          [(bvsgt (vector-ref regs r2_4) (bv 0 8))
           ;; L5
           (set-subleq regs r1_5 r2_5)
           (cond
             [(bvsgt (vector-ref regs r2_5) (bv 0 8))
              ;; L6
              (set-subleq regs r1_6 r2_6)
              (cond
                [(bvsgt (vector-ref regs r2_6) (bv 0 8))
                 ;; L7
                 (set-subleq regs r1_7 r2_7)
                 (cond
                   [(bvsgt (vector-ref regs r2_7) (bv 0 8))
                    next]
                   [else
                    (cond
                      [(equal? jump7 8)
                       next]
                      [else
                       label1])
                    ])
                 ]
                [else
                 ;; L6 -> L7 || next || label1
                 (cond
                   ;; L7
                   [(equal? jump6 7)
                    (set-subleq regs r1_7 r2_7)
                    (cond
                      [(bvsgt (vector-ref regs r2_7) (bv 0 8))
                       next]
                      [else
                       (cond
                         [(equal? jump7 8)
                          next]
                         [else
                          label1])
                       ])]
                   ;; next
                   [(equal? jump6 8) next]
                   ;; label1
                   [else label1])
                 ])
              ]
             [else
              ;; L5 -> L6 || L7 || next || label1
              (cond
                ;; L6
                [(equal? jump5 6)
                 (set-subleq regs r1_6 r2_6)
                 (cond
                   [(bvsgt (vector-ref regs r2_6) (bv 0 8))
                    ;; L7
                    (set-subleq regs r1_7 r2_7)
                    (cond
                      [(bvsgt (vector-ref regs r2_7) (bv 0 8))
                       next]
                      [else
                       (cond
                         [(equal? jump7 8)
                          next]
                         [else
                          label1])
                       ])]
                   [else
                    ;; L6 -> L7 || next || label1
                    (cond
                      ;; L7
                      [(equal? jump6 7)
                       (set-subleq regs r1_7 r2_7)
                       (cond
                         [(bvsgt (vector-ref regs r2_7) (bv 0 8))
                          next]
                         [else
                          (cond
                            [(equal? jump7 8)
                             next]
                            [else
                             label1])
                          ])]
                      ;; next
                      [(equal? jump6 8) next]
                      ;; label1
                      [else label1])
                    ])]
                ;; L7
                [(equal? jump5 7)
                 (set-subleq regs r1_7 r2_7)
                 (cond
                   [(bvsgt (vector-ref regs r2_7) (bv 0 8))
                    next]
                   [else
                    (cond
                      [(equal? jump7 8)
                       next]
                      [else
                       label1])
                    ])]
                ;; next
                [(equal? jump5 8) next]
                ;; label1
                [else label1])
              ])
           ]
          [else
           ;; L4 -> L5 || L6 || L7 || next || label1
           (cond
             ;; L5
             [(equal? jump4 5)
              (set-subleq regs r1_5 r2_5)
              (cond
                [(bvsgt (vector-ref regs r2_5) (bv 0 8))
                 ;; L6
                 (set-subleq regs r1_6 r2_6)
                 (cond
                   [(bvsgt (vector-ref regs r2_6) (bv 0 8))
                    ;; L7
                    (set-subleq regs r1_7 r2_7)
                    (cond
                      [(bvsgt (vector-ref regs r2_7) (bv 0 8))
                       next]
                      [else
                       (cond
                         [(equal? jump7 8)
                          next]
                         [else
                          label1])
                       ])
                    ]
                   [else
                    ;; L6 -> L7 || next || label1
                    (cond
                      ;; L7
                      [(equal? jump6 7)
                       (set-subleq regs r1_7 r2_7)
                       (cond
                         [(bvsgt (vector-ref regs r2_7) (bv 0 8))
                          next]
                         [else
                          (cond
                            [(equal? jump7 8)
                             next]
                            [else
                             label1])
                          ])]
                      ;; next
                      [(equal? jump6 8) next]
                      ;; label1
                      [else label1])
                    ])
                 ]
                [else
                 ;; L5 -> L6 || L7 || next || label1
                 (cond
                   ;; L6
                   [(equal? jump5 6)
                    (set-subleq regs r1_6 r2_6)
                    (cond
                      [(bvsgt (vector-ref regs r2_6) (bv 0 8))
                       ;; L7
                       (set-subleq regs r1_7 r2_7)
                       (cond
                         [(bvsgt (vector-ref regs r2_7) (bv 0 8))
                          next]
                         [else
                          (cond
                            [(equal? jump7 8)
                             next]
                            [else
                             label1])
                          ])]
                      [else
                       ;; L6 -> L7 || next || label1
                       (cond
                         ;; L7
                         [(equal? jump6 7)
                          (set-subleq regs r1_7 r2_7)
                          (cond
                            [(bvsgt (vector-ref regs r2_7) (bv 0 8))
                             next]
                            [else
                             (cond
                               [(equal? jump7 8)
                                next]
                               [else
                                label1])
                             ])]
                         ;; next
                         [(equal? jump6 8) next]
                         ;; label1
                         [else label1])
                       ])]
                   ;; L7
                   [(equal? jump5 7)
                    (set-subleq regs r1_7 r2_7)
                    (cond
                      [(bvsgt (vector-ref regs r2_7) (bv 0 8))
                       next]
                      [else
                       (cond
                         [(equal? jump7 8)
                          next]
                         [else
                          label1])
                       ])]
                   ;; next
                   [(equal? jump5 8) next]
                   ;; label1
                   [else label1])
                 ])
              ]
             ;; L6
             [(equal? jump4 6)
              (set-subleq regs r1_6 r2_6)
              (cond
                [(bvsgt (vector-ref regs r2_6) (bv 0 8))
                 ;; L7
                 (set-subleq regs r1_7 r2_7)
                 (cond
                   [(bvsgt (vector-ref regs r2_7) (bv 0 8))
                    next]
                   [else
                    (cond
                      [(equal? jump7 8)
                       next]
                      [else
                       label1])
                    ])
                 ]
                [else
                 ;; L6 -> L7 || next || label1
                 (cond
                   ;; L7
                   [(equal? jump6 7)
                    (set-subleq regs r1_7 r2_7)
                    (cond
                      [(bvsgt (vector-ref regs r2_7) (bv 0 8))
                       next]
                      [else
                       (cond
                         [(equal? jump7 8)
                          next]
                         [else
                          label1])
                       ])]
                   ;; next
                   [(equal? jump6 8) next]
                   ;; label1
                   [else label1])
                 ])
              ]
             ;; L7
             [(equal? jump4 7)
              (set-subleq regs r1_7 r2_7)
              (cond
                [(bvsgt (vector-ref regs r2_7) (bv 0 8))
                 next]
                [else
                 (cond
                   [(equal? jump7 8)
                    next]
                   [else
                    label1])
                 ])]
             ;; next
             [(equal? jump4 8) next]
             ;; label1
             [else label1])
           ])
        ]
       ;; L5
       [(equal? jump3 5)
        (set-subleq regs r1_5 r2_5)
        (cond
          [(bvsgt (vector-ref regs r2_5) (bv 0 8))
           ;; L6
           (set-subleq regs r1_6 r2_6)
           (cond
             [(bvsgt (vector-ref regs r2_6) (bv 0 8))
              ;; L7
              (set-subleq regs r1_7 r2_7)
              (cond
                [(bvsgt (vector-ref regs r2_7) (bv 0 8))
                 next]
                [else
                 (cond
                   [(equal? jump7 8)
                    next]
                   [else
                    label1])
                 ])
              ]
             [else
              ;; L6 -> L7 || next || label1
              (cond
                ;; next
                [(equal? jump6 8) next]
                ;; label1
                [else label1])
              ])
           ]
          [else
           ;; L5 -> L6 || L7 || next || label1
           (cond
             ;; L6
             [(equal? jump5 6)
              (set-subleq regs r1_6 r2_6)
              (cond
                [(bvsgt (vector-ref regs r2_6) (bv 0 8))
                 ;; L7
                 (set-subleq regs r1_7 r2_7)
                 (cond
                   [(bvsgt (vector-ref regs r2_7) (bv 0 8))
                    next]
                   [else
                    (cond
                      [(equal? jump7 8)
                       next]
                      [else
                       label1])
                    ])]
                [else
                 ;; L6 -> L7 || next || label1
                 (cond
                   ;; L7
                   [(equal? jump6 7)
                    (set-subleq regs r1_7 r2_7)
                    (cond
                      [(bvsgt (vector-ref regs r2_7) (bv 0 8))
                       next]
                      [else
                       (cond
                         [(equal? jump7 8)
                          next]
                         [else
                          label1])
                       ])]
                   ;; next
                   [(equal? jump6 8) next]
                   ;; label1
                   [else label1])
                 ])]
             ;; L7
             [(equal? jump5 7)
              (set-subleq regs r1_7 r2_7)
              (cond
                [(bvsgt (vector-ref regs r2_7) (bv 0 8))
                 next]
                [else
                 (cond
                   [(equal? jump7 8)
                    next]
                   [else
                    label1])
                 ])]
             ;; next
             [(equal? jump5 8) next]
             ;; label1
             [else label1])
           ])
        ]
       ;; L6
       [(equal? jump3 6)
        (set-subleq regs r1_6 r2_6)
        (cond
          [(bvsgt (vector-ref regs r2_6) (bv 0 8))
           ;; L7
           (set-subleq regs r1_7 r2_7)
           (cond
             [(bvsgt (vector-ref regs r2_7) (bv 0 8))
              next]
             [else
              (cond
                [(equal? jump7 8)
                 next]
                [else
                 label1])
              ])
           ]
          [else
           ;; L6 -> L7 || next || label1
           (cond
             ;; L7
             [(equal? jump6 7)
              (set-subleq regs r1_7 r2_7)
              (cond
                [(bvsgt (vector-ref regs r2_7) (bv 0 8))
                 next]
                [else
                 (cond
                   [(equal? jump7 8)
                    next]
                   [else
                    label1])
                 ])]
             ;; next
             [(equal? jump6 8) next]
             ;; label1
             [else label1])
           ])
        ]
       ;; L7
       [(equal? jump3 7)
        (set-subleq regs r1_7 r2_7)
        (cond
          [(bvsgt (vector-ref regs r2_7) (bv 0 8))
           next]
          [else
           (cond
             [(equal? jump7 8)
              next]
             [else
              label1])
           ])]
       ;; next
       [(equal? jump3 8) next]
       ;; label1
       [else label1])
     ])
  )

;; (println (branch-template (bv 1 8) (bv 42 8) (bv 17 8) (vector (bv 0 8) (bv 0 8) (bv 0 8) (bv 0 8) (bv 0 8) (bv 0 8) (bv 0 8) (bv 0 8) (bv -1 8) (bv 1 8))))

(define (simple-beqz a L1 L2 regs)
  (if (bveq a (bv 0 8)) L1 L2)
  )

(define (simple-bne a b L1 L2 regs)
  (if (bveq a b) L1 L2)
  )

(define (two-adds-bne a b c d L1 L2 regs)
  (if (bveq (bvadd a b) (bvadd c b)) L1 L2)
  )

(define (same-beqz original example a next label1)
  (define regs (make-vector 10 (bv 0 8)))
  (vector-set! regs 9 (bv 1 8))
  (assert (bveq (original a next label1 regs) (example a next label1 regs)))
  )

(define (same-bne original example a b next label1)
  (define regs (make-vector 10 (bv 0 8)))
  (vector-set! regs 9 (bv 1 8))
  (assert (bveq (original a b next label1 regs) (example a b next label1 regs)))
  )

(define (same-tab original example a b c d next label1)
  (define regs (make-vector 14 (bv 0 8)))
  (vector-set! regs 13 (bv 1 8))
  (vector-set! regs 12 (bv -1 8))
  (assert (bveq (original a b c d next label1 regs) (example a b c d next label1 regs)))
  )
;; (define line1 #t)
;; (define line2 #t)
;; (define jump3 9)
;; (define jump4 5)
;; (define jump5 6)
;; (define jump6 8)
;; (define jump7 9)
;; (define r1_1 1)
;; (define r2_1 9)
;; (define r1_2 2)
;; (define r2_2 4)
;; (define r1_3 4)
;; (define r2_3 9)
;; (define r1_4 4)
;; (define r2_4 5)
;; (define r1_5 2)
;; (define r2_5 9)
;; (define r1_6 1)
;; (define r2_6 5)
;; (define r1_7 4)
;; (define r2_7 4)

;; Synthesis examples
(define-symbolic a (bitvector 8))
(define-symbolic b (bitvector 8))
(define-symbolic c (bitvector 8))
(define-symbolic d (bitvector 8))
(define-symbolic e (bitvector 8))
(define-symbolic f (bitvector 8))

(define sol
  (synthesize #:forall (list a b c d e f)
              #:guarantee (same-tab two-adds-bne branch-template a b c d e f)))

(print-forms sol)
