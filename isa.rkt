#lang rosette

(current-bitwidth 16)
(require rosette/lib/synthax)

(define (dynamic) (define-symbolic* a integer?) a)

(define (create-regs) (vector (dynamic) (dynamic) (dynamic) (dynamic) (dynamic)
                            (dynamic) (dynamic) (dynamic) (dynamic) (dynamic)))

(define (vector-cpy v1)
  (define len (vector-length v1))
  (define v2 (make-vector len 0))
  (for ([i len])
    (vector-set! v2 i (vector-ref v1 i)))
  v2)

(define (set-subleq regs r1 r2)
  (vector-set! regs r2 (- (vector-ref regs r2) (vector-ref regs r1)))
)

(define (simple-add a b regs)
  (+ a b)
)

(define (straightline-template a b regs)
  (vector-set! regs 1 a)
  (vector-set! regs 2 b)
  (define line1 (?? boolean?))
  (define line2 (?? boolean?))
  (define line3 (?? boolean?))
  (define line4 (?? boolean?))
  (define line5 (?? boolean?))
  (define line6 (?? boolean?))
  (define line7 (?? boolean?))
  (define r1_0 (choose 0 1 2 3 4 5 6 7 8 9))
  (define r2_0 (choose 0 3 4 5 6 7 8 9))
  (define r1_1 (choose 0 1 2 3 4 5 6 7 8 9))
  (define r2_1 (choose 0 3 4 5 6 7 8 9))
  (define r1_2 (choose 0 1 2 3 4 5 6 7 8 9))
  (define r2_2 (choose 0 3 4 5 6 7 8 9))
  (define r1_3 (choose 0 1 2 3 4 5 6 7 8 9))
  (define r2_3 (choose 0 3 4 5 6 7 8 9))
  (define r1_4 (choose 0 1 2 3 4 5 6 7 8 9))
  (define r2_4 (choose 0 3 4 5 6 7 8 9))
  (define r1_5 (choose 0 1 2 3 4 5 6 7 8 9))
  (define r2_5 (choose 0 3 4 5 6 7 8 9))
  (define r1_6 (choose 0 1 2 3 4 5 6 7 8 9))
  (define r2_6 (choose 0 3 4 5 6 7 8 9))
  (define r1_7 (choose 0 1 2 3 4 5 6 7 8 9))
  (define r2_7 (choose 0 3 4 5 6 7 8 9))
  ;; L0
  (set-subleq regs r1_0 r2_0)
  ;; L1
  (cond [line1 (set-subleq regs r1_1 r2_1)])
  ;; L2
  (cond [line2 (set-subleq regs r1_2 r2_2)])
  ;; L3
  (cond [line3 (set-subleq regs r1_3 r2_3)])
  ;; L4
  (cond [line4 (set-subleq regs r1_4 r2_4)])
  ;; L5
  (cond [line5 (set-subleq regs r1_5 r2_5)])
  ;; L6
  (cond [line6 (set-subleq regs r1_6 r2_6)])
  ;; L7
  (cond [line7 (set-subleq regs r1_7 r2_7)])
  (vector-ref regs 0)
  )

(define (subleq-add a b regs)
  (vector-set! regs 1 a)
  (vector-set! regs 2 b)
  ;; subleqi 3 3 1
  (define r1 (??))
  (define r2 (??))
  (set-subleq regs r1 r2)
  (cond
    [(positive? (vector-ref regs r2))
     ;; subleqi 1 3 2
     (set-subleq regs 1 3)
     (cond
       [(positive? (vector-ref regs 3))
        ;; subleqi 2 3 3
        (set-subleq regs 2 3)
        (cond
          [(positive? (vector-ref regs 3))
           ;; subleqi 0 0 4
           (set-subleq regs 0 0)
           (cond
             [(positive? (vector-ref regs 0))
              ;; subleqi 3 0 5
              (set-subleq regs 3 0)
              (cond
                [(positive? (vector-ref regs 0))
                 ;; goto 100
                 (vector-ref regs 0)
                 ]
                [else
                 ;; goto 100
                 (vector-ref regs 0)
                 ]
              )]
             [else
              ;; subleqi 3 0 5
              (set-subleq regs 3 0)
              (cond
                [(positive? (vector-ref regs 0))
                 ;; goto 100
                 (vector-ref regs 0)
                 ]
                [else
                 ;; goto 100
                 (vector-ref regs 0)
                 ]
              )]
           )]
          [else
           ;; subleqi 0 0 4
           (set-subleq regs 0 0)
           (cond
             [(positive? (vector-ref regs 0))
              ;; subleqi 3 0 5
              (set-subleq regs 3 0)
              (cond
                [(positive? (vector-ref regs 0))
                 ;; goto 100
                 (vector-ref regs 0)
                 ]
                [else
                 ;; goto 100
                 (vector-ref regs 0)
                 ]
                )]
             [else
              ;; subleqi 3 0 5
              (set-subleq regs 3 0)
              (cond
                [(positive? (vector-ref regs 0))
                 ;; goto 100
                 (vector-ref regs 0)
                 ]
                [else
                 ;; goto 100
                 (vector-ref regs 0)
                 ]
                )]
           )]
        )]
       [else
        ;; subleqi 2 3 3
        (set-subleq regs 2 3)
        (cond
          [(positive? (vector-ref regs 3))
           ;; subleqi 0 0 4
           (set-subleq regs 0 0)
           (cond
             [(positive? (vector-ref regs 0))
              ;; subleqi 3 0 5
              (set-subleq regs 3 0)
              (cond
                [(positive? (vector-ref regs 0))
                 ;; goto 100
                 (vector-ref regs 0)
                 ]
                [else
                 ;; goto 100
                 (vector-ref regs 0)
                 ]
              )]
             [else
              ;; subleqi 3 0 5
              (set-subleq regs 3 0)
              (cond
                [(positive? (vector-ref regs 0))
                 ;; goto 100
                 (vector-ref regs 0)
                 ]
                [else
                 ;; goto 100
                 (vector-ref regs 0)
                 ]
              )]
           )]
          [else
           ;; subleqi 0 0 4
           (set-subleq regs 0 0)
           (cond
             [(positive? (vector-ref regs 0))
              ;; subleqi 3 0 5
              (set-subleq regs 3 0)
              (cond
                [(positive? (vector-ref regs 0))
                 ;; goto 100
                 (vector-ref regs 0)
                 ]
                [else
                 ;; goto 100
                 (vector-ref regs 0)
                 ]
                )]
             [else
              ;; subleqi 3 0 5
              (set-subleq regs 3 0)
              (cond
                [(positive? (vector-ref regs 0))
                 ;; goto 100
                 (vector-ref regs 0)
                 ]
                [else
                 ;; goto 100
                 (vector-ref regs 0)
                 ]
                )]
           )]
        )]
     )]
    [else
     ;; subleqi 1 3 2
     (set-subleq regs 1 3)
     (cond
       [(positive? (vector-ref regs 3))
        ;; subleqi 2 3 3
        (set-subleq regs 2 3)
        (cond
          [(positive? (vector-ref regs 3))
           ;; subleqi 0 0 4
           (set-subleq regs 0 0)
           (cond
             [(positive? (vector-ref regs 0))
              ;; subleqi 3 0 5
              (set-subleq regs 3 0)
              (cond
                [(positive? (vector-ref regs 0))
                 ;; goto 100
                 (vector-ref regs 0)
                 ]
                [else
                 ;; goto 100
                 (vector-ref regs 0)
                 ]
              )]
             [else
              ;; subleqi 3 0 5
              (set-subleq regs 3 0)
              (cond
                [(positive? (vector-ref regs 0))
                 ;; goto 100
                 (vector-ref regs 0)
                 ]
                [else
                 ;; goto 100
                 (vector-ref regs 0)
                 ]
              )]
           )]
          [else
           ;; subleqi 0 0 4
           (set-subleq regs 0 0)
           (cond
             [(positive? (vector-ref regs 0))
              ;; subleqi 3 0 5
              (set-subleq regs 3 0)
              (cond
                [(positive? (vector-ref regs 0))
                 ;; goto 100
                 (vector-ref regs 0)
                 ]
                [else
                 ;; goto 100
                 (vector-ref regs 0)
                 ]
                )]
             [else
              ;; subleqi 3 0 5
              (set-subleq regs 3 0)
              (cond
                [(positive? (vector-ref regs 0))
                 ;; goto 100
                 (vector-ref regs 0)
                 ]
                [else
                 ;; goto 100
                 (vector-ref regs 0)
                 ]
                )]
           )]
        )]
       [else
        ;; subleqi 2 3 3
        (set-subleq regs 2 3)
        (cond
          [(positive? (vector-ref regs 3))
           ;; subleqi 0 0 4
           (set-subleq regs 0 0)
           (cond
             [(positive? (vector-ref regs 0))
              ;; subleqi 3 0 5
              (set-subleq regs 3 0)
              (cond
                [(positive? (vector-ref regs 0))
                 ;; goto 100
                 (vector-ref regs 0)
                 ]
                [else
                 ;; goto 100
                 (vector-ref regs 0)
                 ]
              )]
             [else
              ;; subleqi 3 0 5
              (set-subleq regs 3 0)
              (cond
                [(positive? (vector-ref regs 0))
                 ;; goto 100
                 (vector-ref regs 0)
                 ]
                [else
                 ;; goto 100
                 (vector-ref regs 0)
                 ]
              )]
           )]
          [else
           ;; subleqi 0 0 4
           (set-subleq regs 0 0)
           (cond
             [(positive? (vector-ref regs 0))
              ;; subleqi 3 0 5
              (set-subleq regs 3 0)
              (cond
                [(positive? (vector-ref regs 0))
                 ;; goto 100
                 (vector-ref regs 0)
                 ]
                [else
                 ;; goto 100
                 (vector-ref regs 0)
                 ]
                )]
             [else
              ;; subleqi 3 0 5
              (set-subleq regs 3 0)
              (cond
                [(positive? (vector-ref regs 0))
                 ;; goto 100
                 (vector-ref regs 0)
                 ]
                [else
                 ;; goto 100
                 (vector-ref regs 0)
                 ]
                )]
           )]
        )]
       )]
  )
)


(define (simple-beqz a L1 L2 regs)
  (if (equal? a 0) L1 L2)
)

(define (subleq-beqz a L1 L2 regs)
  ;; regs[0] is Z (always 0)
  (vector-set! regs 0 0)
  (vector-set! regs 1 a)
  (define r1_0 (??)) ;; 1
  (define r2_0 (??)) ;; 0
  (define r1_1 (??)) ;; 0
  (define r2_1 (??)) ;; 0
  (define r1_2 (??)) ;; 0
  (define r2_2 (??)) ;; 0
  (define r1_3 (??)) ;; 0
  (define r2_3 (??)) ;; 1
  ;; L0
  (set-subleq regs r1_0 r2_0)
  (cond
    [(positive? (vector-ref regs r2_0))
     ;; L1
     (set-subleq regs r1_1 r2_1)
     (cond
       [(positive? (vector-ref regs r2_1))
        ;; L2
        (set-subleq regs r1_2 r2_2)
        (cond
          [(positive? (vector-ref regs r2_2))
           ;; L3
           (set-subleq regs r1_3 r2_3)
           (cond
             [(positive? (vector-ref regs r2_3))
              ;; L4 -> L2
              L2
             ]
             [else
              ;; r2 -> L1
              L1
             ]
           )
          ]
          [else
           ;; L3
           (set-subleq regs r1_3 r2_3)
           (cond
             [(positive? (vector-ref regs r2_3))
              ;; L4 -> L2
              L2
             ]
             [else
              ;; r2 -> L1
              L1
             ]
           )
          ]
        )
       ]
       [else
        ;; L4 -> L2
        L2
       ]
     )
    ]
    [else
     ;; L2
     (set-subleq regs r1_2 r2_2)
     (cond
       [(positive? (vector-ref regs r2_2))
        ;; L3
        (set-subleq regs r1_3 r2_3)
        (cond
          [(positive? (vector-ref regs r2_3))
           ;; L4 -> L2
           L2
          ]
          [else
           ;; r2 -> L1
           L1
          ]
        )
       ]
       [else
        ;; L3
        (set-subleq regs r1_3 r2_3)
        (cond
          [(positive? (vector-ref regs r2_3))
           ;; L4 -> L2
           L2
          ]
          [else
           ;; r2 -> L1
           L1
          ]
        )
       ]
     )
    ]
  )
)

(define (branch-template a next label1 regs)
  ;; regs[0] is Z (always 0)
  (vector-set! regs 1 a)
  (define line0 (?? boolean?))
  (define line1 (?? boolean?))
  (define line2 (?? boolean?))
  (define jump3 (choose 4 5 6 7 8 9))
  (define jump4 (choose 5 6 7 8 9))
  (define jump5 (choose 6 7 8 9))
  (define jump6 (choose 7 8 9))
  (define r1_0 (choose 0 1 2 3 4 5 6 7 8 9))
  (define r2_0 (choose 3 4 5 6 7 8 9))
  (define r1_1 (choose 0 1 2 3 4 5 6 7 8 9))
  (define r2_1 (choose 3 4 5 6 7 8 9))
  (define r1_2 (choose 0 1 2 3 4 5 6 7 8 9))
  (define r2_2 (choose 3 4 5 6 7 8 9))
  (define r1_3 (choose 0 1 2 3 4 5 6 7 8 9))
  (define r2_3 (choose 3 4 5 6 7 8 9))
  (define r1_4 (choose 0 1 2 3 4 5 6 7 8 9))
  (define r2_4 (choose 3 4 5 6 7 8 9))
  (define r1_5 (choose 0 1 2 3 4 5 6 7 8 9))
  (define r2_5 (choose 3 4 5 6 7 8 9))
  (define r1_6 (choose 0 1 2 3 4 5 6 7 8 9))
  (define r2_6 (choose 3 4 5 6 7 8 9))
  (define r1_7 (choose 0 1 2 3 4 5 6 7 8 9))
  (define r2_7 (choose 3 4 5 6 7 8 9))
  ;; L0
  (cond [line0 (set-subleq regs r1_0 r2_0)])
  ;; L1
  (cond [line1 (set-subleq regs r1_1 r2_1)])
  ;; L2
  (cond [line2 (set-subleq regs r1_2 r2_2)])
  ;; L3
  (set-subleq regs r1_3 r2_3)
  (cond
    [(positive? (vector-ref regs r2_3))
     ;; L4
     (set-subleq regs r1_4 r2_4)
     (cond
       [(positive? (vector-ref regs r2_4))
        ;; L5
        (set-subleq regs r1_5 r2_5)
        (cond
          [(positive? (vector-ref regs r2_5))
           ;; L6
           (set-subleq regs r1_6 r2_6)
           (cond
             [(positive? (vector-ref regs r2_6))
              ;; L7
              (set-subleq regs r1_7 r2_7)
              (cond
                [(positive? (vector-ref regs r2_7))
                 next]
                [else
                 label1])
              ]
             [else
              ;; L6 -> L7 || next || label1
              (cond
                ;; L7
                [(equal? jump6 7)
                 (set-subleq regs r1_7 r2_7)
                 (cond
                   [(positive? (vector-ref regs r2_7))
                    next]
                   [else
                    label1])]
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
                [(positive? (vector-ref regs r2_6))
                 ;; L7
                 (set-subleq regs r1_7 r2_7)
                 (cond
                   [(positive? (vector-ref regs r2_7))
                    next]
                   [else
                    label1])]
                [else
                 ;; L6 -> L7 || next || label1
                 (cond
                   ;; L7
                   [(equal? jump6 7)
                    (set-subleq regs r1_7 r2_7)
                    (cond
                      [(positive? (vector-ref regs r2_7))
                       next]
                      [else
                       label1])]
                   ;; next
                   [(equal? jump6 8) next]
                   ;; label1
                   [else label1])
                 ])]
             ;; L7
             [(equal? jump5 7)
              (set-subleq regs r1_7 r2_7)
              (cond
                [(positive? (vector-ref regs r2_7))
                 next]
                [else
                 label1])]
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
             [(positive? (vector-ref regs r2_5))
              ;; L6
              (set-subleq regs r1_6 r2_6)
              (cond
                [(positive? (vector-ref regs r2_6))
                 ;; L7
                 (set-subleq regs r1_7 r2_7)
                 (cond
                   [(positive? (vector-ref regs r2_7))
                    next]
                   [else
                    label1])
                 ]
                [else
                 ;; L6 -> L7 || next || label1
                 (cond
                   ;; L7
                   [(equal? jump6 7)
                    (set-subleq regs r1_7 r2_7)
                    (cond
                      [(positive? (vector-ref regs r2_7))
                       next]
                      [else
                       label1])]
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
                   [(positive? (vector-ref regs r2_6))
                    ;; L7
                    (set-subleq regs r1_7 r2_7)
                    (cond
                      [(positive? (vector-ref regs r2_7))
                       next]
                      [else
                       label1])]
                   [else
                    ;; L6 -> L7 || next || label1
                    (cond
                      ;; L7
                      [(equal? jump6 7)
                       (set-subleq regs r1_7 r2_7)
                       (cond
                         [(positive? (vector-ref regs r2_7))
                          next]
                         [else
                          label1])]
                      ;; next
                      [(equal? jump6 8) next]
                      ;; label1
                      [else label1])
                    ])]
                ;; L7
                [(equal? jump5 7)
                 (set-subleq regs r1_7 r2_7)
                 (cond
                   [(positive? (vector-ref regs r2_7))
                    next]
                   [else
                    label1])]
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
             [(positive? (vector-ref regs r2_6))
              ;; L7
              (set-subleq regs r1_7 r2_7)
              (cond
                [(positive? (vector-ref regs r2_7))
                 next]
                [else
                 label1])
              ]
             [else
              ;; L6 -> L7 || next || label1
              (cond
                ;; L7
                [(equal? jump6 7)
                 (set-subleq regs r1_7 r2_7)
                 (cond
                   [(positive? (vector-ref regs r2_7))
                    next]
                   [else
                    label1])]
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
             [(positive? (vector-ref regs r2_7))
              next]
             [else
              label1])]
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
          [(positive? (vector-ref regs r2_4))
           ;; L5
           (set-subleq regs r1_5 r2_5)
           (cond
             [(positive? (vector-ref regs r2_5))
              ;; L6
              (set-subleq regs r1_6 r2_6)
              (cond
                [(positive? (vector-ref regs r2_6))
                 ;; L7
                 (set-subleq regs r1_7 r2_7)
                 (cond
                   [(positive? (vector-ref regs r2_7))
                    next]
                   [else
                    label1])
                 ]
                [else
                 ;; L6 -> L7 || next || label1
                 (cond
                   ;; L7
                   [(equal? jump6 7)
                    (set-subleq regs r1_7 r2_7)
                    (cond
                      [(positive? (vector-ref regs r2_7))
                       next]
                      [else
                       label1])]
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
                   [(positive? (vector-ref regs r2_6))
                    ;; L7
                    (set-subleq regs r1_7 r2_7)
                    (cond
                      [(positive? (vector-ref regs r2_7))
                       next]
                      [else
                       label1])]
                   [else
                    ;; L6 -> L7 || next || label1
                    (cond
                      ;; L7
                      [(equal? jump6 7)
                       (set-subleq regs r1_7 r2_7)
                       (cond
                         [(positive? (vector-ref regs r2_7))
                          next]
                         [else
                          label1])]
                      ;; next
                      [(equal? jump6 8) next]
                      ;; label1
                      [else label1])
                    ])]
                ;; L7
                [(equal? jump5 7)
                 (set-subleq regs r1_7 r2_7)
                 (cond
                   [(positive? (vector-ref regs r2_7))
                    next]
                   [else
                    label1])]
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
                [(positive? (vector-ref regs r2_5))
                 ;; L6
                 (set-subleq regs r1_6 r2_6)
                 (cond
                   [(positive? (vector-ref regs r2_6))
                    ;; L7
                    (set-subleq regs r1_7 r2_7)
                    (cond
                      [(positive? (vector-ref regs r2_7))
                       next]
                      [else
                       label1])
                    ]
                   [else
                    ;; L6 -> L7 || next || label1
                    (cond
                      ;; L7
                      [(equal? jump6 7)
                       (set-subleq regs r1_7 r2_7)
                       (cond
                         [(positive? (vector-ref regs r2_7))
                          next]
                         [else
                          label1])]
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
                      [(positive? (vector-ref regs r2_6))
                       ;; L7
                       (set-subleq regs r1_7 r2_7)
                       (cond
                         [(positive? (vector-ref regs r2_7))
                          next]
                         [else
                          label1])]
                      [else
                       ;; L6 -> L7 || next || label1
                       (cond
                         ;; L7
                         [(equal? jump6 7)
                          (set-subleq regs r1_7 r2_7)
                          (cond
                            [(positive? (vector-ref regs r2_7))
                             next]
                            [else
                             label1])]
                         ;; next
                         [(equal? jump6 8) next]
                         ;; label1
                         [else label1])
                       ])]
                   ;; L7
                   [(equal? jump5 7)
                    (set-subleq regs r1_7 r2_7)
                    (cond
                      [(positive? (vector-ref regs r2_7))
                       next]
                      [else
                       label1])]
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
                [(positive? (vector-ref regs r2_6))
                 ;; L7
                 (set-subleq regs r1_7 r2_7)
                 (cond
                   [(positive? (vector-ref regs r2_7))
                    next]
                   [else
                    label1])
                 ]
                [else
                 ;; L6 -> L7 || next || label1
                 (cond
                   ;; L7
                   [(equal? jump6 7)
                    (set-subleq regs r1_7 r2_7)
                    (cond
                      [(positive? (vector-ref regs r2_7))
                       next]
                      [else
                       label1])]
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
                [(positive? (vector-ref regs r2_7))
                 next]
                [else
                 label1])]
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
          [(positive? (vector-ref regs r2_5))
           ;; L6
           (set-subleq regs r1_6 r2_6)
           (cond
             [(positive? (vector-ref regs r2_6))
              ;; L7
              (set-subleq regs r1_7 r2_7)
              (cond
                [(positive? (vector-ref regs r2_7))
                 next]
                [else
                 label1])
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
             ;; L7
             [(equal? jump5 7)
              (set-subleq regs r1_7 r2_7)
              (cond
                [(positive? (vector-ref regs r2_7))
                 next]
                [else
                 label1])]
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
          [(positive? (vector-ref regs r2_6))
           ;; L7
           (set-subleq regs r1_7 r2_7)
           (cond
             [(positive? (vector-ref regs r2_7))
              next]
             [else
              label1])
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
       ;; L7
       [(equal? jump3 7)
        (set-subleq regs r1_7 r2_7)
        (cond
          [(positive? (vector-ref regs r2_7))
           next]
          [else
           label1])]
       ;; next
       [(equal? jump3 8) next]
       ;; label1
       [else label1])
     ])
  )

(define (simple-mult a b regs)
  (* a b)
  )

(define (loop-template a b regs)
  (vector-set! regs 1 a)
  (vector-set! regs 2 b)
  (define line10 (?? boolean?))
  (define line11 (?? boolean?))
  (define line12 (?? boolean?))
  (define line13 (?? boolean?))
  (define line3 (?? boolean?))
  (define line4 (?? boolean?))
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
    [(positive? (vector-ref regs r2_5))
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
       [(positive? (vector-ref regs r2_5))
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
          [(positive? (vector-ref regs r2_5))
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
             [(positive? (vector-ref regs r2_5))
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
                [(positive? (vector-ref regs r2_5))
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
                   [(positive? (vector-ref regs r2_5))
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
                      [(positive? (vector-ref regs r2_5))
                       ;; Exit
                       (vector-ref regs 0)]
                      [else
                       (vector-ref regs 0)
                       ])
                    ])
                 ])
              ])
           ])
        ])
     ])
  )

(define (interpret prog regs)
  (define len (length prog))
  (define (interpret-inst instr pc regs)
    (define op (list-ref instr 0))
    (cond
      [(equal? op 'add)
       (define r1 (list-ref instr 1)) (define r2 (list-ref instr 2)) (define r3 (list-ref instr 3))
       (define new_regs (vector-cpy regs))
       (vector-set! new_regs r1 (+ (vector-ref regs r2) (vector-ref regs r3)))
       (list (+ pc 1) new_regs)]
      ;; [(equal? op 'beqz)
      ;;  (define r1 (list-ref instr 1)) (define r2 (list-ref instr 2))
      ;;  (set! pc (if (equal? (vector-ref regs r1) 0) (vector-ref regs r2) (+ pc 1)))]
      ;; [(equal? op 'beqzi)
      ;;  (define r1 (list-ref instr 1)) (define L2 (list-ref instr 2))
      ;;  (set! pc (if (equal? (vector-ref regs r1) 0) L2 (+ pc 1)))]
      [(equal? op 'subleqi)
       (define r1 (list-ref instr 1)) (define r2 (list-ref instr 2)) (define L3 (list-ref instr 3))
       (define new_regs (vector-cpy regs))
       (vector-set! new_regs r2 (- (vector-ref regs r2) (vector-ref regs r1)))
       (list (if (positive? (vector-ref regs r2)) (+ pc 1) L3) new_regs)]
      ;; [(equal? op 'subleq)
      ;;  (define r1 (list-ref instr 1)) (define r2 (list-ref instr 2)) (define r3 (list-ref instr 3))
      ;;  (vector-set! regs r2 (- (vector-ref regs r2) (vector-ref regs r1)))
      ;;  (set! pc (if (positive? (vector-ref regs r2)) (+ pc 1) (vector-ref regs r3)))]
      [(equal? op 'goto)
       (list len regs)]
      [else (100 regs) (println "No such opcode defined!")]))
  (define all_states (make-vector (+ len 1) 0))
  (vector-set! all_states 0 regs)
  (define all_pcs (make-vector (+ len 1) 0))
  (for ([pc len])
    (define res (interpret-inst (list-ref prog pc) pc (vector-ref all_states pc)))
    (vector-set! all_pcs pc (list-ref res 0))
    (vector-set! all_states (list-ref res 0) (list-ref res 1))
   )
  (vector (vector-ref (vector-ref all_states len) 0))
)

;; Verification examples
(define (add-ex regs)
  (interpret `[(subleqi 3 3 1) (subleqi 1 3 2) (subleqi 2 3 3) (subleqi 0 0 4)
                               (subleqi 3 0 5) (goto)] regs)
  )

(define (add-orig regs)
  (interpret `[(add 0 1 2) (goto)] regs)
  )

(define (beqz-ex regs)
  (interpret `[(subleqi 0 0 1) (subleqi 1 0 3) (subleqi 0 0 5) (subleqi 0 0 4)
                               (subleq 0 1 2) (goto 3)] regs)
  )

(define (beqz-orig regs)
  (interpret `[(beqz 1 2) (goto 3)] regs)
  )

(define (same original example)
  (define regs (create-regs))
  (assert (equal? (original regs) (example regs)))
  )

(define (same-add original example a b)
  (define regs (make-vector 10 0))
  (assert (equal? (original a b regs) (example a b regs)))
)

(define (same-beqz original example a next label1)
  (define regs (make-vector 10 0))
  (assert (equal? (original a next label1 regs) (example a next label1 regs)))
)

(define (same-mult original example a)
  (define b 3)
  (define regs (vector 0 0 0 0 0 0 0 0 1 1))
  (assert (equal? (original a b regs) (example a b regs)))
)

;; (verify (same-beqz simple-beqz subleq-beqz))

;; Synthesis examples
(define-symbolic a integer?)
(define-symbolic b integer?)
(define-symbolic c integer?)

;; (define sol-add
;;   (synthesize #:forall (list a b)
;;               #:guarantee (same-add simple-add straightline-template a b)))

(define sol-beqz
  (synthesize #:forall (list a b c)
              #:guarantee (same-beqz simple-beqz branch-template a b c)))

;; (define sol-mult
;;   (synthesize #:forall a
;;               #:guarantee (same-mult simple-mult loop-template a)))

(print-forms sol-beqz)
