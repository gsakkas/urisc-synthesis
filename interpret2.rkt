#lang rosette
(define (do_instr prog istr r1 r2 r3 regs pc)
  (case istr
    ['add (vector-set! regs r1 (+ (vector-ref regs r2) (vector-ref regs r3))) 
     (interpret prog (+ pc 1) regs)]
    ['sub (vector-set! regs r1 (- (vector-ref regs r2) (vector-ref regs r3))) 
     (interpret prog (+ pc 1) regs)]
    ['beqzi (let ([regs2 (copy-vec regs)])
           (if (equal? (vector-ref regs r1) 0) 
           (interpret prog r2 regs)
           (interpret prog (+ pc 1) regs2)))]
    ['subleqi (vector-set! regs r2 (- (vector-ref regs r2) (vector-ref regs r1)))
           (let ([regs2 (copy-vec regs)])
           (if (positive? (vector-ref regs r2) ) 
           (interpret prog (+ pc 1) regs)
           (interpret prog r3 regs2)))]
    ['subleq (vector-set! regs r2 (- (vector-ref regs r2) (vector-ref regs r1)))
           (let ([regs2 (copy-vec regs)])
           (if (positive? (vector-ref regs r2) )
           (interpret prog (+ pc 1) regs)
           (interpret prog (vector-ref regs r3) regs2)))]
    ['goto (interpret prog r1 regs)]
    )
  )

(define (interpret prog pc regs)
  (print regs)
  (cond 
    [(and (>= pc 0) (< pc (length prog)))
      (println "COND2") 
      (println pc)
          (let* ([pc pc]
                 [instr (list-ref prog pc)]
                 [len (vector-length regs)])
                 (do_instr prog (list-ref instr 0) (list-ref instr 1)
                          (list-ref instr 2) (list-ref instr 3) regs pc))
    ]
    [else 
      (println "COND1") `[,pc ,regs] ])
)

(define (interpret-top prog pc regs)
  (let ([ans (interpret prog pc regs)])
  (vector (list-ref ans 1) (vector-ref (list-ref ans 1) 1)))
)
(define (copy-vec regs)
  (define len (vector-length regs))
  (define r (make-vector len 0))
  (for ([in regs]
        [i len])
     (vector-set! r i in)
  )
  r
)

(define (dynamic) (define-symbolic* a integer?) a)
(define regs (vector (dynamic) (dynamic) (dynamic) (dynamic) (dynamic) ))

;(define add-orig
;  (interpret `[(add 1 2 3) (goto 100 -100 -100)] 0 (copy-vec regs))
;)
(println "DONE")
(define beqz-orig
  (interpret-top `[(beqzi 1 101 -100) (goto 100 -100 -100)] 0 (copy-vec regs))
)
(println "DONE")
(define beqz-ex
  (interpret-top `[(subleqi 3 3 1) (subleqi 1 3 3) (subleqi 3 3 5) (subleqi 3 3 4)
                               (subleqi 3 1 101) (goto 100 -100 -100)] 
                               0 (copy-vec regs))
)
(define add-ex
    (interpret-top `[(subleqi 3 3 1) (subleqi 1 3 2) (subleqi 2 3 3) (subleqi 0 0 4)
                              (subleqi 3 0 5) (goto 100 -100 -100)] 0 (copy-vec regs))
      )

(define add-orig
    (interpret-top `[(add 0 1 2) (goto 100 -100 -100)] 0 (copy-vec regs))
      )

(define sub-orig
    (interpret-top `[(sub 0 1 2) (goto 100 -100 -100)] 0 (copy-vec regs))
      )

(verify (assert (equal? beqz-orig beqz-ex)))
