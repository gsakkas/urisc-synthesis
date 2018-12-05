#lang rosette
(define (do_instr istr r1 r2 r3 regs pc)
  (case istr
    ['add (vector-set! regs r1 (+ (vector-ref regs r2) (vector-ref regs r3))) (+ pc 1)]
    ['beqz (if (equal? (vector-ref regs r1) 0) (vector-ref regs r2) (+ pc 1) )]
    ['subleqi (vector-set! regs r2 (- (vector-ref regs r2) (vector-ref regs r1)))
                            (if (positive? (vector-ref regs r2) ) (+ pc 1) r3)]
    ['subleq (vector-set! regs r2 (- (vector-ref regs r2) (vector-ref regs r1)))
                           (if (positive? (vector-ref regs r2) ) (+ pc 1)
                               (vector-ref regs r3))]
    ['goto r1]
    )
  )

(define (interpret prog pc regs)
  (println prog) (println pc) (println regs)
  (cond 
    [(or (>= pc (length prog)) (< pc 0)) pc]
    [else ((let ([instr (list-ref prog pc)])
       (set! pc (do_instr (list-ref instr 0) (list-ref instr 1)
                          (list-ref instr 2) (list-ref instr 3) regs pc))
  ) (interpret prog pc regs)) ])
)

(define (dynamic) (define-symbolic* a integer?) a)
(define regs (vector (dynamic) (dynamic) (dynamic) (dynamic) (dynamic) ))

(define add-orig
  (interpret `[(add 1 2 3) (goto 100 -100 -100)] 0 regs)
)
