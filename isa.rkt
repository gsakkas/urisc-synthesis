#lang rosette

(define (dynamic) (define-symbolic* a integer?) a)

(define (create-regs) (vector (dynamic) (dynamic) (dynamic) (dynamic) (dynamic)
                            (dynamic) (dynamic) (dynamic) (dynamic) (dynamic)))

(define (interpret prog regs)
  (define len (length prog))
  (define pc 0)
  (define (interpret-inst instr)
    (define op (list-ref instr 0))
    (cond
      [(equal? op 'add)
       (define r1 (list-ref instr 1)) (define r2 (list-ref instr 2)) (define r3 (list-ref instr 3))
       (vector-set! regs r1 (+ (vector-ref regs r2) (vector-ref regs r3))) (set! pc (+ pc 1))]
      [(equal? op 'beqz)
       (define r1 (list-ref instr 1)) (define r2 (list-ref instr 2))
       (set! pc (if (equal? (vector-ref regs r1) 0) (vector-ref regs r2) (+ pc 1)))]
      [(equal? op 'subleqi)
       (define r1 (list-ref instr 1)) (define r2 (list-ref instr 2)) (define L3 (list-ref instr 3))
       (vector-set! regs r2 (- (vector-ref regs r2) (vector-ref regs r1)))
       (set! pc (if (positive? (vector-ref regs r2)) (+ pc 1) L3))]
      [(equal? op 'subleq)
       (define r1 (list-ref instr 1)) (define r2 (list-ref instr 2)) (define r3 (list-ref instr 3))
       (vector-set! regs r2 (- (vector-ref regs r2) (vector-ref regs r1)))
       (set! pc (if (positive? (vector-ref regs r2)) (+ pc 1) (vector-ref regs r3)))]
      [(equal? op 'goto)
       (define L1 (list-ref instr 1))
       (set! pc L1)]
      [else (set! pc 100) (println "No such opcode defined!")]))
  (do ()
      ((assert (and (< pc (length prog)) (>= pc 0))))
    (interpret-inst (list-ref prog pc))
    )
  (vector pc (vector-ref regs 0))
)

;; Examples
(define (add-ex regs)
  (interpret `[(subleqi 3 3 1) (subleqi 1 3 2) (subleqi 2 3 3) (subleqi 0 0 4)
                      (subleqi 3 0 5) (goto 100)] regs)
)

(define (add-orig regs)
  (interpret `[(add 0 1 2) (goto 100)] regs)
)

(define (beqz-ex regs)
  (interpret `[(subleqi 3 3 1) (subleqi 1 3 3) (subleqi 3 3 5) (subleqi 3 3 4)
                               (subleq 3 1 2) (goto 100)] regs)
  )

(define (beqz-orig regs)
  (interpret `[(beqz 1 2) (goto 100)] regs)
  )

(define (same original example)
  (define regs (create-regs))
  (assert (equal? (original regs) (example regs)))
  )

(verify (same beqz-orig beqz-ex))
