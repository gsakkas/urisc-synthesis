#lang rosette


(define (eval istr r1 r2 r3 regs pc)
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

(define (interpret prog inputs)
  (define len (vector-length inputs))
  (define regs (make-vector len 0))
  (for ([in inputs]
        [i (vector-length inputs)])
    (vector-set! regs i in)
    )
  (do ([pc 0])
      ((or (>= pc (length prog)) (< pc 0)) (vector-set! regs 0 pc))
    (println prog) (print "STOPPING") (println (>= pc (length prog)))
    (let ([instr (list-ref prog pc)]) (print "INSTR") (println instr) (print "PC") (println pc)
      (set! pc (eval (list-ref instr 0) (list-ref instr 1) (list-ref instr 2)
                     (list-ref instr 3) regs pc)))
    (print "PC2") (println pc) (print "Stopping 2") (println (>= pc (length prog)))
    )
  (vector (vector-ref regs 0) (vector-ref regs 1))
  )

;; Examples
; (println (interpret `[(mult 2 1 0) (add 3 2 2)] `[6 7]))

(define (dynamic) (define-symbolic* a integer?) a)
(define regs (vector (dynamic) (dynamic) (dynamic) (dynamic) (dynamic) ))

(define add-ex
  (interpret `[(subleqi 4 4 1) (subleqi 2 4 2) (subleqi 3 4 3) (subleqi 1 1 4)
                      (subleqi 4 1 5) (goto 100 -100 -100)] regs)
)

(define add-orig
  (interpret `[(add 1 2 3) (goto 100 -100 -100)] regs)
)

(define beqz-ex
  (interpret `[(subleqi 3 3 1) (subleqi 1 3 3) (subleqi 3 3 5) (subleqi 3 3 4) (subleq 3 1 2) (goto 100 -100 -100)] regs)
  )

(define beqz-orig
  (interpret `[(beqz 1 2 -100) (goto 100 -100 -100)] regs)
  )

(verify (assert (equal? beqz-ex beqz-orig)))
