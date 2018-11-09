#lang racket

(require racket/match)


(define (eval istr regs)
  (match istr
    [`(add ,rd ,rs ,rt)  (vector-set! regs rd (+ (vector-ref regs rs) (vector-ref regs rt)))]
    [`(sub ,rd ,rs ,rt)  (vector-set! regs rd (- (vector-ref regs rs) (vector-ref regs rt)))]
    [`(div ,rd ,rs ,rt) (vector-set! regs rd (/ (vector-ref regs rs) (vector-ref regs rt)))]
    [`(mult ,rd ,rs ,rt) (vector-set! regs rd (* (vector-ref regs rs) (vector-ref regs rt)))]
    ;; subleq not working as is
    ;; [`(subleq ,rd ,rs ,rt) (vector-set! regs rd (- (vector-ref regs rs) (vector-ref regs rd))) (if (positive? rd) (+ rd + 1) rt)]
    )
  )

(define (interpret prog inputs)
  (define len (+ (length prog) (length inputs)))
  (define regs (make-vector len +nan.0))
  (for ([in inputs]
        [i (length inputs)])
    (vector-set! regs i in)
  )
  (for ([instr prog])
    (eval instr regs)
  )
  (println (vector-ref regs (- len 1)))
)

;; Examples
(interpret `[(mult 2 1 0) (add 3 2 2)] `[6 7])
