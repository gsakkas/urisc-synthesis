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

(define (interpret prog)
  (define regs (make-vector 10 -1000))
  (vector-set! regs 0 6)
  (vector-set! regs 1 7)
  (for ([instr prog])
    (eval instr regs)
  )
  (println regs)
)

;; Examples
(interpret `[(mult 2 1 0) (add 3 2 2)])
