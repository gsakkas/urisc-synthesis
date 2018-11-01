#lang racket

(require racket/match)

(define regs (make-vector 5 -1000))

(define (eval istr)
  (match istr
    [(? number?)         istr]
    [`(add, rd, rs, rt)  (vector-set! regs rd (+ (vector-ref regs rs) (vector-ref regs rt)))]
    [`(sub, rd, rs, rt)  (vector-set! regs rd (- (vector-ref regs rs) (vector-ref regs rt)))]
    [`(mult, rd, rs, rt) (vector-set! regs rd (* (vector-ref regs rs) (vector-ref regs rt)))]
    )
  )

;; Examples
(vector-set! regs 0 6)
(vector-set! regs 1 7)

(eval `(mult 2 0 1))

regs