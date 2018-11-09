#lang rosette

(require racket/match)


(define (eval istr regs pc)
  (match istr
    [`(add ,rd ,rs ,rt)  (vector-set! regs rd (+ (vector-ref regs rs) (vector-ref regs rt))) (+ pc 1)]
    [`(sub ,rd ,rs ,rt)  (vector-set! regs rd (- (vector-ref regs rs) (vector-ref regs rt))) (+ pc 1)]
    [`(div ,rd ,rs ,rt)  (vector-set! regs rd (/ (vector-ref regs rs) (vector-ref regs rt))) (+ pc 1)]
    [`(mult ,rd ,rs ,rt) (vector-set! regs rd (* (vector-ref regs rs) (vector-ref regs rt))) (+ pc 1)]
    [`(subleq ,ra ,rb ,rc) (vector-set! regs rb (- (vector-ref regs rb) (vector-ref regs ra))) (if (positive? rb) (+ pc 1) rc)]
    )
  )

(define (interpret prog inputs)
  (define len (+ (length prog) (length inputs)))
  (define regs (make-vector len +nan.0))
  (for ([in inputs]
        [i (length inputs)])
    (vector-set! regs i in)
  )
  (do ([pc 0])
      ((>= pc (length prog)))
    (let ([instr (list-ref prog pc)])
      (set! pc (eval instr regs pc)))
  )
  (vector-ref regs (- len 1))
)

;; Examples
(println (interpret `[(mult 2 1 0) (add 3 2 2)] `[6 7]))
