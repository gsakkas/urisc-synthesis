#lang rosette

(require racket/match)


(define (eval istr regs pc)
  (match istr
    [`(add ,rd ,rs ,rt)  (vector-set! regs rd (+ (vector-ref regs rs) (vector-ref regs rt))) (+ pc 1)]
    [`(sub ,rd ,rs ,rt)  (vector-set! regs rd (- (vector-ref regs rs) (vector-ref regs rt))) (+ pc 1)]
    [`(div ,rd ,rs ,rt)  (vector-set! regs rd (/ (vector-ref regs rs) (vector-ref regs rt))) (+ pc 1)]
    [`(mult ,rd ,rs ,rt) (vector-set! regs rd (* (vector-ref regs rs) (vector-ref regs rt))) (+ pc 1)]
    [`(subleqi ,ra ,rb ,rc) (vector-set! regs rb (- (vector-ref regs rb) (vector-ref regs ra)))
                           (if (positive? (vector-ref regs rb) ) (+ pc 1) rc)]
    [`(subleq ,ra ,rb ,rc) (vector-set! regs rb (- (vector-ref regs rb) (vector-ref regs ra))) 
                           (if (positive? (vector-ref regs rb) ) (+ pc 1) 
                               (vector-ref regs rc))]
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
      ((>= pc (length prog)))
    (let ([instr (list-ref prog pc)])
      (set! pc (eval instr regs pc)))
  )
  (vector-ref regs 0)
)

;; Examples
; (println (interpret `[(mult 2 1 0) (add 3 2 2)] `[6 7]))

(define (dynamic) (define-symbolic* a integer?) a)
(define regs (vector (dynamic) (dynamic) (dynamic) (dynamic) (dynamic) ))

(define exp1
(interpret `[(subleqi 3 3 1) (subleqi 1 3 2) (subleqi 2 3 3) (subleqi 0 0 4)
                      (subleqi 3 0 5)] regs)
)

(define exp2
(interpret `[(add 0 1 2)] regs)
)

(define exp3
  (interpret `[(sub 0 1 2)] regs))

(verify (assert (= exp2 exp3)))
