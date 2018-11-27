#lang rosette

(require racket/match)


(define (eval istr regs pc)
  (match istr
    [`(add ,rd ,rs ,rt)  (vector-set! regs rd (+ (vector-ref regs rs) (vector-ref regs rt))) (+ pc 1)]
    [`(sub ,rd ,rs ,rt)  (vector-set! regs rd (- (vector-ref regs rs) (vector-ref regs rt))) (+ pc 1)]
    [`(div ,rd ,rs ,rt)  (vector-set! regs rd (/ (vector-ref regs rs) (vector-ref regs rt))) (+ pc 1)]
    [`(mult ,rd ,rs ,rt) (vector-set! regs rd (* (vector-ref regs rs) (vector-ref regs rt))) (+ pc 1)]
    [`(beqz ,rd ,rc) (if (equal? (vector-ref regs rd) 0) (vector-ref regs rc) (+ pc 1) )]
    [`(subleqi ,ra ,rb ,rc) (vector-set! regs rb (- (vector-ref regs rb) (vector-ref regs ra)))
                           (if (positive? (vector-ref regs rb) ) (+ pc 1) rc)]
    [`(subleq ,ra ,rb ,rc) (vector-set! regs rb (- (vector-ref regs rb) (vector-ref regs ra))) 
                           (if (positive? (vector-ref regs rb) ) (+ pc 1) 
                               (vector-ref regs rc))]
    [`(goto ,pos) pos] 
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
      ((>= pc (length prog)) (vector-set! regs 0 pc) )
    (let ([instr (list-ref prog pc)])
      (set! pc (eval instr regs pc)))
  )
  (vector (vector-ref regs 0) (vector-ref regs 1))
)

;; Examples
; (println (interpret `[(mult 2 1 0) (add 3 2 2)] `[6 7]))

(define (dynamic) (define-symbolic* a integer?) a)
(define regs (vector (dynamic) (dynamic) (dynamic) (dynamic) (dynamic) ))

(define exp1
(interpret `[(subleqi 4 4 1) (subleqi 2 4 2) (subleqi 3 4 3) (subleqi 1 1 4)
                      (subleqi 4 1 5) (goto 100)] regs)
)

(define exp2
(interpret `[(add 1 2 3) (goto 100)] regs)
)

(define exp3
  (interpret `[(sub 1 2 3) (goto 100)] regs))

(verify (assert (equal? exp2 exp3)))
