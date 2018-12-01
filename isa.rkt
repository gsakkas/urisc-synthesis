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

(define (eval-2 istr r1 r2 r3 regs pc)
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
      ((>= pc (length prog)) (vector-set! regs 0 pc))
    (let ([instr (list-ref prog pc)])
      (set! pc (eval instr regs pc)))
  )
  (vector (vector-ref regs 0) (vector-ref regs 1))
)

(define (interpret-2 prog inputs)
  (define len (vector-length inputs))
  (define regs (make-vector len 0))
  (for ([in inputs]
        [i (vector-length inputs)])
    (vector-set! regs i in)
    )
  (do ([pc 0])
      ((>= pc (length prog)) (vector-set! regs 0 pc))
    (let ([instr (list-ref prog pc)])
      (set! pc (eval-2 (list-ref instr 0) (list-ref instr 1) (list-ref instr 2) (list-ref instr 3) regs pc)))
    )
  (vector (vector-ref regs 0) (vector-ref regs 1))
  )

;; Examples
; (println (interpret `[(mult 2 1 0) (add 3 2 2)] `[6 7]))

(define (dynamic) (define-symbolic* a integer?) a)
(define regs (vector (dynamic) (dynamic) (dynamic) (dynamic) (dynamic) ))

(define add-ex
  (interpret-2 `[(subleqi 4 4 1) (subleqi 2 4 2) (subleqi 3 4 3) (subleqi 1 1 4)
                      (subleqi 4 1 5) (goto 100 -100 -100)] regs)
)

(define add-orig
  (interpret-2 `[(add 1 2 3) (goto 100 -100 -100)] regs)
)

(define beqz-ex
  (interpret-2 `[(subleqi 3 3 1) (subleqi 1 3 3) (subleqi 3 3 5) (subleqi 3 3 4) (subleqi 3 1 2) (goto 100 -100 -100)] regs)
  )

(define beqz-orig
  (interpret-2 `[(beqz 1 2 -100) (goto 100 -100 -100)] regs)
  )

(verify (assert (equal? beqz-ex beqz-orig)))
