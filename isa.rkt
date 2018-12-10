#lang rosette

(current-bitwidth 8)
(require rosette/lib/synthax)

(define (dynamic) (define-symbolic* a integer?) a)

(define (create-regs) (vector (dynamic) (dynamic) (dynamic) (dynamic) (dynamic)
                            (dynamic) (dynamic) (dynamic) (dynamic) (dynamic)))

(define (vector-cpy v1)
  (define len (vector-length v1))
  (define v2 (make-vector len 0))
  (for ([i len])
    (vector-set! v2 i (vector-ref v1 i)))
  v2)

(define (set-subleq regs r1 r2)
  (vector-set! regs r2 (- (vector-ref regs r2) (vector-ref regs r1)))
)

(define (interpret prog regs)
  (define len (length prog))
  (define (interpret-inst instr pc regs)
    (define op (list-ref instr 0))
    (cond
      [(equal? op 'add)
       (define r1 (list-ref instr 1)) (define r2 (list-ref instr 2)) (define r3 (list-ref instr 3))
       (define new_regs (vector-cpy regs))
       (vector-set! new_regs r1 (+ (vector-ref regs r2) (vector-ref regs r3)))
       (list (+ pc 1) new_regs)]
      ;; [(equal? op 'beqz)
      ;;  (define r1 (list-ref instr 1)) (define r2 (list-ref instr 2))
      ;;  (set! pc (if (equal? (vector-ref regs r1) 0) (vector-ref regs r2) (+ pc 1)))]
      ;; [(equal? op 'beqzi)
      ;;  (define r1 (list-ref instr 1)) (define L2 (list-ref instr 2))
      ;;  (set! pc (if (equal? (vector-ref regs r1) 0) L2 (+ pc 1)))]
      [(equal? op 'subleqi)
       (define r1 (list-ref instr 1)) (define r2 (list-ref instr 2)) (define L3 (list-ref instr 3))
       (define new_regs (vector-cpy regs))
       (vector-set! new_regs r2 (- (vector-ref regs r2) (vector-ref regs r1)))
       (list (if (positive? (vector-ref regs r2)) (+ pc 1) L3) new_regs)]
      ;; [(equal? op 'subleq)
      ;;  (define r1 (list-ref instr 1)) (define r2 (list-ref instr 2)) (define r3 (list-ref instr 3))
      ;;  (vector-set! regs r2 (- (vector-ref regs r2) (vector-ref regs r1)))
      ;;  (set! pc (if (positive? (vector-ref regs r2)) (+ pc 1) (vector-ref regs r3)))]
      [(equal? op 'goto)
       (list len regs)]
      [else (100 regs) (println "No such opcode defined!")]))
  (define all_states (make-vector (+ len 1) 0))
  (vector-set! all_states 0 regs)
  (define all_pcs (make-vector (+ len 1) 0))
  (for ([pc len])
    (define res (interpret-inst (list-ref prog pc) pc (vector-ref all_states pc)))
    (vector-set! all_pcs pc (list-ref res 0))
    (vector-set! all_states (list-ref res 0) (list-ref res 1))
   )
  (vector (vector-ref (vector-ref all_states len) 0))
)

;; Verification examples
(define (add-ex regs)
  (interpret `[(subleqi 3 3 1) (subleqi 1 3 2) (subleqi 2 3 3) (subleqi 0 0 4)
                               (subleqi 3 0 5) (goto)] regs)
  )

(define (add-orig regs)
  (interpret `[(add 0 1 2) (goto)] regs)
  )

(define (beqz-ex regs)
  (interpret `[(subleqi 0 0 1) (subleqi 1 0 3) (subleqi 0 0 5) (subleqi 0 0 4)
                               (subleq 0 1 2) (goto 3)] regs)
  )

(define (beqz-orig regs)
  (interpret `[(beqz 1 2) (goto 3)] regs)
  )

(define (same original example)
  (define regs (create-regs))
  (assert (equal? (original regs) (example regs)))
  )

(verify (same add-orig add-ex))
