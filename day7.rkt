#lang racket

(require (for-syntax racket/base syntax/parse syntax/for-body))

(define-syntax (for/min stx)
  (syntax-parse stx
    [(_ clauses body ... tail-expr)
     (with-syntax ([original stx]
                   [((pre-body ...) (post-body ...))
                    (split-for-body stx #'(body ... tail-expr))])
       #'(for/fold/derived original
           ([res #f])
           clauses
           pre-body ...
           (define maybe-new-min (let () post-body ...))
           (if (number? res)
             (min res maybe-new-min)
             maybe-new-min)))]))

(define (get-input port)
  (map string->number (string-split (read-line port) ",")))

(define (calculate-dist crabs pos travel-cost)
  (define (cost-func n) (travel-cost n pos))
  (apply + (map cost-func crabs)))

(define (travel-cost1 n pos)
  (abs (- n pos)))

(define (travel-cost2 n pos)
  (let ([res (abs (- n pos))])
    (apply + (range 0 (add1 res)))))

(define (calc1 crabs pos)
  (calculate-dist crabs pos travel-cost1))

(define (calc2 crabs pos)
  (calculate-dist crabs pos travel-cost2))

(define (leftmost-crab crabs)
  (apply min crabs))

(define (rightmost-crab crabs)
  (apply max crabs))

(define (get-best-pos crabs)
  (for/min ([c (in-range (leftmost-crab crabs) (add1 (rightmost-crab crabs)))])
    (calc2 crabs c)))

(get-best-pos (get-input (current-input-port)))
