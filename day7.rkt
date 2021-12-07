#lang racket

(require syntax/parse/define)
(require (for-syntax racket/base syntax/parse))

;; Linear solution for part 1:
;; 1. count the number of crabs to the left and right at each index
;; 2. get fuel count at min idx
;; 3. move right, adjusting the fuel count at each step based on # of crabs at left and right to calculate new fuel usage
;; 4. return the minimum result

(define-syntax-parse-rule (for/min for-clauses body ...+)
  (begin
    (define res
      (for/fold ([n 'no-min])
                for-clauses
        (let ([val (begin body ...)])
          (if (eq? n 'no-min)
            val
            (min val n)))))
    (if (eq? res 'no-min)
      #f
      res)))

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
