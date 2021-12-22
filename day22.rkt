#lang racket

(define (get-input port)
  (define (parse-range range)
    (match-define (list start-str end-str)
      (string-split (substring range 2) ".."))

    (cons (string->number start-str)
          (string->number end-str)))

  (define (parse-line line)
    (match-define (list switch ranges) (string-split line " "))
    (define coord-ranges (map parse-range (string-split ranges ",")))
    (list* (string->symbol switch) coord-ranges))

  (define (get)
    (define line (read-line port))
    (if (eof-object? line)
      '()
      (cons (parse-line line) (get))))

  (get))

;; part 1
(define (in-core-range)
  (in-inclusive-range -50 50))

(define (build-core)
  (for*/hash ([x (in-core-range)]
              [y (in-core-range)]
              [z (in-core-range)])
    (values (list x y z) 0)))

(define (shrink-range range)
  (match range
    [(cons start end)
     (cons (max -50 start)
           (min 50 end))]))

(define (shrink-ranges op)
  (list* (car op) (map shrink-range (cdr op))))

(define (convert-op-type op)
  (if (symbol=? (car op) 'on)
    1
    0))

(define (get-z-range ranges)
  (define range (cadddr ranges))
  (in-inclusive-range (car range)
                      (cdr range)))

(define (get-y-range ranges)
  (define range (caddr ranges))
  (in-inclusive-range (car range)
                      (cdr range)))

(define (get-x-range ranges)
  (define range (cadr ranges))
  (in-inclusive-range (car range)
                      (cdr range)))

(define (apply-op core op)
  (define shrunk (shrink-ranges op))
  (define op-num (convert-op-type shrunk))

  (for*/fold ([core core])
             ([x (get-x-range shrunk)]
              [y (get-y-range shrunk)]
              [z (get-z-range shrunk)])
    (hash-set core (list x y z) op-num)))

(define (apply-ops core ops)
  (for/fold ([core core])
            ([op ops])
    (apply-op core op)))

(define (get-on-count core)
  (for/sum ([v (in-hash-values core)])
    v))

;; part 2
;; new approach: the ranges are too large to fit in memory.
;; so, we'll instead describe everything as a set of operations.
;; There are 'add' and 'subtract' operations. 

(define (range-out-of-core? range)
  (or (and (> (car range) 50)
           (> (cdr range) 50))
      (and (< (car range) -50)
           (< (cdr range) -50))))

(define (op-out-of-core? op)
  (andmap range-out-of-core? (cdr op)))

(define (falls-in-range? n range)
  (and (>= n (car range))
       (<= n (cdr range))))

(define (overlapping-ranges? r1 r2)
  (or (falls-in-range? (car r1) r2)
      (falls-in-range? (cdr r1) r2)))

(define (is-on? op)
  (symbol=? (car op) 'on))

(define (is-off? op)
  (symbol=? (car op) 'off))

(define (invert-type op-type)
  (match op-type
    ['on 'off]
    ['off 'on]
    [_ (error "invert-sym: only handles 'on and 'off, instead got: " op-type)]))

(define (overlaps? op1 op2)
  (and (overlapping-ranges? (cadr op1) (cadr op2))
       (overlapping-ranges? (caddr op1) (caddr op2))
       (overlapping-ranges? (cadddr op1) (cadddr op2))
       (or (and (is-on? op1) (is-off? op2))
           (and (is-off? op1) (is-on? op2)))))

(define (find-overlaps ops)
  (define (overlaps-with op others)
    (for ([other others]
          #:when (overlaps? op other))
      (displayln "OVERLAP")
      (displayln op)
      (displayln other)))

  (define (all-overlaps ops)
    (unless (empty? ops)
      (overlaps-with (car ops) (cdr ops))
      (all-overlaps (cdr ops))))

  (all-overlaps ops))

(define (get-size range)
  (max (- (add1 (cdr range)) (car range))
       0))

(define (get-volume op)
  (* (get-size (cadr op))
     (get-size (caddr op))
     (get-size (cadddr op))))

(define (range-intersect r1 r2)
  (cons (max (car r1) (car r2))
        (min (cdr r1) (cdr r2))))

(define (op-intersect op-type op1 op2)
  (list op-type
        (range-intersect (cadr op1) (cadr op2))
        (range-intersect (caddr op1) (caddr op2))
        (range-intersect (cadddr op1) (cadddr op2))))

(define (range-subsumes? r1 r2)
  (and (<= (car r1) (car r2))
       (>= (cdr r1) (cdr r2))))

(define (subsumes? op1 op2)
  (and (range-subsumes? (cadr op1) (cadr op2))
       (range-subsumes? (caddr op1) (caddr op2))
       (range-subsumes? (cadddr op1) (cadddr op2))))

(define (process-all-ops ops)
  (define (apply-off-op op state)
    (if (empty? state)
      '()
      (let ([next-op (car state)]
            [new-state (apply-off-op op (cdr state))])
        (define intersection
          (op-intersect (invert-type (car next-op)) op next-op))

        (if (positive? (get-volume intersection))
          (list* next-op intersection new-state)
          (cons next-op new-state)))))

  (define (apply-on-op op state)
    (if (empty? state)
      (list op)
      (let ([next-op (car state)]
            [new-state (apply-on-op op (cdr state))])
        (define intersection
          (op-intersect (invert-type (car next-op)) op next-op))

        (if (positive? (get-volume intersection))
          (list* next-op intersection new-state)
          (cons next-op new-state)))))

  (define (apply-op op state)
    (if (is-on? op)
      (apply-on-op op state)
      (apply-off-op op state)))

  (define (apply-all-ops ops)
    (for/fold ([state '()])
              ([op ops])
      (apply-op op state)))

  (apply-all-ops ops))

(define (get-total-volume cubes)
  (cond
    [(empty? cubes) 0]
    [else
     (let ([volume (get-volume (car cubes))])
       (if (is-on? (car cubes))
         (+ (get-total-volume (cdr cubes)) volume)
         (- (get-total-volume (cdr cubes)) volume)))]))

(let ([inp (get-input (current-input-port))]
      [core (build-core)])
  (get-total-volume (process-all-ops inp)))
