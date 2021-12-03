#lang racket

(define (get-input port)
  (let loop ([inp '()])
    (let ([next (read-line port)])
      (if (eof-object? next)
        inp
        (loop (cons next inp))))))

(define (get-rate report)
  ;; INVARIANT: all binary numbers in the report are the same length

  (define (get-one-counts)
    (define counts (make-vector (string-length (car report)) 0))
    (for ([r report])
      (for ([c (string->list r)]
            [i (in-range (string-length r))]
            #:when (char=? #\1 c))
        (vector-set! counts i (add1 (vector-ref counts i)))))
    counts)

  (define (get-threshold report)
    (/ (length report) 2))

  ;; PART 1
  (define (get-power-consumption-rates counts cmp)
    (define res 0)
    (define threshold (get-threshold report))

    (for/fold ([res 0])
              ([c (in-vector counts)])
      (if (cmp c threshold)
        (+ 1 (* 2 res))
        (* 2 res))))

  (define (get-gamma counts)
    (get-power-consumption-rates counts >))

  (define (get-epsilon counts)
    (get-power-consumption-rates counts <))

  ;; PART 2
  (define (one-most-common? report pos)
    (define count-ones
      (for/fold ([n 0])
                ([r report]
                 #:when (char=? (string-ref r pos) #\1))
        (add1 n)))

    (>= count-ones (get-threshold report)))

  (define (get-next-digit one-most-common? common-type)
    (if (boolean=? common-type one-most-common?)
      #\1
      #\0))

  (define (filter-by-digit report pos digit)
    (for/list ([r report]
               #:when (char=? (string-ref r pos) digit))
      r))

  (define (bin->num b)
    (for/fold ([n 0])
              ([digit (string->list b)])
      (if (char=? digit #\1)
        (+ 1 (* 2 n))
        (* 2 n))))

  (define (get-life-support-rates common-type curr-report pos)
    (let* ([one-common (one-most-common? curr-report pos)]
           [next-digit (get-next-digit one-common common-type)]
           [rem (filter-by-digit curr-report pos next-digit)])
      (if (= (length rem) 1)
        (bin->num (car rem))
        (get-life-support-rates common-type rem (add1 pos)))))

  (define (get-oxygen-gen)
    (get-life-support-rates #t report 0))

  (define (get-scrubber)
    (get-life-support-rates #f report 0))

  (if (empty? report)
    0
    (* (get-oxygen-gen) (get-scrubber))))

(get-rate (get-input (current-input-port)))
