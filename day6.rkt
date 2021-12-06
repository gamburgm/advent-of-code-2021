#lang racket

(define (get-input port)
  (map string->number (string-split (read-line port) ",")))

(define (get-fish-count counter end-date cache)
  (define pair (cons counter end-date))
  (define spawn-date (add1 counter))

  (cond
    [(hash-has-key? cache pair)
      (values (hash-ref cache pair) cache)]
    [(>= spawn-date end-date)
      (values 1 cache)]
    [else
      (let*-values ([(next-date) (- end-date spawn-date)]
                    [(curr-fish-count new-cache) (get-fish-count 6 next-date cache)]
                    [(next-fish-count new-cache) (get-fish-count 8 next-date new-cache)]
                    [(total-count) (+ curr-fish-count next-fish-count)]
                    [(final-cache) (hash-set new-cache pair total-count)])
        (values total-count final-cache))]))

(define (get-total fish end-date)
  (define-values (res cache)
    (for/fold ([total 0]
               [cache (hash)])
              ([f fish])
      (let-values ([(fish-count new-cache) (get-fish-count f end-date cache)])
        (values (+ total fish-count) new-cache))))

  res)

(get-total (get-input (current-input-port)) 257)
