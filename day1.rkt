#lang racket

(define (get-input-list port)
  (define reversed-input
    (let read-input ([input-so-far '()])
      (define next (read port))
      (if (eof-object? next)
        input-so-far
        (read-input (cons next input-so-far)))))

  (reverse reversed-input))

(define (make-windows ls n)
  (define-values (sum next-ls)
    (for/fold ([sum 0]
               [next-ls ls])
              ([idx (in-range n)])
      (values (+ sum (car next-ls)) (cdr next-ls))))

  (define-values (reversed-windows _final-sum)
    (for/fold ([windows (list sum)]
               [curr-sum sum])
              ([left ls]
               [right next-ls])
      (let ([next-elem (- (+ curr-sum right) left)])
        (values (cons next-elem windows) next-elem))))

  (reverse reversed-windows))

(define (get-rising-windows ls)
  (define (get-rising-count ls)
    (for/sum ([prev ls]
              [next (cdr ls)])
      (if (> next prev) 1 0)))

  (if (< (length ls) 4)
    0
    (let ([windows (make-windows ls 3)])
      (get-rising-count windows))))

(get-rising-windows (get-input-list (current-input-port)))

