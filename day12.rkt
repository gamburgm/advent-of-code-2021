#lang racket

(define (get-input port)
  (define line (read-line port))
  (if (eof-object? line)
    '()
    (cons
      (string-split line "-")
      (get-input port))))

(define (add-directed-edge graph src dst)
  (hash-update graph src (λ (ls) (cons dst ls)) '()))

(define (add-edge graph n1 n2)
  (let ([new-graph (add-directed-edge graph n1 n2)])
    (add-directed-edge new-graph n2 n1)))

(define (build-graph mapping)
  (for/fold ([graph (hash)])
            ([m mapping])
    (match-define (list n1 n2) m)
    (add-edge graph n1 n2)))

(define (big-cave? cave)
  (andmap char-upper-case? (string->list cave)))

(define (small-cave? cave)
  (not (big-cave? cave)))

(define (end? cave)
  (string=? cave "end"))

(define (can-double? node doubled?)
  (and (not doubled?)
       (not (string=? node "start"))))

(define (count-unique-paths graph)
  (define (count/acc node visited doubled?)
    (cond
      [(end? node) 1]
      [(set-member? visited node)
       (if (can-double? node doubled?)
         (visit-neighbors node visited #t)
         0)]
      [else (visit-neighbors node visited doubled?)]))

  (define (visit-neighbors node visited doubled?)
    (define new-visited
      (if (small-cave? node) (set-add visited node) visited))

    (for/sum ([neighbor (hash-ref graph node '())])
      (count/acc neighbor new-visited doubled?)))

  (visit-neighbors "start" (set) #f))


(let* ([input (get-input (current-input-port))]
       [graph (build-graph input)]
       [res (count-unique-paths graph)])
  res)
