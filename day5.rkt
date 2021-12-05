#lang racket

(define (get-coords line)
  (define elems (string-split line " "))
  (define start (car elems))

  (define start-coords (map string->number (string-split start ",")))

  (define end (caddr elems))
  (define end-coords (map string->number (string-split end ",")))

  (list start-coords end-coords))

(define (parse-lines port)
  (define next (read-line port))
  (if (eof-object? next)
    '()
    (cons (get-coords next) (parse-lines port))))

(define (get-overlapping-coords lines)
  (define (x1 line)
    (car (car line)))
  
  (define (y1 line)
    (cadr (car line)))

  (define (x2 line)
    (car (cadr line)))

  (define (y2 line)
    (cadr (cadr line)))

  (define (x-eq? line)
    (= (x1 line) (x2 line)))

  (define (y-eq? line)
    (= (y1 line) (y2 line)))

  (define coord-counts (hash))

  (define (traverse-straight-path fixed c1 c2 make-coord counts)
    (define low (min c1 c2))
    (define high (max c1 c2))

    (for/fold ([counts counts])
              ([c (in-range low (add1 high))])
      (hash-update counts (make-coord fixed c) add1 0)))

  (define (traverse-diagonal-path x1 x2 y1 y2 counts)
    (define x-rev? (> x1 x2))
    (define y-rev? (> y1 y2))

    (for/fold ([counts counts])
              ([x (in-range x1 (if x-rev? (sub1 x2) (add1 x2)) (if x-rev? -1 1))]
               [y (in-range y1 (if y-rev? (sub1 y2) (add1 y2)) (if y-rev? -1 1))])
      (hash-update counts (cons x y) add1 0)))

  (define (traverse-line line counts)
    (cond
      [(x-eq? line)
       (traverse-straight-path (x1 line)
                               (y1 line)
                               (y2 line)
                               cons
                               counts)]
      [(y-eq? line)
       (traverse-straight-path (y1 line)
                               (x1 line)
                               (x2 line)
                               (λ (n1 n2) (cons n2 n1))
                               counts)]
      [else 
        (traverse-diagonal-path (x1 line)
                                (x2 line)
                                (y1 line)
                                (y2 line)
                                counts)]))

  (define (traverse-lines lines counts)
    (for/fold ([counts counts])
              ([l lines])
      (traverse-line l counts)))

  (define (count-overlapping-coords counts)
    (apply + (map (λ (n) (if (> n 1) 1 0)) (hash-values counts))))

  (count-overlapping-coords (traverse-lines lines coord-counts)))

(get-overlapping-coords (parse-lines (current-input-port)))




