#lang racket

(define (get-input port)
  (define (get-nums-from-line line)
    (for/list ([n (string-split line "")]
               #:when (not (string=? n "")))
      (string->number n)))

  (let loop ([grid '()])
    (define line (read-line port))
    (if (eof-object? line)
      grid
      (loop (cons (get-nums-from-line line) grid)))))

(define (get-dimensions grid)
  (let ([height (length grid)])
    (if (zero? height)
      (values 0 0)
      (values height (length (car grid))))))

(define (get-val grid row col)
  (list-ref (list-ref grid row) col))

(define (out-of-bounds? row col height width)
  (or (< row 0)
      (< col 0)
      (>= row height)
      (>= col width)))

(define (smaller? grid n row col height width)
  (or (out-of-bounds? row col height width)
      (< n (get-val grid row col))))

(define (low-point? grid row col height width)
  (let ([n (get-val grid row col)])
    (and (smaller? grid n (add1 row) col height width)
         (smaller? grid n (sub1 row) col height width)
         (smaller? grid n row (add1 col) height width)
         (smaller? grid n row (sub1 col) height width))))

(define (risk-level grid row col)
  (add1 (get-val grid row col)))

(define (get-risk-levels grid)
  (define-values (height width) (get-dimensions grid))
  (for/sum ([row (in-range height)])
    (for/sum ([col (in-range width)]
              #:when (low-point? grid row col height width))
      (risk-level grid row col))))

(define (get-basins grid)
  (define-values (height width) (get-dimensions grid))

  (define (in-basin? target row col visited)
    (if (not (out-of-bounds? row col height width))
      (let ([n (get-val grid row col)])
        (and (not (= n 9)) (<= target n)))
      #f))

  (define (visit-pos target row col visited)
    (cond
      [(set-member? visited (cons row col))
       visited]
      [(in-basin? target row col visited)
       (let ([new-visited (set-add visited (cons row col))])
         (visit-neighbors row col new-visited))]
      [else visited]))

  (define (visit-neighbors row col visited)
    (let* ([target (get-val grid row col)]
           [v+ (visit-pos target (add1 row) col visited)]
           [v++ (visit-pos target (sub1 row) col v+)]
           [v+++ (visit-pos target row (add1 col) v++)]
           [v++++ (visit-pos target row (sub1 col) v+++)])
      v++++))

  (define (basin-size visited)
    (set-count visited))

  (define (largest-three basins)
    (define (get-max ls)
      (let* ([res (apply max ls)]
             [rem (remove res ls)])
        (values res rem)))

    (let*-values ([(fst rem) (get-max basins)]
                  [(snd rem) (get-max rem)]
                  [(thd rem) (get-max rem)])
      (list fst snd thd)))

  (define (basins/acc grid)
    (for*/fold ([basins '()])
               ([row (in-range height)]
                [col (in-range width)]
                #:when (low-point? grid row col height width))
      (let ([visited (set (cons row col))])
        (cons (basin-size (visit-neighbors row col visited))
              basins))))

  (apply * (largest-three (basins/acc grid))))


(get-basins (get-input (current-input-port)))
