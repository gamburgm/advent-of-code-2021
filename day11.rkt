#lang racket

(define (get-input port)
  (define (parse-line line)
    (for/list ([n (string-split line "")]
               #:when (not (string=? n "")))
      (string->number n)))

  (define (help)
    (let ([line (read-line port)])
      (if (eof-object? line)
        '()
        (cons (parse-line line) (help)))))

  (help))

(define (get-dimensions grid)
  (let ([height (length grid)])
    (if (zero? height)
      (values 0 0)
      (values height (length (car grid))))))

(define (flash? n)
  (> n 9))

(define (out-of-bounds? row col height width)
  (or
    (< row 0)
    (< col 0)
    (>= row height)
    (>= col width)))

(define (get grid row col)
  (list-ref (list-ref grid row) col))

(define (inc grid row col height width stack resolved)
  (define (help)
    (define new-grid
      (list-update grid
                  row
                  (λ (r) (list-update r col add1))))

    (define coord (cons row col))

    (define-values (new-stack new-resolved)
      (if (and (not (set-member? resolved coord))
               (flash? (get new-grid row col)))
        (values (cons coord stack) (set-add resolved coord))
        (values stack resolved)))

    (values new-grid new-stack new-resolved))

  (if (out-of-bounds? row col height width)
    (values grid stack resolved)
    (help)))

(define (take-step grid height width)
  (define (inc-grid grid)
    (for/list ([row grid])
      (for/list ([n row])
        (add1 n))))

  (define (find-eligible grid)
    (for*/fold ([coords '()])
               ([row (in-range height)]
                [col (in-range width)])
      (if (flash? (get grid row col))
        (cons (cons row col) coords)
        coords)))

  (define (zero-out-flashes grid)
    (for/list ([row grid])
      (for/list ([n row])
        (if (flash? n) 0 n))))

  (define (resolve-flash grid coord to-resolve resolved)
    (match-define (cons row col) coord) 

    (let*-values ([(g stack res) (inc grid (add1 row) col height width to-resolve resolved)]
                  [(g stack res) (inc g (sub1 row) col height width stack res)]
                  [(g stack res) (inc g row (add1 col) height width stack res)]
                  [(g stack res) (inc g row (sub1 col) height width stack res)]
                  [(g stack res) (inc g (add1 row) (add1 col) height width stack res)]
                  [(g stack res) (inc g (add1 row) (sub1 col) height width stack res)]
                  [(g stack res) (inc g (sub1 row) (add1 col) height width stack res)]
                  [(g stack res) (inc g (sub1 row) (sub1 col) height width stack res)])
      (values g stack res)))

  (define (resolve-and-count-flashes grid to-resolve resolved)
    (cond
      [(empty? to-resolve)
       (values (zero-out-flashes grid)
               (set-count resolved))]
      [else
        (call-with-values
          (λ () (resolve-flash grid (car to-resolve) (cdr to-resolve) resolved))
          resolve-and-count-flashes)]))


  (define new-grid (inc-grid grid))
  (define eligible (find-eligible new-grid))
  (resolve-and-count-flashes new-grid eligible (list->set eligible)))

(define (print-grid grid)
  (for ([row grid])
    (for ([n row])
      (display n))
    (displayln "")))

;; for part 1, get rid of let/cc and return the final ending count that's accumulated.
(define (play grid steps)
  (let/cc k
    (define-values (height width) (get-dimensions grid))
    (define-values (_final-g cnt)
      (for/fold ([g grid]
                 [cnt 0])
                ([step (in-range steps)])
        (define-values (new-g flashed) (take-step g height width))
        (when (= (* height width) flashed)
          (k (add1 step)))
        (values new-g (+ cnt flashed))))
    (error "complete flash never identified")))

(play (get-input (current-input-port)) 100000)
