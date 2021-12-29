#lang racket

(define (get-input port)
  (define (parse-line line)
    (for/list ([c (string-split line "")]
               #:when (not (string=? c "")))
      c))

  (define (get)
    (define line (read-line port))
    (if (eof-object? line)
      '()
      (cons (parse-line line) (get))))

  (get))

(define (get-dimensions grid)
  (let ([height (length grid)])
    (if (zero? height)
      (values 0 0)
      (values height (length (car grid))))))

(define (grid-get grid row col)
  (list-ref (list-ref grid row) col))

(define (cucumber? c)
  (or (string=? c ">")
      (string=? c "v")))

(define (cucumber->sym c)
  (match c
    [">" 'east]
    ["v" 'south]
    [_ #f]))

(define (make-lookup grid height width)
  (for*/fold ([lookup (hash)])
             ([i (in-range height)]
              [j (in-range width)])
    (define elem (grid-get grid i j))
    (if (cucumber? elem)
      (hash-set lookup
                (cons i j)
                (cucumber->sym elem))
      lookup)))

(define (available-pos? lookup new-pos)
  (not (hash-has-key? lookup new-pos)))

(define (move lookup direction modify-pos)
  (for/hash ([(pos state) (in-hash lookup)])
    (if (and (symbol=? direction state)
             (available-pos? lookup (modify-pos pos)))
      (values (modify-pos pos) state)
      (values pos state))))

(define (inc val mx)
  (modulo (add1 val) mx))

(define (move-east lookup height width)
  (define (modify-pos pair)
    (cons (car pair) (inc (cdr pair) width)))

  (move lookup 'east modify-pos))

(define (move-south lookup height width)
  (define (modify-pos pair)
    (cons (inc (car pair) height) (cdr pair)))

  (move lookup 'south modify-pos))

(define (count-steps lookup height width)
  (let loop ([lookup lookup]
             [n 0])
    (let* ([east-look (move-east lookup height width)]
           [south-look (move-south east-look height width)]
           [new-n (add1 n)])
      (if (equal? lookup south-look)
        new-n
        (loop south-look new-n)))))

(let ([grid (get-input (current-input-port))])
  (define-values (height width) (get-dimensions grid))
  (define lookup (make-lookup grid height width))
  (count-steps lookup height width))
