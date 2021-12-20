#lang racket

;; grid metadata and related functions
(struct metadata (minh minw maxh maxw oob) #:transparent)

(define (init-metadata height width)
  (metadata 0 0 height width 0))

(define (set-min-height mt val)
  (metadata (min (metadata-minh mt) val)
            (metadata-minw mt)
            (metadata-maxh mt)
            (metadata-maxw mt)
            (metadata-oob mt)))

(define (set-min-width mt val)
  (metadata (metadata-minh mt)
            (min (metadata-minw mt) val)
            (metadata-maxh mt)
            (metadata-maxw mt)
            (metadata-oob mt)))

(define (set-max-height mt val)
  (metadata (metadata-minh mt)
            (metadata-minw mt)
            (max (metadata-maxh mt) val)
            (metadata-maxw mt)
            (metadata-oob mt)))

(define (set-max-width mt val)
  (metadata (metadata-minh mt)
            (metadata-minw mt)
            (metadata-maxh mt)
            (max (metadata-maxw mt) val)
            (metadata-oob mt)))

(define (set-oob-val mt val)
  (metadata (metadata-minh mt)
            (metadata-minw mt)
            (metadata-maxh mt)
            (metadata-maxw mt)
            val))

(define (flip-oob mt)
  (if (zero? (metadata-oob mt))
    (set-oob-val mt 1)
    (set-oob-val mt 0)))

(define (update-dimensions mt row col)
  (let* ([mt+ (set-min-height mt row)]
         [mt++ (set-min-width mt+ col)]
         [mt+++ (set-max-height mt++ row)]
         [mt++++ (set-max-width mt+++ col)])
    mt++++))

(define (in-height mt)
  (in-range (sub1 (metadata-minh mt))
            (+ 2 (metadata-maxh mt))))

(define (in-width mt)
  (in-range (sub1 (metadata-minw mt))
            (+ 2 (metadata-maxw mt))))

;; read input
(define (get-input port)
  (define (read-grid)
    (define line (read-line port))
    (if (eof-object? line)
      '()
      (cons (string->list line) (read-grid))))

  (define line (read-line port))
  (define translation-list (string->list line))
  (read-line port)

  (values translation-list (read-grid)))

;; util
(define (pixel->digit p)
  (if (char=? p #\#) 1 0))

(define (digit->pixel d)
  (if (= d 1) #\# #\.))

;; transformation functions
(define (make-translation-lookup translation)
  (list->vector (map pixel->digit translation)))

(define (make-grid-lookup grid height width)
  (for*/hash  ([row (in-range height)]
               [col (in-range width)])
    (values (cons row col)
            (pixel->digit (grid-ref grid row col)))))

;; initial grid util
(define (get-dimensions grid)
  (let ([height (length grid)])
    (if (zero? height)
      (values 0 0)
      (values height (length (car grid))))))

(define (grid-ref grid row col)
  (list-ref (list-ref grid row) col)) 

;; read numbers
(define (get-digit lookup mt row col)
  (if (hash-has-key? lookup (cons row col))
    (hash-ref lookup (cons row col))
    (metadata-oob mt)))

(define (lookup-seq row col)
  (list (cons (sub1 row) (sub1 col))
        (cons (sub1 row) col)
        (cons (sub1 row) (add1 col))
        (cons row (sub1 col))
        (cons row col)
        (cons row (add1 col))
        (cons (add1 row) (sub1 col))
        (cons (add1 row) col)
        (cons (add1 row) (add1 col))))

(define (get-bin lookup mt row col)
  (for/fold ([num 0])
            ([next-pos (lookup-seq row col)])
    (let ([next-digit (get-digit lookup mt (car next-pos) (cdr next-pos))])
      (+ next-digit (* 2 num)))))

(define (translate translator num)
  (vector-ref translator num))

(define (update-dim-by-lookup lookup mt)
  (for/fold ([mt mt])
            ([coord (in-hash-keys lookup)])
    (match-define (cons row col) coord)
    (update-dimensions mt row col)))

(define (flip-oob? translator mt)
  (or (and (zero? (metadata-oob mt))
           (= 1 (translate translator 0)))
      (and (= 1 (metadata-oob mt))
           (zero? (translate translator 511)))))

(define (maybe-flip-oob translator mt)
  (if (flip-oob? translator mt)
    (flip-oob mt)
    mt))

(define (update-metadata lookup translator mt)
  (define new-mt (update-dim-by-lookup lookup mt))
  (maybe-flip-oob translator new-mt))

(define (apply-translation translator lookup mt)
  (define new-lookup
    (for*/hash ([row (in-height mt)]
                [col (in-width mt)])
      (values (cons row col)
              (translate translator (get-bin lookup mt row col)))))

  (values new-lookup (update-metadata new-lookup translator mt)))

(define (apply-translations lookup translator mt n)
  (if (zero? n)
    lookup
    (let-values ([(new-lookup new-mt) (apply-translation translator lookup mt)])
      (apply-translations new-lookup translator new-mt (sub1 n)))))

(define (count-lookup lookup)
  (for/sum ([v (in-hash-values lookup)])
    v))

(let-values ([(translation-list grid) (get-input (current-input-port))])
  (define translator (make-translation-lookup translation-list))
  (define-values (height width) (get-dimensions grid))
  (define lookup (make-grid-lookup grid height width))
  (define mt (init-metadata height width))

  (count-lookup (apply-translations lookup translator mt 50)))
