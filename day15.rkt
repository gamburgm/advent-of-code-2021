#lang racket

(define (get-input port)
  (define (parse-line line)
    (for/list ([n (string-split line "")]
               #:when (not (string=? n "")))
      (string->number n)))

  (define (get/acc)
    (define line (read-line port))
    (if (eof-object? line)
      '()
      (cons (parse-line line) (get/acc))))

  (get/acc))

(define (get-dimensions grid)
  (let ([height (length grid)])
    (if (zero? height)
      (values 0 0)
      (values height (length (car grid))))))

(define (out-of-bounds? row col height width)
  (or (< row 0)
      (< col 0)
      (>= row height)
      (>= col width)))

(define (get grid row col)
  (list-ref (list-ref grid row) col))

(define (target? coord target)
  (and (= (car coord) (car target))
       (= (cdr coord) (cdr target))))


#;(define (lowest-risk grid)
  (define-values (height width) (get-dimensions grid))
  (define MAX-NUM (add1 (* height width 9)))
  (define target (cons (sub1 height) (sub1 width)))

  ;; returning seen might be useless
  (define (get-lowest-for-point row col seen known)
    (define coord (cons row col))

    (cond
      [(out-of-bounds? row col height width)
       (values MAX-NUM known)]
      [(target? coord target)
       (values (get grid row col) known)]
      ;; ordering of these next two clauses?
      [(set-member? seen coord)
       (values MAX-NUM known)]
      [(hash-has-key? known coord)
       (values (hash-ref known coord) known)]
      [else
       (define seen+ (set-add seen coord))
       (define-values (top-lowest known+)
                      (get-lowest-for-point (add1 row) col seen+ known))
       (define-values (left-lowest known++)
                      (get-lowest-for-point (sub1 row) col seen+ known+))
       (define-values (right-lowest known+++)
                      (get-lowest-for-point row (add1 col) seen+ known++))
       (define-values (bot-lowest known++++)
                      (get-lowest-for-point row (sub1 col) seen+ known+++))

       (define lowest-score
         (+ (get grid row col)
            (min top-lowest left-lowest right-lowest bot-lowest)))

       (define final-known
         (hash-set known++++ coord lowest-score))

       (values lowest-score final-known)]))

  (define-values (score _known)
                 (get-lowest-for-point 0 0 (set) (hash)))

  (displayln _known)
  (displayln MAX-NUM)
  score)

(define (generate-full-grid grid)
  (define (modify-num n modifier)
    (let ([new-num (+ n modifier)])
      (if (= new-num 9)
        new-num
        (modulo new-num 9))))

  (define (modify-row row modifier)
    (map (λ (n) (modify-num n modifier)) row))

  (define (modify-grid grid modifier)
    (map (λ (row) (modify-row row modifier)) grid))

  (define (generate-right-grids grid)
    (for/list ([row grid])
      (let* ([fst (modify-row row 1)]
             [snd (modify-row row 2)]
             [thd (modify-row row 3)]
             [frt (modify-row row 4)])
        (append row fst snd thd frt))))

  (define (generate-bot-grids grid)
    (append grid
            (modify-grid grid 1)
            (modify-grid grid 2)
            (modify-grid grid 3)
            (modify-grid grid 4)))

  (generate-bot-grids (generate-right-grids grid)))

(define (print-grid grid)
  (for ([row grid])
    (for ([n row])
      (display n))
    (displayln "")))

(define (lowest-risk grid)
  (define-values (height width) (get-dimensions grid))
  (define target (cons (sub1 height) (sub1 width)))
  (define target-score (get grid (sub1 height) (sub1 width)))
  (define MAX-NUM (add1 (* height width 9)))

  (define (lowest-for-coord row col cache)
    (define coord (cons row col))
    (cond
      [(out-of-bounds? row col height width)
       (values MAX-NUM cache)]
      [(target? coord target)
       (values target-score cache)]
      [(hash-has-key? cache coord)
       (values (hash-ref cache coord) cache)]
      [else
       (let*-values ([(bot-low cache+) (lowest-for-coord (add1 row) col cache)]
                     [(right-low cache++) (lowest-for-coord row (add1 col) cache+)])
         (define lowest (+ (get grid row col) (min bot-low right-low)))
         (values lowest (hash-set cache++ coord lowest)))]))

  (define-values (lowest _cache) (lowest-for-coord 0 0 (hash)))
  (displayln height)
  (displayln width)
  (- lowest (get grid 0 0)))

(define (lowest-risk-dp grid)
  (define-values (height width) (get-dimensions grid))
  ;; fill out grid with empty
  ;; replace bottom-right with the grid value
  ;; iterate up one step at a time until you get to like 500 probably, something in that ballpark
  ;; return
  ;; could iterate to a fixed point LOL
  ;; also, have two functions: one that adds the entered cell to the value, and one that visits the cell.
  ;;   with that recursive structure, I can call the second function as the entry point and don't have
  ;;   to worry about subtracting (0 0) at the end.


(define grid (get-input (current-input-port)))
(define-values (height width) (get-dimensions grid))
(displayln height)
(displayln width)
(define full-grid (generate-full-grid grid))

(lowest-risk full-grid)

