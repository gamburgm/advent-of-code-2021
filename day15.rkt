#lang racket

(require profile)

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

(define (set-grid grid row col val)
  (list-update grid
               row
               (λ (r) (list-update r col (λ (_) val)))))

(define (val-grid height width val)
  (for/list ([_row-idx (in-range height)])
    (for/list ([_col-idx (in-range width)])
      val)))

(define (false-grid height width)
  (val-grid height width #f))

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
  (- lowest (get grid 0 0)))



(define (lowest-risk-dp grid)
  (define-values (height width) (get-dimensions grid))
  (define target-row (sub1 height))
  (define target-col (sub1 width))

  (define start-dp
    (set-grid (false-grid height width)
              target-row
              target-col
              0))

  (define (get-result dp row col)
    (if (out-of-bounds? row col height width)
      #f
      (let ([v (get dp row col)])
        (and v (+ v (get grid row col))))))

  (define (min-dp . vals)
    (define remaining (filter (λ (v) (not (false? v))) vals))
    (if (empty? remaining)
      #f
      (apply min remaining)))

  (define (inc-step old-dp)
    (for/list ([row (in-range height)])
      (for/list ([col (in-range width)])
        (min-dp (get old-dp row col)
                (get-result old-dp (add1 row) col)
                (get-result old-dp (sub1 row) col)
                (get-result old-dp row (add1 col))
                (get-result old-dp row (sub1 col))))))

  (define (find-lowest old-dp)
    (let ([new-dp (inc-step old-dp)])
      (if (equal? new-dp old-dp)
        old-dp
        (find-lowest new-dp))))

  (define (get-answer dp)
    (get dp 0 0))

  (get-answer (find-lowest start-dp)))

(define (lowest-risk-dp! grid)
  (define-values (height width) (get-dimensions grid))
  (define target-row (sub1 height))
  (define target-col (sub1 width))

  (define (grid-to-vec grid)
    (for/vector ([row grid])
      (for/vector ([val row])
        val)))

  (define grid-vec (grid-to-vec grid))

  (define (val-grid-vector val)
    (for/vector ([_row-idx (in-range height)])
      (for/vector ([_col-idx (in-range width)])
        val)))

  (define (false-grid-vector)
    (val-grid-vector #f))

  (define (get-vec vec row col)
    (vector-ref (vector-ref vec row) col))

  (define (set-vec! vec row col val)
    (vector-set! (vector-ref vec row)
                 col
                 val))

  (define start-dp (false-grid-vector))
  (set-vec! start-dp target-row target-col 0)

  (define (get-result dp row col)
    (if (out-of-bounds? row col height width)
      #f
      (let ([v (get-vec dp row col)])
        (and v (+ v (get-vec grid-vec row col))))))

  (define (min-dp . vals)
    (define remaining (filter (λ (v) (not (false? v))) vals))
    (if (empty? remaining)
      #f
      (apply min remaining)))

  (define (inc-step! old-dp new-dp)
    (for ([row (in-range height)])
      (for ([col (in-range width)])
        (define best
          (min-dp (get-vec old-dp row col)
                  (get-result old-dp (add1 row) col)
                  (get-result old-dp (sub1 row) col)
                  (get-result old-dp row (add1 col))
                  (get-result old-dp row (sub1 col))))

        (set-vec! new-dp row col best))))

  (define (find-lowest old-dp new-dp n)
    (inc-step! old-dp new-dp)
    (if (equal? new-dp old-dp)
      old-dp
      (find-lowest new-dp old-dp (add1 n))))

  (define (get-answer dp)
    (get-vec dp 0 0))

  (get-answer (find-lowest start-dp (false-grid-vector) 1)))


(define grid (get-input (current-input-port)))
(define full-grid (generate-full-grid grid))
(lowest-risk-dp! full-grid)

