#lang racket

(require racket/hash)

(define (counter)
  (define val 0)
  (λ ()
    (define res val)
    (set! val (add1 val))
    res))

(define (create-name char num)
  (string->symbol (string-append char (number->string num))))

(define (get-input port)
  (define SYM-IDS
    (hash "A" (counter)
          "B" (counter)
          "C" (counter)
          "D" (counter)))

  (define (get-id sym)
    ((hash-ref SYM-IDS sym)))

  (define (update-pool pools idx char)
    (define sym (create-name char (get-id char)))
    (list-update pools idx (λ (ls) (reverse (cons sym ls)))))

  (define (parse-line line pools)
    (define-values (final-pools _)
      (for/fold ([pools pools]
                 [idx 0])
                ([char (string-split line "#")]
                 #:when (not (string=? (string-trim char) "")))
        (values (update-pool pools idx char)
                (add1 idx))))

    final-pools)

  (read-line port)
  (read-line port)
  (define first-pools
    (parse-line (read-line port) (list '() '() '() '())))

  (parse-line (read-line port) first-pools))

(define IDX-GRAPH
  (hash "A" 0
        "B" 1
        "C" 2
        "D" 3))

(define (extract-sym e)
  (substring (symbol->string e) 0 1))

(define (elem->idx e)
  (hash-ref IDX-GRAPH (extract-sym e)))

(define (same-symbol? e1 e2)
  (string=? (extract-sym e1)
            (extract-sym e2)))

(define (min-sym . es)
  (define (any-sym? sym)
    (ormap (λ (n) (same-symbol? n sym)) es))

  (cond
    [(any-sym? 'A) "A"]
    [(any-sym? 'B) "B"]
    [(any-sym? 'C) "C"]
    [else "D"]))

(define (add-dependency dependencies key val)
  (hash-update dependencies
               key
               (λ (deps) (set-add deps val))
               (set)))

(define (add-above col i dependencies)
  (if (= i (elem->idx (cadr col)))
    dependencies
    (add-dependency dependencies (cadr col) (car col))))

(define (add-in-col elem i j inp dependencies)
  (if (and (= i (elem->idx elem))
           (= j 1))
    dependencies
    (for/fold ([d dependencies])
              ([other (list-ref inp (elem->idx elem))])
      (if (not (same-symbol? elem other))
        (add-dependency d elem other)
        d))))

(define (build-dependency-graph inp)
  (for*/fold ([deps (hash)])
             ([i (in-range 4)]
              [j (in-range 2)])
    (let* ([col (list-ref inp i)]
           [elem (list-ref col j)])

      (add-in-col elem i j inp (add-above col i deps)))))

(define (try-add-cycle cycles path)
  (cond
    [(empty? cycles) (list path)]
    [(subset? (car cycles) path) cycles]
    [(subset? path (car cycles))
     (try-add-cycle (cdr cycles) path)]
    [else (cons (car cycles)
                (try-add-cycle (cdr cycles) path))]))

(define (visit-dependencies graph cycles path curr)
  (for/fold ([c cycles])
            ([dep (hash-ref graph curr '())])
    (find-cycles graph c (set-add path curr) dep)))

(define (find-cycles graph cycles path curr)
  (if (set-member? path curr)
    (try-add-cycle cycles path)
    (visit-dependencies graph cycles path curr)))

(define (find-all-cycles inp graph)
  (for*/fold ([cycles '()])
             ([i (in-range 4)]
              [j (in-range 2)])
    (let* ([col (list-ref inp i)]
           [elem (list-ref col j)])
      (find-cycles graph cycles (set) elem))))

(define (same-col-cycle? inp cycle)
  (and (= (set-count cycle) 2)
       (for/first ([col inp]
                   #:when (set-member? cycle (car col)))
         (set-member? cycle (cadr col)))))

(define (get-cycle-top inp cycle)
  (for/first ([col inp]
              #:when (set-member? cycle (car col)))
    (car col)))

(define (count-cycle-vals inp cycles)
  (for/fold ([moves (hash)])
            ([c cycles])
    (if (same-col-cycle? inp c)
      (hash-update moves
                   (extract-sym (get-cycle-top inp c))
                   (λ (n) (+ n 4))
                   0)
      (hash-update moves
                   (apply min-sym (set->list c))
                   (λ (n) (+ n 2))
                   0))))

(define MOVE-VALS
  (hash "A" 1
        "B" 10
        "C" 100
        "D" 1000))

(define (get-min-moves inp)
  (for*/fold ([moves (hash "A" 1 "B" 1 "C" 1 "D" 1)])
             ([i (in-range 4)]
              [j (in-range 2)])
    (let* ([col (list-ref inp i)]
           [elem (list-ref col j)]
           [desired (elem->idx elem)])
      (cond
        [(and (= i desired)
              (= j 1))
         (hash-update moves (extract-sym elem) sub1)]
        [(= i desired) moves]
        [else
         (hash-update moves
                      (extract-sym elem)
                      (λ (n)
                        (+ n 2 j (* 2 (abs (- i desired))))))]))))

(define (get-min-cost inp cycles)
  (let ([min-moves (get-min-moves inp)]
        [cycle-moves (count-cycle-vals inp cycles)])
    (define combined-moves
      (hash-union min-moves cycle-moves #:combine +))
    (for/sum ([(sym move-count) (in-hash combined-moves)])
      (* (hash-ref MOVE-VALS sym) move-count))))

(let* ([inp (get-input (current-input-port))]
       [graph (build-dependency-graph inp)]
       [cycles (find-all-cycles inp graph)]
       [min-cost (get-min-cost inp cycles)])
  (displayln (set-count cycles))
  (displayln cycles)
  min-cost)
