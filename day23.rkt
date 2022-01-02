#lang racket

(require racket/hash)
(require data/heap)

(define (counter)
  (define val 0)
  (λ ()
    (define res val)
    (set! val (add1 val))
    res))

(define (create-name char num)
  (string->symbol (string-append char (number->string num))))

;; part 1
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
  (define (first-sym-val sym)
    (for/first ([e es]
                #:when (string=? (extract-sym e) (extract-sym sym)))
      e))

  (or (first-sym-val 'A)
      (first-sym-val 'B)
      (first-sym-val 'C)
      (first-sym-val 'D)))


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
              [j (in-range 4)])
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
                   (get-cycle-top inp c)
                   (λ (n) (+ n 4))
                   0)
      (hash-update moves
                   (apply min-sym (set->list c))
                   (λ (n) (if (zero? n) 2 n))
                   0))))

(define MOVE-VALS
  (hash "A" 1
        "B" 10
        "C" 100
        "D" 1000))

(define (extract-moves moves)
  (for/fold ([new-moves (hash)])
            ([(sym val) (in-hash moves)])
    (hash-update new-moves
                 (extract-sym sym)
                 (λ (n) (+ n val))
                 0)))

(define (get-min-moves inp)
  (for*/fold ([moves (hash "A" 1 "B" 1 "C" 1 "D" 1)])
             ([i (in-range 4)]
              [j (in-range 4)])
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
        [cycle-moves (extract-moves (count-cycle-vals inp cycles))])
    (define combined-moves
      (hash-union min-moves cycle-moves #:combine +))
    (for/sum ([(sym move-count) (in-hash combined-moves)])
      (* (hash-ref MOVE-VALS sym) move-count))))

;; part 2 -> FAILED
;; A totally different algorithm and program is necessary.
;; While I could try and devise another graph-and-cycle based
;; algorithm, I'm opting to just brute force and memoize
;; the input and produce a best answer from that.
(define (read-input port)
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

(define (get-wall inp)
  (for*/fold ([illegal (set)])
             ([i (in-range (length inp))]
              [j (in-range (length (car inp)))])
    (define row (list-ref inp i))
    (if (>= j (length row))
      illegal
      (let ([elem (list-ref row j)])
        (if (or (string=? elem "#")
                (string=? elem " "))
          (set-add illegal (cons i j))
          illegal)))))

(define SYM-IDS
  (hash "A" (counter)
        "B" (counter)
        "C" (counter)
        "D" (counter)))

(define (get-id sym)
  ((hash-ref SYM-IDS sym)))

(define (get-positions inp)
  (define (create-name elem)
    (string->symbol elem))

  (for*/fold ([pos (hash)])
             ([i (in-range (length inp))]
              [j (in-range (length (car inp)))])
    (define row (list-ref inp i))
    (if (>= j (length row))
      pos
      (let ([elem (list-ref row j)])
        (if (hash-has-key? SYM-IDS elem)
          (hash-set pos (cons i j) (create-name elem))
          pos)))))

(define (can-move? pos illegal row col)
  (not (or (set-member? illegal (cons row col))
           (hash-has-key? pos (cons row col)))))

(define (in-possible-moves row col)
  (in-list
    (list (cons (add1 row) col)
          (cons (sub1 row) col)
          (cons row (add1 col))
          (cons row (sub1 col)))))

(define (correct-pos? pos sym row col)
  (and (hash-has-key? pos (cons row col))
       (same-symbol? (hash-ref pos (cons row col))
                     sym)))

(define (finished? pos)
  (and (correct-pos? pos 'A 2 3)
       (correct-pos? pos 'A 3 3)
       (correct-pos? pos 'A 4 3)
       (correct-pos? pos 'A 5 3)
       (correct-pos? pos 'B 2 5)
       (correct-pos? pos 'B 3 5)
       (correct-pos? pos 'B 4 5)
       (correct-pos? pos 'B 5 5)
       (correct-pos? pos 'C 2 7)
       (correct-pos? pos 'C 3 7)
       (correct-pos? pos 'C 4 7)
       (correct-pos? pos 'C 5 7)
       (correct-pos? pos 'D 2 9)
       (correct-pos? pos 'D 3 9)
       (correct-pos? pos 'D 4 9)
       (correct-pos? pos 'D 5 9)))

(define (try-move curr-pos illegal seen moves-so-far sym-pos sym)
  (match-define (cons row col) sym-pos)
  (for/fold ([options '()]
             [seen seen])
            ([new-sym-pos (in-possible-moves row col)]
             #:when (can-move? curr-pos illegal (car new-sym-pos)
                                                (cdr new-sym-pos)))
    (define-values (new-options new-seen)
                   (move-all (hash-set (hash-remove curr-pos sym-pos) new-sym-pos sym)
                             illegal
                             seen
                             (hash-update moves-so-far sym add1 0)))

    (values (append new-options options) new-seen)))


(define (move-all curr-pos illegal seen moves-so-far)
  ;; (print-grid curr-pos)
  (cond
    [(set-member? seen curr-pos) (values '() seen)]
    [(finished? curr-pos) (values (list moves-so-far) seen)]
    [else
     (for/fold ([options '()]
                [seen (set-add seen curr-pos)])
               ([(sym-pos sym) (in-hash curr-pos)])
       (define-values (new-options new-seen)
                      (try-move curr-pos illegal seen moves-so-far sym-pos sym))

       (displayln (length options))
       (values (append new-options options) new-seen))]))

(define (print-grid curr-pos)
  (define (dot-or-value row col)
    (if (hash-has-key? curr-pos (cons row col))
      (extract-sym (hash-ref curr-pos (cons row col)))
      "."))

  (displayln "#############")
  (printf "#~a~a~a~a~a~a~a~a~a~a~a#\n"
          (dot-or-value 1 1)
          (dot-or-value 1 2)
          (dot-or-value 1 3)
          (dot-or-value 1 4)
          (dot-or-value 1 5)
          (dot-or-value 1 6)
          (dot-or-value 1 7)
          (dot-or-value 1 8)
          (dot-or-value 1 9)
          (dot-or-value 1 10)
          (dot-or-value 1 11))

  (printf "###~a#~a#~a#~a###\n"
          (dot-or-value 2 3)
          (dot-or-value 2 5)
          (dot-or-value 2 7)
          (dot-or-value 2 9))

  (printf "  #~a#~a#~a#~a#\n"
          (dot-or-value 3 3)
          (dot-or-value 3 5)
          (dot-or-value 3 7)
          (dot-or-value 3 9))

  (printf "  #~a#~a#~a#~a#\n"
          (dot-or-value 4 3)
          (dot-or-value 4 5)
          (dot-or-value 4 7)
          (dot-or-value 4 9))

  (printf "  #~a#~a#~a#~a#\n"
          (dot-or-value 5 3)
          (dot-or-value 5 5)
          (dot-or-value 5 7)
          (dot-or-value 5 9))

  (displayln "  #########"))


;; part 2 (again)
;; So, DFS doesn't work. Now to try Dijkstra's.
(define MAX-VAL (* 10000 7 14))

(define (all-can-visit)
  (set (cons 1 1)
       (cons 1 2)
       (cons 1 4)
       (cons 1 6)
       (cons 1 8)
       (cons 1 10)
       (cons 1 11)))

(define (sym-can-visit sym)
  (match (extract-sym sym)
    ["A" (set (cons 2 3) (cons 3 3) (cons 4 3) (cons 5 3))]
    ["B" (set (cons 2 5) (cons 3 5) (cons 4 5) (cons 5 5))]
    ["C" (set (cons 2 7) (cons 3 7) (cons 4 7) (cons 5 7))]
    ["D" (set (cons 2 9) (cons 3 9) (cons 4 9) (cons 5 9))]))

(define (find-legal-positions illegal node pos sym)
  (define (any-incorrect-sym? targets)
    (ormap (λ (pos) (and (hash-has-key? node pos)
                         (symbol=? sym (hash-ref node pos))))
           (set->list targets)))

  (define (min-col-pos poses)
    (let loop ([ps (sort (set->list poses) (λ (n1 n2) (< (car n1) (car n2))))]
               [curr-best '()])
      (cond
        [(empty? ps) curr-best]
        [(can-move? node illegal (car (car ps)) (cdr (car ps)))
         (loop (cdr ps) (car ps))]
        [else curr-best])))

  (match-define (cons row col) pos)
  (define target-coords (sym-can-visit sym))

  (define (try-visit-target seen targets curr-pos dist)
    (match-define (cons row col) curr-pos)
    (define pos-info (list row col dist))

    (define curr-set
      (if (set-member? targets curr-pos)
        (set pos-info)
        (set)))

    (if (set-member? seen curr-pos)
      (values (set) seen)
      (for/fold ([to-visit curr-set]
                 [seen (set-add seen curr-pos)])
                ([new-pos (in-possible-moves (car curr-pos) (cdr curr-pos))]
                 #:when (can-move? node illegal (car new-pos) (cdr new-pos)))
        (define-values (new-set new-seen) (try-visit-target seen targets new-pos (add1 dist)))
        (values (set-union to-visit new-set) new-seen))))

  (define min-pos (min-col-pos target-coords))
  (define any-others? (any-incorrect-sym? target-coords))

  (define target-set
    (cond
      [(set-member? target-coords pos) (all-can-visit)]
      [(and (not any-others?) (cons? min-pos) (= row 1)) (set min-pos)]
      [(= row 1) (set)]
      [(and (not any-others?) (cons? min-pos)) (set-add (all-can-visit) min-pos)]
      [else (all-can-visit)]))

  (if (zero? (set-count target-set))
    '()
    (let-values ([(reachable _seen) (try-visit-target (set) target-set pos 0)])
      (set->list reachable))))

(define (score-move sym)
  (hash-ref MOVE-VALS (extract-sym sym)))

(define (get-moves illegal node pos sym)
  ;; options:
  ;; 1. you're in the right column, and everything below you is also right: *no moves*
  ;; 2. you're in the right column, but there's at least one pieces below you that's wrong: *hallway reachable*
  ;; 3. you're in the hallway, but the right column has pieces it shouldn't: *no moves*
  ;; 4. you're in the hallway, the right column has only your own pieces: *one move, end of col*
  ;; 5. you're in the wrong room: *hallway, + your room if no one wrong's in there*
  ;;
  ;; what do I need to compute this?
  ;; 1. determine if you're in the correct column
  ;; 2. determine if you're 'correctly placed'
  ;; 3. travel

  (define (lowest-point col-poses)
    (define sorted (sort (set->list col-poses) #:key car >))
    (for/first ([pos sorted]
                #:when (can-move? node illegal (car pos) (cdr pos)))
      pos))

  (define (only-correct-in-col? col-poses)
    (andmap (λ (pos) (or (not (hash-has-key? node pos))
                         (same-symbol? sym (hash-ref node pos))))
            (set->list col-poses)))

  (define (try-visit seen pos dist)
    (if (set-member? seen pos)
      (values (hash) seen)
      (let ([seen+ (set-add seen pos)])
        (for/fold ([can-visit (hash pos dist)]
                   [seen seen+])
                  ([new-pos (in-possible-moves (car pos) (cdr pos))]
                   #:when (can-move? node illegal (car new-pos) (cdr new-pos)))
          (define-values (new-visit new-seen) (try-visit seen new-pos (add1 dist)))
          (values (hash-union new-visit can-visit) new-seen)))))

  (define (resolve-pos h pos)
    (let ([dist (hash-ref h pos)])
      (list (car pos) (cdr pos) dist)))

  (define (resolve-poses h key-set)
    (for/set ([key key-set]
              #:when (hash-has-key? h key))
      (resolve-pos h key)))

  (match-define (cons row col) pos)
  (define right-col? (= col (col-for-sym sym)))
  (define col-pos-set (sym-can-visit sym))
  (define hall-pos-set (all-can-visit))

  (define (correctly-placed?)
    (and right-col?
         (andmap (λ (pos) (or (<= (car pos) row)
                              (not (hash-has-key? node pos))
                              (= col (col-for-sym (hash-ref node pos)))))
                 (set->list col-pos-set))))

  (define-values (almost-reachable-poses _seen) (try-visit (set) pos 0))
  (define reachable-poses (hash-remove almost-reachable-poses pos))
  (define lowest (lowest-point col-pos-set))

  (cond
    [(correctly-placed?) '()]
    [right-col? (set->list (resolve-poses reachable-poses hall-pos-set))]
    [(and (set-member? hall-pos-set pos)
          (only-correct-in-col? col-pos-set)
          lowest
          (hash-has-key? reachable-poses lowest))
     (list (resolve-pos reachable-poses lowest))]
    [(not (set-member? hall-pos-set pos))
     (let ([hall-poses (resolve-poses reachable-poses hall-pos-set)])
       (if (and (only-correct-in-col? col-pos-set)
                (hash-has-key? reachable-poses lowest))
         (set->list (set-add hall-poses (resolve-pos reachable-poses lowest)))
         (set->list hall-poses)))]
    [else '()]))

(define SCORE-TOO-LARGE 44170)

(define (out-of-place-count lookup)
  (for/sum ([(pos sym) (in-hash lookup)])
    (if (set-member? (sym-can-visit sym) pos)
      0
      1)))

(define (col-for-sym sym)
  (match (extract-sym sym)
    ["A" 3]
    ["B" 5]
    ["C" 7]
    ["D" 9]))

(define (dist-from-col node pos sym)
  (match-define (cons row col) pos)
  (let* ([sym-col (col-for-sym sym)]
         [col-dist (* 2 (abs (- col sym-col)))])
    (cond
      [(= 1 row) (add1 col-dist)]
      [(= col sym-col) 0]
      [else (+ (add1 col-dist) (* (sub1 row) row))])))

(define (total-dist node)
  (for/sum ([(pos sym) (in-hash node)])
    (dist-from-col node pos sym)))

(define dijkstra-count 0)

(define (dijkstra illegal start)
  ;; finished? means we're at the final one

  (define distances (make-hash))
  (define queue (make-heap (λ (v1 v2) (<= (hash-ref distances v1)
                                          (hash-ref distances v2)))))

  (define visited (mutable-set))

  (define (try-visit-neighbors node)
    (when (< (hash-ref distances node) SCORE-TOO-LARGE)
      (when (zero? (modulo dijkstra-count 1000))
        (print-grid node))
      (set! dijkstra-count (add1 dijkstra-count))
      (for ([(pos sym) (in-hash node)])
        (for ([new-pos-info (get-moves illegal node pos sym)])
          (define new-pos (cons (car new-pos-info) (cadr new-pos-info)))
          (define new-node
            (hash-set (hash-remove node pos) new-pos sym))

          (when (not (set-member? visited new-node))
            (define add-to-queue? (not (hash-has-key? distances new-node)))

            (hash-update! distances
                          new-node
                          (λ (v) (min v (+ (* (score-move sym) (caddr new-pos-info))
                                           (hash-ref distances node))))
                          MAX-VAL)

            (when add-to-queue? (heap-add! queue new-node)))))

      (hash-remove! distances node)))


  (define (visit node)
    (cond
      [(finished? node)
       (print-grid node)
       (displayln distances)
       (hash-ref distances node)]
      [else
       (set-add! visited node)
       (try-visit-neighbors node)
       (define min-elem (heap-min queue))
       (heap-remove-min! queue)
       (visit min-elem)]))

  (hash-set! distances start 0)

  (visit start))

;; (define random-deps
;;   (hash 'B1 (set 'C1 'C2 'D3)
;;         'D1 (set 'B1 'A3 'C4 'A4)
;;         'D2 (set 'B1 'A3 'C4 'A4)
;;         'C1 (set 'B3 'B4 'A2)
;;         'C2 (set 'B3 'B4 'A2)
;;         'B2 (set 'D3)
;;         'D3 (set 'B2 'C1 'C2 'A3 'C4 'A4)
;;         'B3 (set 'C1 'C2 'D3)
;;         'B4 (set 'C1 'C2 'D3)
;;         'A2 (set 'B3 'B4 'B1 'D1 'D2)
;;         'D4 (set 'A3 'C4 'A4)
;;         'A3 (set 'D4 'B1 'B2)
;;         'C4 (set 'A3 'D4 'B3 'B4 'A2)
;;         'A4 (set 'C4 'D4 'B1 'D1 'D2)))

;; (define (print-cycles cycles)
;;   (for ([c cycles])
;;     (displayln c)))

;; (define (cycle-appearances cycles)
;;   (for/fold ([res (hash)])
;;             ([c cycles])
;;     (for/fold ([r res])
;;               ([elem c])
;;       (hash-update r elem add1 0))))

;; (define random-inp
;;   (list (list 'B1 'D1 'D2 'A1)
;;         (list 'C1 'C2 'B2 'D3)
;;         (list 'B3 'B4 'A2 'C3)
;;         (list 'D4 'A3 'C4 'A4)))

;; (define cycles (find-all-cycles random-inp random-deps))

;; (print-cycles cycles)
;; (displayln (cycle-appearances cycles))


(let* ([inp (read-input (current-input-port))]
       [illegal (get-wall inp)]
       [node (get-positions inp)])
  (dijkstra illegal node))
  ;; (define m1 (hash-set (hash-remove node (cons 2 3)) (cons 1 11) 'B))
  ;; (define m2 (hash-set (hash-remove m1 (cons 2 9)) (cons 1 10) 'D))
  ;; (print-grid m2)
  ;; (find-legal-positions illegal m2 (cons 3 3) 'D))


;; (let* ([inp (get-input (current-input-port))]
;;        [graph (build-dependency-graph inp)]
;;        [cycles (find-all-cycles inp graph)]
;;        [min-cost (get-min-cost inp cycles)])
;;   (displayln (set-count cycles))
;;   (displayln cycles)
;;   min-cost)
