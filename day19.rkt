#lang racket

(define (coord-or-dist? e)
  (and (list? e)
       (= (length e) 3)
       (andmap number? e)))

(define (abs-dist d)
  (map abs d))

(define (same-dist? d1 d2)
  (equal? (list->set (abs-dist d1))
          (list->set (abs-dist d2))))

(define (flip n) (* -1 n))

(define (add-pts pt1 pt2)
  (list (+ (car pt1) (car pt2))
        (+ (cadr pt1) (cadr pt2))
        (+ (caddr pt1) (caddr pt2))))

(define (subtract-pts pt1 pt2)
  (list (- (car pt1) (car pt2))
        (- (cadr pt1) (cadr pt2))
        (- (caddr pt1) (caddr pt2))))

(struct graph (points dists point-resolver poses) #:transparent)

(define (make-graph)
  (graph (set) (hash) (hash) (set '(0 0 0))))

(define (coord-dist c1 c2)
  (for/list ([cmp1 c1]
             [cmp2 c2])
    (- cmp2 cmp1)))

(define (graph-member? graph pt)
  (set-member? (graph-points graph) pt))

(define (graph-add-point graph pt)
  (if (graph-member? graph pt)
    graph
    (for/fold ([g graph])
              ([other (in-graph-points graph)])
      (add-graph-edge g pt other))))

(define (add-directed-edge g c1 c2)
  (define dist (coord-dist c1 c2))
  (define new-points
    (set-add (set-add (graph-points g) c1) c2))

  (define new-dists
    (hash-update (graph-dists g)
                 c1
                 (λ (neighbors) (set-add neighbors (coord-dist c1 c2)))
                 (set)))

  (define new-resolver
    (hash-set (graph-point-resolver g)
              (cons c1 dist)
              c2))

  (graph new-points new-dists new-resolver (graph-poses g)))

(define (add-graph-edge g c1 c2)
  (let ([first-dir (add-directed-edge g c1 c2)])
    (add-directed-edge first-dir c2 c1)))

(define (in-graph-points g)
  (in-set (graph-points g)))

(define (in-pt-dists g pt)
  (in-set (hash-ref (graph-dists g) pt)))

(define (in-graph-dists g)
  (in-hash (graph-dists g)))

(define (in-graph-dists-vals g)
  (in-hash-values (graph-dists g)))

(define (edge-intersect e1 e2)
  (for*/set ([d1 (in-set e1)]
             [d2 (in-set e2)]
             #:when (same-dist? d1 d2))
    d1))

(define (overlapping? e1 e2)
  (>= (set-count (edge-intersect e1 e2)) 11))

(define (get-to graph from dist)
  (hash-ref (graph-point-resolver graph)
            (cons from dist)))

(define (get-any-dist graph pt)
  (set-first (hash-ref (graph-dists graph) pt)))

(define (resolve-points graph start-pt edges)
  (define resolved-pts
    (for/set ([e edges])
      (get-to graph start-pt e)))

  (set-add resolved-pts start-pt))

(define (add-new-graph-pos g pos)
  (graph (graph-points g)
         (graph-dists g)
         (graph-point-resolver g)
         (set-add (graph-poses g) pos)))

(define (create-point-align-func pt1 pt2)
  (define (get-selector elem pt)
    (cond
      [(eq? (abs elem) (abs (car pt))) car]
      [(eq? (abs elem) (abs (cadr pt))) cadr]
      [(eq? (abs elem) (abs (caddr pt))) caddr]
      [else (error "BAD")]))

  (define (get-orientation pt1 pt2)
    (let ([fst (get-selector (car pt1) pt2)]
          [snd (get-selector (cadr pt1) pt2)]
          [thd (get-selector (caddr pt1) pt2)])
      (λ (pt)
         (list (fst pt)
               (snd pt)
               (thd pt)))))

  (define (get-direction target other)
    (if (eq? target other)
      identity
      flip))

  (define (get-axes pt1 pt2)
    (let ([fst (get-direction (car pt1) (car pt2))]
          [snd (get-direction (cadr pt1) (cadr pt2))]
          [thd (get-direction (caddr pt1) (caddr pt2))])
    (λ (pt)
       (list (fst (car pt))
             (snd (cadr pt))
             (thd (caddr pt))))))

  (define orienter (get-orientation pt1 pt2))
  (define axis-fix (get-axes pt1 (orienter pt2)))

  (λ (pt)
     (axis-fix (orienter pt))))

(define (add-graph g1 start1 g2 start2)
  (define (get-alignment-from-dists)
    (match-define (cons g1-dist g2-dist)
      (for*/first ([d1 (in-pt-dists g1 start1)]
                   [d2 (in-pt-dists g2 start2)]
                  #:when (same-dist? d1 d2))
        (cons d1 d2)))

    (create-point-align-func g1-dist g2-dist))

  (define pt-align-fn (get-alignment-from-dists))
  (define g2-pos (subtract-pts start1 (pt-align-fn start2)))

  (define updated-g1 (add-new-graph-pos g1 g2-pos))

  (for/fold ([graph updated-g1])
            ([pt (in-graph-points g2)])
    (define aligned-pt (pt-align-fn pt))
    (graph-add-point graph (add-pts g2-pos aligned-pt))))

(define (intersect-graphs g1 g2)
  (define intersection-result
    (for*/first ([(k1 edges1) (in-graph-dists g1)]
                 [(k2 edges2) (in-graph-dists g2)]
                 #:when (overlapping? edges1 edges2))
      (cons k1 k2)))

  (if intersection-result
    (match-let ([(cons s1 s2) intersection-result])
      (values (add-graph g1 s1 g2 s2) #t))
    (values g1 #f)))

(define (graph-size g)
  (set-count (graph-points g)))


(define (get-input port)
  (define (stop-reading? line)
    (or (eof-object? line)
        (= (string-length line) 0))) 

  (define (parse-coord line)
    (let ([str-coords (string-split line ",")])
      (map string->number str-coords)))

  (define (get-scanner)
    (define line (read-line port))
    (if (stop-reading? line)
      '()
      (cons (parse-coord line) (get-scanner))))

  (define (get-scanners)
    (define line (read-line port))
    (if (eof-object? line)
      '()
      (let ([scanner (get-scanner)])
        (cons scanner (get-scanners)))))

  (get-scanners))

(define (scanner->graph scanner)
  (define (add-coord-dists coord other-coords graph)
    (for/fold ([g graph])
              ([c other-coords])
      (add-graph-edge g coord c)))

  (define (graph-dists coords graph)
    (if (empty? coords)
      graph
      (let ([new-g (add-coord-dists (car coords) (cdr coords) graph)])
        (graph-dists (cdr coords) new-g))))

  (graph-dists scanner (make-graph)))

(define (combine-graphs graphs)
  (define (combine-graph g others)
    (cond
      [(empty? others) (values g '())]
      [else
       (define-values (new-g combined?) (intersect-graphs g (car others)))
       (if combined?
         (values new-g (cdr others))
         (let-values ([(new-g new-others) (combine-graph g (cdr others))])
           (values new-g (cons (car others) new-others))))]))

  (define (combine-until-empty g others)
    (if (empty? others)
      g
      (let-values ([(new-g new-others) (combine-graph g others)])
        (combine-until-empty new-g new-others))))

  (combine-until-empty (car graphs) (cdr graphs)))

(define (manhattan-axis v1 v2)
  (abs (- v1 v2)))

(define (manhattan-dist pt1 pt2)
  (+ (manhattan-axis (car pt1) (car pt2))
     (manhattan-axis (cadr pt1) (cadr pt2))
     (manhattan-axis (caddr pt1) (caddr pt2))))

(define (max-manhattan pts)
  (for*/fold ([res 0])
             ([pt1 pts]
              [pt2 pts]
              #:when (not (equal? pt1 pt2)))
    (max res (manhattan-dist pt1 pt2))))
             

(let* ([scanners (get-input (current-input-port))]
       [graphs (map scanner->graph scanners)])
  (define new-graph (combine-graphs graphs))
  (max-manhattan (set->list (graph-poses new-graph))))
