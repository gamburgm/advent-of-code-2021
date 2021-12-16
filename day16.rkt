#lang racket

(struct node (ver) #:transparent)
(struct literal node (num) #:transparent)
(struct operator node (op-type nodes) #:transparent)

(define (get-input port)
  (string->list (read-line port)))

(define HEX-TRANSLATE
  (hash #\0 '(0 0 0 0)
        #\1 '(0 0 0 1)
        #\2 '(0 0 1 0)
        #\3 '(0 0 1 1)
        #\4 '(0 1 0 0)
        #\5 '(0 1 0 1)
        #\6 '(0 1 1 0)
        #\7 '(0 1 1 1)
        #\8 '(1 0 0 0)
        #\9 '(1 0 0 1)
        #\A '(1 0 1 0)
        #\B '(1 0 1 1)
        #\C '(1 1 0 0)
        #\D '(1 1 0 1)
        #\E '(1 1 1 0)
        #\F '(1 1 1 1)))

(define (hex->bin hexstr)
  (cond
    [(empty? hexstr) '()]
    [else (append (hash-ref HEX-TRANSLATE (car hexstr))
                  (hex->bin (cdr hexstr)))]))

(define (parser-error msg . ctx)
  (printf "Parser Error - ~a: ~a\n" msg ctx))

(define (evaluator-error msg . ctx)
  (printf "Evaluator Error - ~a: ~a\n" msg ctx))

(define (bin->int bin)
  (for/fold ([int 0])
            ([b bin])
    (+ b (* 2 int))))

(define (consume str len)
  (cond
    [(> len (length str))
     (parser-error "Attempted to read more than was available\n" str len)
     (exit 1)]
    [else
     (split-at str len)]))

(define (parse bin)
  ;; get version number
  (define-values (ver-str ver-rst) (consume bin 3))
  (define ver-num (bin->int ver-str))

  ;; get type
  (define-values (type-str type-rst) (consume ver-rst 3))
  (define type (bin->int type-str))

  (match type
    [4 (parse-num ver-num type-rst)]
    [_ (parse-op ver-num type type-rst)]))

(define (parse-num ver bin)
  (define (next-num? bin-str)
    (not (zero? (car bin-str))))

  (define (read-next-num bin)
    (define-values (has-next-num-str num-rst) (consume bin 1))
    (define has-next-num (next-num? has-next-num-str))
    (define-values (next-num bin-rst) (consume num-rst 4))
    (values has-next-num next-num bin-rst))

  (define (read-num bin)
    (define-values (has-next-num num bin-rst) (read-next-num bin))
    (cond
      [has-next-num
       (define-values (rst-num rst) (read-num bin-rst))
       (values (append num rst-num) rst)]
      [else (values num bin-rst)]))

  (define-values (num rst) (read-num bin))
  (values (literal ver (bin->int num))
          rst))

(define (parse-op ver op-type bin)
  (define (zero-len-type? len-type-str)
    (zero? (car len-type-str)))

  (define (read-n-packets bin n)
    (cond
      [(zero? n) (values '() bin)]
      [else
       (define-values (nod nod-rst) (parse bin))
       (define-values (rst-nods rst) (read-n-packets nod-rst (sub1 n)))
       (values (cons nod rst-nods) rst)]))

  (define (read-len-packets bin len)
    (cond
      [(zero? len) (values '() bin)]
      [else
       (define-values (nod nod-rst) (parse bin))
       (define read-len (- (length bin) (length nod-rst)))

       (define-values (rst-nods rst)
                      (read-len-packets nod-rst (- len read-len)))
       (values (cons nod rst-nods) rst)]))

  (define (get-n bin)
    (define-values (n-str rst) (consume bin 11))
    (values (bin->int n-str) rst))

  (define (get-len bin)
    (define-values (len-str rst) (consume bin 15))
    (values (bin->int len-str) rst))

  (define (get-nodes bin is-zero)
    (cond
      [is-zero
       (define-values (len rst) (get-len bin))
       (read-len-packets rst len)]
      [else
       (define-values (n rst) (get-n bin))
       (read-n-packets rst n)]))

  (define-values (length-type-str len-rst) (consume bin 1))
  (define is-zero (zero-len-type? length-type-str))

  (define-values (nods rst) (get-nodes len-rst is-zero))
  (values (operator ver op-type nods) rst))

(define (sum-ver-nums nod)
  (match nod
    [(literal ver _) ver]
    [(operator ver _ nods)
     (+ ver
        (for/sum ([n nods])
          (sum-ver-nums n)))]))

(define (eval-node nod)
  (define (eval-op type nods)
    (define nodes-res (map eval-node nods))
    (match type
      [0 (apply + nodes-res)]
      [1 (apply * nodes-res)]
      [2 (apply min nodes-res)]
      [3 (apply max nodes-res)]
      [5
       (unless (= (length nodes-res) 2)
         (evaluator-error "greater-than node can only have two sub-packets"
                          nodes-res)) 

       (if (> (car nodes-res) (cadr nodes-res)) 1 0)]
      [6
       (unless (= (length nodes-res) 2)
         (evaluator-error "less-than node can only have two sub-packets"
                          nodes-res)) 

       (if (< (car nodes-res) (cadr nodes-res)) 1 0)]
      [7
       (unless (= (length nodes-res) 2)
         (evaluator-error "equal-to node can only have two sub-packets"
                          nodes-res)) 

       (if (= (car nodes-res) (cadr nodes-res)) 1 0)]))

  (match nod
    [(literal _ n) n]
    [(operator _ type nods) (eval-op type nods)]))


  
(let* ([inp (get-input (current-input-port))]
       [bin-str (hex->bin inp)])
  (define-values (nod _rst) (parse bin-str))
  (eval-node nod))
