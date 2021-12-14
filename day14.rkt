#lang racket

(require racket/hash)

(define (get-input port)
  (define (get-start)
    (define line (read-line port))
    (for/list ([c (string-split line "")]
               #:when (not (string=? c "")))
      c))

  (define (get-rules)
    (for/hash ([line (in-lines port)])
      (match-define (list key val) (string-split line " -> "))
      (define key-combo
        (cons (substring key 0 1)
              (substring key 1 2)))

      (values key-combo val)))

  (let ([start (get-start)]
        [_l (read-line port)]
        [rules (get-rules)])
    (values start rules)))

(define (apply-rules-once curr rules)
  (define (app curr)
    (if (< (length curr) 2)
      curr
      (iter (car curr) (cadr curr) (cddr curr))))

  (define (iter fst-char snd-char rst)
    (let ([pair (cons fst-char snd-char)]
          [apped-rst (app (cons snd-char rst))])
      (if (hash-has-key? rules pair)
        (list* fst-char (hash-ref rules pair) apped-rst)
        (list* fst-char apped-rst))))

  (app curr))

(define (apply-rules start rules n)
  (for/fold ([curr start])
            ([_ (in-range n)])
    (apply-rules-once curr rules)))

(define (count-chars seq)
  (for/fold ([h (hash)])
            ([c seq])
    (hash-update h c add1 0)))

(define (calculate-score char-counts)
  (let ([vals (hash-values char-counts)])
    (- (apply max vals) (apply min vals))))

(define (apply-rules-in-place start rules n)
  (define (app curr)
    (if (< (length curr) 2)
      curr
      (iter (car curr) (cadr curr) (cdr curr))))

  (define (iter fst-char snd-char rst)
    (define pair (cons fst-char snd-char))
    (define mid-char (hash-ref rules pair))

    (append (add-chars fst-char mid-char (sub1 n))
            (cdr (add-chars mid-char snd-char (sub1 n)))
            (cdr (app rst))))

  (define (add-chars fst lst rem)
    (if (zero? rem)
      (list fst lst)
      (let ([mid (hash-ref rules (cons fst lst))])
        (append (add-chars fst mid (sub1 rem))
                (cdr (add-chars mid lst (sub1 rem)))))))

  (app start))

;; originally explored entire search space, then added cache
(define (count-in-place start rules n)
  (define (inc-char c counts)
    (hash-update counts c add1 0))

  (define (hash-add h1 h2)
    (hash-union h1 h2 #:combine +))

  (define (app curr memo)
    (cond
      [(empty? curr) (values (hash) memo)]
      [(= (length curr) 1)
       (values (inc-char (car curr) (hash))
               memo)]
      [else
        (define fst-char (car curr))
        (define snd-char (cadr curr))

       (let-values ([(char-counts memo+) (cnt (car curr) (cadr curr) memo n)])
         (define memo++ (hash-set memo+ (list fst-char snd-char n) char-counts))
         (define-values (rem-counts memo+++) (app (cdr curr) memo++))
         (values (hash-add char-counts rem-counts) memo+++))]))

  (define (cnt fst-char lst-char memo rem)
    (define memo-key (list fst-char lst-char rem))
    (cond
      [(zero? rem)
       (values (inc-char fst-char (hash))
               memo)]
      [(hash-has-key? memo memo-key)
       (values (hash-ref memo memo-key) memo)]
      [else
        (define mid-char (hash-ref rules (cons fst-char lst-char)))
        (define n-rem (sub1 rem))
        (let-values ([(left-count memo+) (cnt fst-char mid-char memo n-rem)])
          (define memo++ (hash-set memo+ (list fst-char mid-char n-rem) left-count))
          (define-values (right-count memo+++) (cnt mid-char lst-char memo++ n-rem))
          (define memo++++ (hash-set memo+++ (list mid-char lst-char n-rem) right-count))
          (values (hash-add left-count right-count)
                  memo++++))]))

  (let-values ([(counts memo) (app start (hash))])
    counts))

(let-values ([(start rules) (get-input (current-input-port))])
  (define char-counts (count-in-place start rules 40))
  (calculate-score char-counts))
