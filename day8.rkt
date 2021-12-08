#lang racket

(require racket/hash)

(struct digit (chars) #:transparent)
(struct disp (signals output) #:transparent)

;; return the first 10 strings and the last 4 strings as lists of pairs
(define (get-input port)
  (define (make-digit str)
    (digit
      (for/list ([c (string-split str "")]
                 #:when (not (= (string-length c) 0)))
        (string->symbol c))))

  (define (process-line line)
    (define parts (string-split line " | "))
    (define signals (car parts))
    (define output (cadr parts))

    (disp (map make-digit (string-split signals " "))
          (map make-digit (string-split output " "))))

  (let loop ([lines '()])
    (define line (read-line port))
    (if (eof-object? line)
      lines
      (loop (cons (process-line line) lines)))))

;; part 1
(define (count-unique-nums displays)
  (for/sum ([d displays])
    (for/sum ([n (cdr d)])
      (if (unique-num? n) 1 0))))

(define (is-1? n)
  (= (string-length n) 2))

(define (is-4? n)
  (= (string-length n) 4))

(define (is-7? n)
  (= (string-length n) 3))

(define (is-8? n)
  (= (string-length n) 7))

(define (unique-num? n)
  (or (is-1? n)
      (is-4? n)
      (is-7? n)
      (is-8? n)))

;; part 2
(define ALL-CHARS (set 'a 'b 'c 'd 'e 'f 'g))
(define ZERO (set 'a 'b 'c 'e 'f 'g))
(define ONE (set 'c 'f))
(define TWO (set 'a 'c 'd 'e 'g))
(define THREE (set 'a 'c 'd 'f 'g))
(define FOUR (set 'b 'c 'd 'f))
(define FIVE (set 'a 'b 'd 'f 'g))
(define SIX (set 'a 'b 'd 'e 'f 'g))
(define SEVEN (set 'a 'c 'f))
(define EIGHT (set 'a 'b 'c 'd 'e 'f 'g))
(define NINE (set 'a 'b 'c 'd 'f 'g))

(define digit-lookup
  (hash ZERO  0
        ONE   1
        TWO   2
        THREE 3
        FOUR  4
        FIVE  5
        SIX   6
        SEVEN 7
        EIGHT 8
        NINE  9))

(define (get-affected-chars chars)
  ;; hash of signal-length to intersection of chars for digits with that signal length
  (define m
    (hash 2 ONE
          3 SEVEN
          4 FOUR
          5 (set->list (set-intersect TWO THREE FIVE))
          6 (set->list (set-intersect ZERO SIX NINE))))

  (hash-ref m (length chars) '()))

(define (validate-mapping mapping)
  (when (not (valid-mapping? mapping))
    (error "THIS IS BAD")))

(define (valid-mapping? mapping)
  (andmap (compose not set?) (hash-values mapping)))

(define (resolve-signals signals)
  (define initial-constraints
    (hash 'a ALL-CHARS
          'b ALL-CHARS
          'c ALL-CHARS
          'd ALL-CHARS
          'e ALL-CHARS
          'f ALL-CHARS
          'g ALL-CHARS))

  (define (determine-known-chars constraints)
    (define first-known-char
      (for/first ([(key val) (in-hash constraints)]
                  #:when (and (set? val) (= (set-count val) 1)))
        key))

    (define (update-constraints constraints key)
      (define old-val (hash-ref constraints key))
      (define new-constraints (hash-update constraints key set-first))

      (for/hash ([(key val) (in-hash new-constraints)])
        (if (set? val)
          (values key (set-subtract val old-val))
          (values key val))))

    (if first-known-char
      (determine-known-chars (update-constraints constraints
                                                 first-known-char))
      constraints))

  (define (apply-constraint constraints d)
    (define chars (digit-chars d))
    (define charset (list->set chars))
    (define affected-chars (get-affected-chars chars))

    (define new-constraints
      (for/fold ([constraints constraints])
                ([c affected-chars])
        (if (set? (hash-ref constraints c))
          (hash-update constraints c (λ (curr) (set-intersect curr charset)))
          constraints)))

    (determine-known-chars new-constraints))

  (define (apply-constraints constraints signals)
    (cond
      [(valid-mapping? constraints) constraints]
      [(empty? signals) (error "THIS IS VERY BAD")]
      [else
        (apply-constraints (apply-constraint constraints (car signals))
                           (cdr signals))]))

  (apply-constraints initial-constraints signals))

(define (build-number num-rep)
  (for/fold ([res 0])
            ([n num-rep])
    (+ n (* 10 res))))

(define (get-number chars)
  (hash-ref digit-lookup (list->set chars)))

(define (apply-mapping mapping num)
  (define scrambled-chars (digit-chars num))
  (get-number (map (λ (c) (hash-ref mapping c)) scrambled-chars)))

(define (flip-hash h)
  (for/hash ([(key val) (in-hash h)])
    (values val key)))

(define (solve-display d)
  (let ([mapping (flip-hash (resolve-signals (disp-signals d)))])
    (validate-mapping mapping)
    (define num-rep
      (for/list ([scrambled (disp-output d)])
        (apply-mapping mapping scrambled)))
    (build-number num-rep)))

(define (solve-displays ds)
  (apply + (map solve-display ds)))

(solve-displays (get-input (current-input-port)))
