#lang racket

(require (for-syntax syntax/parse racket/base))

(struct explode-res (left right) #:transparent)

(define-syntax (match-num stx)
  (syntax-parse stx
    [(_ n [(left right) pair-body ...+] [_ num-body ...+])
     #'(match n
         [(cons left right)
          pair-body ...]
         [(? number?)
          num-body ...]
         [_ (error "Snailfish Number is neither a pair nor an racket number!" n)])]))


(define (get-input port)
  (define (char->num c)
    (- (char->integer c) (char->integer #\0)))

  (define (parse-pair chars)
    ;; `cdr` to skip open-bracket
    (define-values (left right-chars) (parse-num (cdr chars)))
    ;; `cdr` to skip comma
    (define-values (right rst-chars) (parse-num (cdr right-chars)))
    ;; `cdr` to skip close-bracket
    (values (cons left right) (cdr rst-chars)))

  (define (parse-num chars)
    (match (car chars)
      [#\[ (parse-pair chars)]
      [(? char-numeric?) (values (char->num (car chars)) (cdr chars))]
      [_ (error "parse-num error: ~a is neither a bracket nor a number" (car chars))]))

  (define (get-nums)
    (define line (read-line port))
    (if (eof-object? line)
      '()
      (let-values ([(num _) (parse-num (string->list line))])
        (cons num (get-nums)))))

  (get-nums))

(define (print-num n)
  (define (wrt v) (printf "~a" v))

  (define (write-num n)
    (match-num n
      [(left right)
       (wrt "[")
       (write-num left)
       (wrt ",")
       (write-num right)
       (wrt "]")]
      [_ (wrt n)]))

  (write-num n)
  (printf "\n"))

(define (nums=? n1 n2)
  (match (cons n1 n2)
    [(cons (cons left1 right1)
           (cons left2 right2))
     (and (nums=? left1 left2)
          (nums=? right1 right2))]
    [(cons (? number?)
           (? number?))
     (= n1 n2)]
    [_ #f]))

(define (reduce-num n)
  (define (too-deep? d)
    (>= d 4))

  (define (inc-depth d)
    (add1 d))

  (define (init-depth)
    0)

  (define (split? n)
    (> n 9))

  (define (split-int n)
    (cons (floor (/ n 2))
          (ceiling (/ n 2))))

  (define (maybe-split n)
    (if (split? n)
      (values (split-int n) #t)
      (values n #f)))

  (define (apply-left n val)
    (match-num n
      [(left right)
       (cons (apply-left left val) right)]
      [_ (+ n val)]))

  (define (apply-right n val)
    (match-num n
      [(left right)
       (cons left (apply-right right val))]
      [_ (+ n val)]))

  (define (make-explode-res left-exp right-exp)
    (if (and (zero? left-exp) (zero? right-exp))
      #t
      (explode-res left-exp right-exp)))

  (define (explode-pair/depth left right d)
    (define-values (new-left left-explosion)
                   (explode-num/depth left (inc-depth d)))

    (cond
      [(explode-res? left-explosion)
       (values (cons new-left
                     (apply-left right (explode-res-right left-explosion)))
               (make-explode-res (explode-res-left left-explosion)
                                 0))]
      [left-explosion (values (cons new-left right) #t)]
      [else
       (define-values (new-right right-explosion)
                      (explode-num/depth right (inc-depth d)))

       (cond
         [(explode-res? right-explosion)
          (values (cons (apply-right new-left (explode-res-left right-explosion))
                        new-right)
                  (make-explode-res 0
                                    (explode-res-right right-explosion)))]
         [right-explosion (values (cons new-left new-right) #t)]
         [else
          (values (cons new-left new-right) #f)])]))

  (define (explode-num/depth n d)
    (match-num n
      [(left right)
       (if (too-deep? d)
         (values 0 (explode-res left right))
         (explode-pair/depth left right d))]
      [_ (values n #f)]))

  (define (split-pair left right)
    (define-values (new-left split?) (split-num left))
    (cond
      [split? (values (cons new-left right) #t)]
      [else
       (define-values (new-right split?) (split-num right))
       (values (cons new-left new-right) split?)]))

  (define (split-num n)
    (match-num n
      [(left right) (split-pair left right)]
      [_ (maybe-split n)]))

  (define (explode-root n)
    (define-values (exploded-n e-res) (explode-num/depth n (init-depth)))
    (if (or (explode-res? e-res) e-res)
      (explode-root exploded-n)
      exploded-n))

  (define (split-root n)
    (define-values (split-n _) (split-num n))
    split-n)

  (define (reduce-root n)
    (let* ([exploded-n (explode-root n)]
           [split-n (split-root exploded-n)])
      (if (equal? n split-n)
        n
        (reduce-root split-n))))

  (reduce-root n))

(define (add-num n1 n2)
  (reduce-num (cons n1 n2)))

(define (add-nums nums)
  (for/fold ([curr-num (car nums)])
            ([next-num (cdr nums)])
    (add-num curr-num next-num)))

(define (num-magnitude n)
  (match-num n
    [(left right)
     (+ (* 3 (num-magnitude left))
        (* 2 (num-magnitude right)))]
    [_ n]))

(define (get-max-addition nums)
  (define (best-with-n n nums)
    (define (add-n other)
      (max (num-magnitude (add-num n other))
           (num-magnitude (add-num other n))))
    (if (empty? nums)
      0
      (apply max (map add-n nums))))

  (define (best-sums nums)
    (cond
      [(empty? nums) 0]
      [else
       (max (best-with-n (car nums) (cdr nums))
            (best-sums (cdr nums)))]))

  (best-sums nums))

(let ([nums (get-input (current-input-port))])
  (get-max-addition nums))
