#lang racket

#|
My actual solution to today's problem was by hand. I wrote this code
hoping I could solve by brute force.

My by-hand solution was as follows:
1. I realized that the instructions between 'stdin reads' were
   nearly identical.
2. I took the first of these and hand-optimized it (with a more
   expressive instruction set). For instance,
     ```
     mul x 0
     add x 14
     ```
   became `x = 14`.
3. I highlighted the differences between series of instructions
   between reads. I found three:
     a. the register `z` is divided either by 1 or 26
     b. the register `x` is incremented by some number
     c. the register `y` is set to w + some number
4. I quickly realized that the register `z` was only divided
   by 26 when the register `x` was 'incremented' by a negative
   number. It also occurred to me that the following condition
   could never be true if the number `x` was incremented by was
   >= 10 (which it always was), but it could be true if that
   number was greater than -25 and less than 0 (which was true
   of all such numbers which were negative).
5. With that realization, I determined that the code was equivalent to:
     ```
     if (w == (z % 26) + n1):
         z //= 26
     else:
         z = (z * 26) + w + n2
     ```
   Where n1 was the number that register `x` was incremented by,
   and n2 was the number that register `y` was incremented by.
6. This code reminded me of binary number construction, and
   it occurred to me that what you effectively had was a stack.
   That stack worked as follows (in prose):
     ```
     - if n1 is positive, push `w` + n2 onto the stack
     - if n2 is negative and |n2| + w == what's on top of the
       stack, pop off the stack
     ```
   And you succeed when the stack is empty at the end of execution.
7. I used the specified constraints to relate each 'pushed' digit
   with its corresponding 'popped' digit, and with n1 and n2,
   formulated the following equations for the constraints (where
   w'n' is the nth digit of the number, leftmost is 1st):
     ```
     w1 + 4 = w14
     w2 - 6 = w13
     w3 - 1 = w10
     w4 - 3 = w5
     w6 - 8 = w7
     w8 + 7 = w9
     w11 + 8 = w12
     ```
8. Using these equations, it's easy to minimize or maximize the
   resulting number. To maximize, set the larger of the two
   digits in each equation to 9, and to minimize, set them to
   1. Then solve and concatenate to produce the final number.
|#


;; registers
(struct regs (w x y z) #:transparent)

;; instructions
(struct inp (reg) #:transparent)
(struct add (reg v) #:transparent)
(struct mul (reg v) #:transparent)
(struct div (reg v) #:transparent)
(struct mod (reg v) #:transparent)
(struct eql (reg v) #:transparent)

(define (string->reg s)
  (string->symbol s))

(define (reg-string? s)
  (and (string? s)
       (or (string=? s "w")
           (string=? s "x")
           (string=? s "y")
           (string=? s "z"))))

(define (string->val s)
  (if (reg-string? s)
    (string->reg s)
    (string->number s)))

(define (get-input port)
  (define (parse-line line)
    (define parts (string-split line " "))
    (match (car parts)
      ["inp" (inp (string->reg (cadr parts)))]
      ["add" (add (string->reg (cadr parts))
                  (string->val (caddr parts)))]
      ["mul" (mul (string->reg (cadr parts))
                  (string->val (caddr parts)))]
      ["div" (div (string->reg (cadr parts))
                  (string->val (caddr parts)))]
      ["mod" (mod (string->reg (cadr parts))
                  (string->val (caddr parts)))]
      ["eql" (eql (string->reg (cadr parts))
                  (string->val (caddr parts)))]))

  (define (read-lines)
    (define line (read-line port))
    (if (eof-object? line)
      '()
      (cons (parse-line line) (read-lines))))

  (read-lines))

;; register helpers
(define (init-regs)
  (regs 0 0 0 0))

(define (update-w r val)
  (regs val
        (regs-x r)
        (regs-y r)
        (regs-z r)))

(define (update-x r val)
  (regs (regs-w r)
        val
        (regs-y r)
        (regs-z r)))

(define (update-y r val)
  (regs (regs-w r)
        (regs-x r)
        val
        (regs-z r)))

(define (update-z r val)
  (regs (regs-w r)
        (regs-x r)
        (regs-y r)
        val))

(define (update-reg r register val)
  (match register
    ['w (update-w r val)]
    ['x (update-x r val)]
    ['y (update-y r val)]
    ['z (update-z r val)]
    [_ (error "NO MATCHING REGISTER IN UPDATE: " register)]))

(define (get-reg r register)
  (match register
    ['w (regs-w r)]
    ['x (regs-x r)]
    ['y (regs-y r)]
    ['z (regs-z r)]
    [_ (error "NO MATCHING REGISTER IN GET: " register)]))

(define (register? v)
  (and (symbol? v)
       (or (symbol=? v 'w)
           (symbol=? v 'x)
           (symbol=? v 'y)
           (symbol=? v 'z))))

(define (get-val r v)
  (if (register? v)
    (get-reg r v)
    v))

(define (step r instr stream)
  (match instr
    [(inp reg)
     (when (empty? stream)
       (error "CANNOT INPUT FROM EMPTY STREAM: " r instr))

     (values (update-reg r reg (car stream))
             (cdr stream))]
    [(add reg v)
     (let ([reg-val (get-reg r reg)]
           [actual-val (get-val r v)])
       (values (update-reg r reg (+ reg-val actual-val))
               stream))]
    [(mul reg v)
     (let ([reg-val (get-reg r reg)]
           [actual-val (get-val r v)])
       (values (update-reg r reg (* reg-val actual-val))
               stream))]
    [(div reg v)
     (let ([reg-val (get-reg r reg)]
           [actual-val (get-val r v)])
       (when (zero? actual-val)
         (error "CANNOT DIVIDE BY ZERO: " r instr))

       (let* ([res (/ reg-val actual-val)]
              [final (if (positive? res) (floor res) (ceiling res))])
         (values (update-reg r reg final)
                 stream)))]
    [(mod reg v)
     (let ([reg-val (get-reg r reg)]
           [actual-val (get-val r v)])
       (when (or (negative? reg-val)
                 (not (positive? actual-val)))
         (error "ILLEGAL MOD INSTRUCTION: " r instr))

       (values (update-reg r reg (modulo reg-val actual-val))
               stream))]
    [(eql reg v)
     (let ([reg-val (get-reg r reg)]
           [actual-val (get-val r v)])
       (values (update-reg r reg (if (= reg-val actual-val) 1 0))
               stream))]))

(define (alu r instrs stream)
  (if (empty? instrs)
    r
    (let-values ([(new-r new-stream) (step r (car instrs) stream)])
      (print-z (regs-z new-r))
      (alu new-r (cdr instrs) new-stream))))

(define (valid-model? r)
  (zero? (get-reg r 'z)))

(define (num->stream n)
  (for/list ([d (string-split (number->string n) "")]
             #:when (not (string=? d "")))
    (string->number d)))

(define (initial-model)
  97856431200000)

(define (run-models instrs num get-next)
  (define (fail n)
    (printf "FAIL: ~a\n" n))

  (define (succeed n)
    (printf "SUCCESS: ~a\n" n))

  (with-handlers ([exn:fail? identity])
    (define outcome (alu (init-regs) instrs (num->stream num)))
    (when (zero? (regs-x outcome))
      (displayln num)
      (displayln outcome))
    (when (valid-model? outcome)
      (succeed num)))

  (run-models instrs (get-next num) get-next))

(define (print-z z)
  (let loop ([z z])
    (unless (zero? z)
      (printf "~a " (modulo z 26))
      (loop (floor (/ z 26)))))

  (displayln ""))


(let ([input (get-input (current-input-port))]
      [model (initial-model)])
  (alu (init-regs) input (list 1 7 2 4 1 9 1 1 8 1 1 9 1 5)))
