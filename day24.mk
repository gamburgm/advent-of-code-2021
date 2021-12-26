#lang racket

(require "main.rkt")
(require "../faster-miniKanren/numbers.rkt")

;; FIXME why does re-ordering the clauses cause infinite loops again?
(define (appendo l1 l2 o)
  (conde
    [(== l1 '()) (== l2 o)]
    [(fresh (a d new-o)
       (== l1 `(,a . ,d))
       (== o `(,a . ,new-o))
       (appendo d l2 new-o))]))

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
      ["inp" `(inp ,(string->reg (cadr parts)))]
      ["add" `(add ,(string->reg (cadr parts))
                   ,(string->val (caddr parts)))]
      ["mul" `(mul ,(string->reg (cadr parts))
                   ,(string->val (caddr parts)))]
      ["div" `(div ,(string->reg (cadr parts))
                   ,(string->val (caddr parts)))]
      ["mod" `(mod ,(string->reg (cadr parts))
                   ,(string->val (caddr parts)))]
      ["eql" `(eql ,(string->reg (cadr parts))
                   ,(string->val (caddr parts)))]))

  (define (read-lines)
    (define line (read-line port))
    (if (eof-object? line)
      '()
      (cons (parse-line line) (read-lines))))

  (read-lines))

(define (regs-o regs w x y z)
  (== regs `(,w ,x ,y ,z)))

(define (update-w val i-regs o-regs)
  (fresh (i-w i-x i-y i-z
          o-w o-x o-y o-z)
    (regs-o i-regs i-w i-x i-y i-z)
    (regs-o o-regs o-w o-x o-y o-z)
    (== o-w val)
    (== i-x o-x)
    (== i-y o-y)
    (== i-z o-z)))

(define (update-x val i-regs o-regs)
  (fresh (i-w i-x i-y i-z
          o-w o-x o-y o-z)
    (regs-o i-regs i-w i-x i-y i-z)
    (regs-o o-regs o-w o-x o-y o-z)
    (== i-w o-w)
    (== o-x val)
    (== i-y o-y)
    (== i-z o-z)))

(define (update-y val i-regs o-regs)
  (fresh (i-w i-x i-y i-z
          o-w o-x o-y o-z)
    (regs-o i-regs i-w i-x i-y i-z)
    (regs-o o-regs o-w o-x o-y o-z)
    (== i-w o-w)
    (== i-x o-x)
    (== o-y val)
    (== i-z o-z)))

(define (update-z val i-regs o-regs)
  (fresh (i-w i-x i-y i-z
          o-w o-x o-y o-z)
    (regs-o i-regs i-w i-x i-y i-z)
    (regs-o o-regs o-w o-x o-y o-z)
    (== i-w o-w)
    (== i-x o-x)
    (== i-y o-y)
    (== o-z val)))

(define (update-reg-o reg val i-regs o-regs)
  (conde
    [(== reg 'w) (update-w val i-regs o-regs)]
    [(== reg 'x) (update-x val i-regs o-regs)]
    [(== reg 'y) (update-y val i-regs o-regs)]
    [(== reg 'z) (update-z val i-regs o-regs)]))

(define (not-reg-o arg)
  (fresh ()
    (=/= arg 'w)
    (=/= arg 'x)
    (=/= arg 'y)
    (=/= arg 'z)))

(define (val-o arg val regs)
  (fresh (w x y z)
    (regs-o regs w x y z)
    (conde
      [(== arg 'w) (== val w)]
      [(== arg 'x) (== val x)]
      [(== arg 'y) (== val y)]
      [(== arg 'z) (== val z)]
      [(not-reg-o arg) (== arg val)])))

(define (input-o register i-regs o-regs inp left)
  (fresh (a d)
    (== inp `(,a . ,d))
    (== d left)
    (update-reg-o register a i-regs o-regs)))

(define (add-o first-arg second-arg i-regs o-regs)
  (fresh (first-val second-val sum)
    (val-o first-arg first-val i-regs)
    (val-o second-arg second-val i-regs)
    (pluso first-val second-val sum)
    (update-reg-o first-arg sum i-regs o-regs)))

(define (mul-o first-arg second-arg i-regs o-regs)
  (fresh (first-val second-val product)
    (val-o first-arg first-val i-regs)
    (val-o second-arg second-val i-regs)
    (*o first-val second-val product)
    (update-reg-o first-arg product i-regs o-regs)))

(define (div-o first-arg second-arg i-regs o-regs)
  (fresh (first-val second-val quot)
    (val-o first-arg first-val i-regs)
    (val-o second-arg second-val i-regs)
    (*o quot second-val first-val)
    (update-reg-o first-arg quot i-regs o-regs)))

(define (eql-o first-arg second-arg i-regs o-regs)
  (fresh (first-val second-val)
    (val-o first-arg first-val i-regs)
    (val-o second-arg second-val i-regs)
    (conde
      [(== first-val second-val) 
       (update-reg-o first-arg '(1) i-regs o-regs)]
      [(update-reg-o first-arg '() i-regs o-regs)])))

(define (eval-instr instr i-regs o-regs inp left)
  (fresh (first-arg second-arg)
    (conde
      [(== instr `(inp ,first-arg))
       (input-o first-arg i-regs o-regs inp left)]
      [(== instr `(add ,first-arg ,second-arg))
       (== inp left)
       (add-o first-arg second-arg i-regs o-regs)]
      [(== instr `(mul ,first-arg ,second-arg))
       (== inp left)
       (mul-o first-arg second-arg i-regs o-regs)]
      [(== instr `(div ,first-arg ,second-arg))
       (== inp left)
       (div-o first-arg second-arg i-regs o-regs)]
      [(== instr `(eql ,first-arg ,second-arg))
       (== inp left)
       (eql-o first-arg second-arg i-regs o-regs)])))

(define (eval-o instrs i-regs o-regs inp left)
  (conde
    [(== instrs '())
     (== inp left)
     (== i-regs o-regs)]
    [(fresh (next-instr rem-instrs next-regs next-inp)
       (== instrs `(,next-instr . ,rem-instrs))
       (eval-instr next-instr i-regs next-regs inp next-inp)
       (eval-o rem-instrs next-regs o-regs next-inp left))]))

(run 10 (q e1 e2)
  (eval-o `(,e1 ,e2)
          `(() () () ())
          `(() () () (1))
          q
          '()))
