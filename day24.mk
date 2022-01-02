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

;; modified relational arithmetic for negative numbers
(define (sumo n m k)
  (fresh (n-num m-num k-num)
    (conde
      [(== n `(pos . ,n-num))
       (== m `(pos . ,m-num))
       (== k `(pos . ,k-num))
       (pluso n-num m-num k-num)]
      [(== n `(pos . ,n-num))
       (== m `(neg . ,m-num))
       (conde
         [(<o n-num m-num)
          (== k `(neg . ,k-num))
          (pluso n-num k-num m-num)]
         [(== k `(pos . ,k-num))
          (pluso m-num k-num n-num)])]
      [(== n `(neg . ,n-num))
       (== m `(pos . ,m-num))
       (conde
         [(<o m-num n-num)
          (== k `(neg . ,k-num))
          (pluso m-num k-num n-num)]
         [(== k `(pos . ,k-num))
          (pluso n-num k-num m-num)])]
      [(== n `(neg . ,n-num))
       (== m `(neg . ,m-num))
       (== k `(neg . ,k-num))
       (pluso n-num m-num k-num)])))

(define (producto n m k)
  (fresh (n-num m-num k-num)
    (conde
      [(== n `(pos . ,n-num))
       (== m `(pos . ,m-num))
       (== k `(pos . ,k-num))
       (*o n-num m-num k-num)]
      [(== n `(pos . ,n-num))
       (== m `(neg . ,m-num))
       (== k `(neg . ,k-num))
       (*o n-num m-num k-num)]
      [(== n `(neg . ,n-num))
       (== m `(pos . ,m-num))
       (== k `(neg . ,k-num))
       (*o n-num m-num k-num)]
      [(== n `(neg . ,n-num))
       (== m `(neg . ,m-num))
       (== k `(pos . ,k-num))
       (*o n-num m-num k-num)])))

(define (quotiento n m q r)
  (fresh (n-num m-num q-num r-num)
    (== n `(pos . ,n-num))
    (== m `(pos . ,m-num))
    (== q `(pos . ,q-num))
    (== r `(pos . ,r-num))
    (/o n-num m-num q-num r-num)))

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
    (sumo first-val second-val sum)
    (update-reg-o first-arg sum i-regs o-regs)))

(define (mul-o first-arg second-arg i-regs o-regs)
  (fresh (first-val second-val product)
    (val-o first-arg first-val i-regs)
    (val-o second-arg second-val i-regs)
    (producto first-val second-val product)
    (update-reg-o first-arg product i-regs o-regs)))

(define (div-o first-arg second-arg i-regs o-regs)
  (fresh (first-val second-val quot _t)
    (val-o first-arg first-val i-regs)
    (val-o second-arg second-val i-regs)
    (quotiento first-val second-val quot _t)
    (update-reg-o first-arg quot i-regs o-regs)))

(define (mod-o first-arg second-arg i-regs o-regs)
  (fresh (first-val second-val mod _t)
    (val-o first-arg first-val i-regs)
    (val-o second-arg second-val i-regs)
    (quotiento first-val second-val _t mod)
    (update-reg-o first-arg mod i-regs o-regs)))

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
      [(== instr `(mod ,first-arg ,second-arg))
       (== inp left)
       (mod-o first-arg second-arg i-regs o-regs)]
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

(define INSTRS
  `((inp w)
    (mul x (pos 0))
    (add x z)
    (mod x (pos 26))
    (div z (pos 1))
    (add x (pos 14))
    (eql x w)
    (eql x (pos 0))
    (mul y (pos 0))
    (add y (pos 25))
    (mul y x)
    (add y (pos 1))
    (mul z y)
    (mul y (pos 0))
    (add y w)
    (add y (pos 16))
    (mul y x)
    (add z y)
    (inp w)
    (mul x (pos 0))
    (add x z)
    (mod x (pos 26))
    (div z (pos 1))
    (add x (pos 11))
    (eql x w)
    (eql x (pos 0))
    (mul y (pos 0))
    (add y (pos 25))
    (mul y x)
    (add y (pos 1))
    (mul z y)
    (mul y (pos 0))
    (add y w)
    (add y (pos 3))
    (mul y x)
    (add z y)
    (inp w)
    (mul x (pos 0))
    (add x z)
    (mod x (pos 26))
    (div z (pos 1))
    (add x (pos 12))
    (eql x w)
    (eql x (pos 0))
    (mul y (pos 0))
    (add y (pos 25))
    (mul y x)
    (add y (pos 1))
    (mul z y)
    (mul y (pos 0))
    (add y w)
    (add y (pos 2))
    (mul y x)
    (add z y)
    (inp w)
    (mul x (pos 0))
    (add x z)
    (mod x (pos 26))
    (div z (pos 1))
    (add x (pos 11))
    (eql x w)
    (eql x (pos 0))
    (mul y (pos 0))
    (add y (pos 25))
    (mul y x)
    (add y (pos 1))
    (mul z y)
    (mul y (pos 0))
    (add y w)
    (add y (pos 7))
    (mul y x)
    (add z y)
    (inp w)
    (mul x (pos 0))
    (add x z)
    (mod x (pos 26))
    (div z (pos 26))
    (add x (neg 10))
    (eql x w)
    (eql x (pos 0))
    (mul y (pos 0))
    (add y (pos 25))
    (mul y x)
    (add y (pos 1))
    (mul z y)
    (mul y (pos 0))
    (add y w)
    (add y (pos 13))
    (mul y x)
    (add z y)
    (inp w)
    (mul x (pos 0))
    (add x z)
    (mod x (pos 26))
    (div z (pos 1))
    (add x (pos 15))
    (eql x w)
    (eql x (pos 0))
    (mul y (pos 0))
    (add y (pos 25))
    (mul y x)
    (add y (pos 1))
    (mul z y)
    (mul y (pos 0))
    (add y w)
    (add y (pos 6))
    (mul y x)
    (add z y)
    (inp w)
    (mul x (pos 0))
    (add x z)
    (mod x (pos 26))
    (div z (pos 26))
    (add x (neg 14))
    (eql x w)
    (eql x (pos 0))
    (mul y (pos 0))
    (add y (pos 25))
    (mul y x)
    (add y (pos 1))
    (mul z y)
    (mul y (pos 0))
    (add y w)
    (add y (pos 10))
    (mul y x)
    (add z y)
    (inp w)
    (mul x (pos 0))
    (add x z)
    (mod x (pos 26))
    (div z (pos 1))
    (add x (pos 10))
    (eql x w)
    (eql x (pos 0))
    (mul y (pos 0))
    (add y (pos 25))
    (mul y x)
    (add y (pos 1))
    (mul z y)
    (mul y (pos 0))
    (add y w)
    (add y (pos 11))
    (mul y x)
    (add z y)
    (inp w)
    (mul x (pos 0))
    (add x z)
    (mod x (pos 26))
    (div z (pos 26))
    (add x (neg 4))
    (eql x w)
    (eql x (pos 0))
    (mul y (pos 0))
    (add y (pos 25))
    (mul y x)
    (add y (pos 1))
    (mul z y)
    (mul y (pos 0))
    (add y w)
    (add y (pos 6))
    (mul y x)
    (add z y)
    (inp w)
    (mul x (pos 0))
    (add x z)
    (mod x (pos 26))
    (div z (pos 26))
    (add x (neg 3))
    (eql x w)
    (eql x (pos 0))
    (mul y (pos 0))
    (add y (pos 25))
    (mul y x)
    (add y (pos 1))
    (mul z y)
    (mul y (pos 0))
    (add y w)
    (add y (pos 5))
    (mul y x)
    (add z y)
    (inp w)
    (mul x (pos 0))
    (add x z)
    (mod x (pos 26))
    (div z (pos 1))
    (add x (pos 13))
    (eql x w)
    (eql x (pos 0))
    (mul y (pos 0))
    (add y (pos 25))
    (mul y x)
    (add y (pos 1))
    (mul z y)
    (mul y (pos 0))
    (add y w)
    (add y (pos 11))
    (mul y x)
    (add z y)
    (inp w)
    (mul x (pos 0))
    (add x z)
    (mod x (pos 26))
    (div z (pos 26))
    (add x (neg 3))
    (eql x w)
    (eql x (pos 0))
    (mul y (pos 0))
    (add y (pos 25))
    (mul y x)
    (add y (pos 1))
    (mul z y)
    (mul y (pos 0))
    (add y w)
    (add y (pos 4))
    (mul y x)
    (add z y)
    (inp w)
    (mul x (pos 0))
    (add x z)
    (mod x (pos 26))
    (div z (pos 26))
    (add x (neg 9))
    (eql x w)
    (eql x (pos 0))
    (mul y (pos 0))
    (add y (pos 25))
    (mul y x)
    (add y (pos 1))
    (mul z y)
    (mul y (pos 0))
    (add y w)
    (add y (pos 4))
    (mul y x)
    (add z y)
    (inp w)
    (mul x (pos 0))
    (add x z)
    (mod x (pos 26))
    (div z (pos 26))
    (add x (neg 12))
    (eql x w)
    (eql x (pos 0))
    (mul y (pos 0))
    (add y (pos 25))
    (mul y x)
    (add y (pos 1))
    (mul z y)
    (mul y (pos 0))
    (add y w)
    (add y (pos 6))
    (mul y x)
    (add z y)))
        

(run 1 (d1 d2 d3 d4 d5 d6 d7 d8 d9 d10 d11 d12 d13 d14)
  (=/= d1 '())
  (=/= d2 '())
  (=/= d3 '())
  (=/= d4 '())
  (=/= d5 '())
  (=/= d6 '())
  (=/= d7 '())
  (=/= d8 '())
  (=/= d9 '())
  (=/= d10 '())
  (=/= d11 '())
  (=/= d12 '())
  (=/= d13 '())
  (=/= d14 '())
  (<o d1 '(0 1 0 1))
  (<o d2 '(0 1 0 1))
  (<o d3 '(0 1 0 1))
  (<o d4 '(0 1 0 1))
  (<o d5 '(0 1 0 1))
  (<o d6 '(0 1 0 1))
  (<o d7 '(0 1 0 1))
  (<o d8 '(0 1 0 1))
  (<o d9 '(0 1 0 1))
  (<o d10 '(0 1 0 1))
  (<o d11 '(0 1 0 1))
  (<o d12 '(0 1 0 1))
  (<o d13 '(0 1 0 1))
  (<o d14 '(0 1 0 1))
  (fresh (ow ox oy)
    (eval-o INSTRS
            '((pos) (pos) (pos) (pos))
            `(,ow ,ox ,oy (pos))
            `((pos . ,d1)
              (pos . ,d2)
              (pos . ,d3)
              (pos . ,d4)
              (pos . ,d5)
              (pos . ,d6)
              (pos . ,d7)
              (pos . ,d8)
              (pos . ,d9)
              (pos . ,d10)
              (pos . ,d11)
              (pos . ,d12)
              (pos . ,d13)
              (pos . ,d14))
            '())))
