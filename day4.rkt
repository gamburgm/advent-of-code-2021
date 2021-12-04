#lang racket

;; return two objects: a list of numbers and a list of lists of lists of numbers
;; the first is the moves getting played, the second is the boards
(define (get-board port first-num)
  (define start (list first-num (read port) (read port) (read port) (read port)))
  (list start
        (list (read port) (read port) (read port) (read port) (read port))
        (list (read port) (read port) (read port) (read port) (read port))
        (list (read port) (read port) (read port) (read port) (read port))
        (list (read port) (read port) (read port) (read port) (read port))))

(define (get-input port)
  (define moves-line (read-line port))
  (define moves (map string->number (string-split moves-line ",")))

  (let loop ([boards '()])
    (define next (read port))
    (if (eof-object? next)
      (values moves boards)
      (loop (cons (get-board port next) boards)))))

(define (winning-sequence? played-moves seq)
  (andmap (λ (n) (set-member? played-moves n)) seq))

(define (winning-board? played-moves board)
  (define (winning-seq? seq)
    (winning-sequence? played-moves seq))

  (define (caddddr ls)
    (cadddr (cdr ls)))

  (define col-funcs (list car cadr caddr cadddr caddddr))

  (or
    (ormap winning-seq? board)
    (ormap (λ (col-func) (winning-seq? (map col-func board))) col-funcs)))


(define (get-first-winning-board moves boards)
  (define (winner/acc played moves boards)
    (let ([now-played (set-add played (car moves))]
          [remaining (cdr moves)])
      (define maybe-winner
        (for/first ([board boards]
                    #:when (winning-board? now-played board))
          board))

      (if maybe-winner
        (values maybe-winner now-played (car moves))
        (winner/acc now-played remaining boards))))

  (winner/acc (set) moves boards))

(define (get-last-winning-board moves boards)
  (define (make-winning played moves board)
    (let ([now-played (set-add played (car moves))]
          [remaining (cdr moves)])

      (if (winning-board? now-played board)
        (values board now-played (car moves))
        (make-winning now-played remaining board))))

  (define (loser/acc played moves boards)
    (let ([now-played (set-add played (car moves))]
          [remaining (cdr moves)])
      (define losers
        (filter (λ (board) (not (winning-board? now-played board))) boards))

      (if (= (length losers) 1)
        (make-winning now-played remaining (car losers))
        (loser/acc now-played remaining boards))))

  (loser/acc (set) moves boards))

(define (get-result winner played last-move)
  (let* ([board-nums (apply append winner)]
         [not-selected (filter (λ (n) (not (set-member? played n))) board-nums)]
         [sum (apply + not-selected)])
    (* sum last-move)))

(let*-values ([(moves boards) (get-input (current-input-port))]
              [(winner played last-move) (get-last-winning-board moves boards)])
  (get-result winner played last-move))
