#lang racket

(define (fix-move move)
  (if (or (> move 10)
          (< move 0))
    (modulo move 10)
    move))

(define (move-player square move)
  (let ([new-square (+ square move)])
    (if (> new-square 10)
      (modulo new-square 10)
      new-square)))

(define (update-player square score move)
  (let ([new-square (move-player square move)])
    (values new-square (+ score new-square))))

(define (winner? score)
  (>= score 1000))

(define (play-round p1-square p1-score p1-move p2-square p2-score p2-move)
  (define-values (new-p1-square new-p1-score)
                 (update-player p1-square p1-score p1-move))
  (if (winner? new-p1-score)
    (values new-p1-square new-p1-score p2-square p2-score)
    (let-values ([(new-p2-square new-p2-score)
                  (update-player p2-square p2-score p2-move)])
      (values new-p1-square
              new-p1-score
              new-p2-square
              new-p2-score))))

(define (play-deterministic-game p1 p2)
  (define (play-game/acc p1-square p1-score p1-move p2-square p2-score p2-move total-moves)
    (define-values (new-p1-square new-p1-score new-p2-square new-p2-score)
                   (play-round p1-square p1-score p1-move p2-square p2-score p2-move))

    (cond
      [(winner? new-p1-score) (values new-p1-score new-p2-score (* 3 (add1 total-moves)))]
      [(winner? new-p2-score) (values new-p2-score new-p1-score (* 3 (+ 2 total-moves)))]
      [else
       (play-game/acc new-p1-square
                      new-p1-score
                      (fix-move (- p1-move 2))
                      new-p2-square
                      new-p2-score
                      (fix-move (- p2-move 2))
                      (+ 2 total-moves))]))

  (play-game/acc p1 0 6 p2 0 5 0))

(define (play-quantum-game p1 p2)
  (define (quantum-winner? score)
    (>= score 21))

  (define (get-wins-from-move new-square curr-score other-square other-score winnings)
    (define new-score (+ new-square curr-score))
    (define winnings-key (list new-square new-score other-square other-score))
    (cond
      [(quantum-winner? new-score) (values 1 0 winnings)]
      [(hash-has-key? winnings winnings-key)
       (match-define (cons final-score other-final-score) (hash-ref winnings winnings-key))
       (values final-score other-final-score winnings)]
      [else
       (define-values (other-wins curr-wins new-winnings)
                      (get-win-count other-square other-score new-square new-score winnings))

       (define final-winnings
         (hash-set new-winnings winnings-key (cons curr-wins other-wins)))

       (values curr-wins other-wins final-winnings)]))

  (define (moves-seq)
    (list (cons 3 1)
          (cons 4 3)
          (cons 5 6)
          (cons 6 7)
          (cons 7 6)
          (cons 8 3)
          (cons 9 1)))

  (define (get-win-count curr-square curr-score other-square other-score winnings)
    (for/fold ([curr-wins 0]
               [other-wins 0]
               [winnings winnings])
              ([move-info (moves-seq)])
      (match-define (cons move multiplier) move-info)
      (define-values (new-curr-wins new-other-wins new-winnings)
        (get-wins-from-move (move-player curr-square move)
                            curr-score
                            other-square
                            other-score
                            winnings))

      (values (+ curr-wins (* multiplier new-curr-wins))
              (+ other-wins (* multiplier new-other-wins))
              new-winnings)))

  (define-values (final-first-wins final-second-wins _win)
    (get-win-count p1 0 p2 0 (hash)))

  (values final-first-wins final-second-wins))



(let-values ([(first-wins second-wins) (play-quantum-game 8 7)])
  (max first-wins second-wins))
