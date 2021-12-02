#lang racket

(define (get-input port)
  (let loop ([res '()])
    (let ([cmd (read port)])
      (if (eof-object? cmd)
        (reverse res)
        (loop (cons (list cmd (read port)) res))))))

(define (process-moves moves)
  (define (process moves curr-depth curr-pos curr-aim)
    (match moves
      ['() (* curr-depth curr-pos)]
      [`((forward ,amt) . ,rst)
        (process rst
                 (+ curr-depth (* curr-aim amt))
                 (+ curr-pos amt)
                 curr-aim)]
      [`((down ,amt) . ,rst)
        (process rst curr-depth curr-pos (+ curr-aim amt))]
      [`((up ,amt) . ,rst)
        (process rst curr-depth curr-pos (- curr-aim amt))]))

  (process moves 0 0 0))

(process-moves (get-input (current-input-port)))
