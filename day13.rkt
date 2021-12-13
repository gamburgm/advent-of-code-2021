#lang racket

(define (get-input port)
  (define (parse-coord line)
    (define parts (string-split line ","))
    (cons (string->number (car parts))
          (string->number (cadr parts))))

  (define (parse-fold line)
    (define fold-part (last (string-split line " ")))
    (define fold-components (string-split fold-part "="))
    (cons (car fold-components)
          (string->number (cadr fold-components))))

  (define (read-coords port)
    (define line (read-line port))
    (if (zero? (string-length line))
      '()
      (cons (parse-coord line) (read-coords port))))

  (define (read-folds port)
    (define line (read-line port))
    (if (eof-object? line)
      '()
      (cons (parse-fold line) (read-folds port))))

  (define coords (read-coords port))
  (define folds (read-folds port))
  (values coords folds))

(define (get-coord-count coords)
  (set-count (apply set coords)))

(define (reflect pos num)
  (- pos (abs (- pos num))))

(define (update-car c f)
  (cons (f (car c)) (cdr c)))

(define (update-cdr c f)
  (cons (car c) (f (cdr c))))

(define (process-fold update-axis pos coords)
  (define (reflect-pos num) (reflect pos num))
  (define (updater c) (update-axis c reflect-pos))

  (map updater coords))

(define (process-x-fold pos coords)
  (process-fold update-car pos coords))

(define (process-y-fold pos coords)
  (process-fold update-cdr pos coords))

(define (apply-fold fold coords)
  (match-define (cons axis pos) fold)
  (if (string=? axis "x")
    (process-x-fold pos coords)
    (process-y-fold pos coords)))

(define (apply-folds folds coords)
  (for/fold ([coords coords])
            ([f folds])
    (apply-fold f coords)))

;; part 2
(define (create-grid height width)
  (for/list ([_h height])
    (for/list ([_w width])
      ".")))

(define (get-dimensions coords)
  (for/fold ([height 0]
             [width 0])
            ([c coords])
    (values (max height (add1 (cdr c)))
            (max width (add1 (car c))))))

(define (update-grid grid coord)
  (match-define (cons col row) coord)
  (list-update grid
               row
               (λ (r) (list-update r col (λ (_) "#")))))

(define (apply-coords-to-grid grid coords)
  (for/fold ([g grid])
            ([c coords])
    (update-grid g c)))

(define (display-grid grid)
  (for ([row grid])
    (for ([p row])
      (display p))
    (displayln "")))


(let-values ([(coords folds) (get-input (current-input-port))])
  (define final-coords (apply-folds folds coords))
  (define grid (call-with-values (λ () (get-dimensions final-coords))
                                 create-grid))

  (display-grid (apply-coords-to-grid grid final-coords)))
