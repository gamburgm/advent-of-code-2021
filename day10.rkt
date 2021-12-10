#lang racket

(define (get-input port)
  (define (get-chars line)
    (for/list ([c (string-split line "")]
               #:when (not (string=? c "")))
      c))

  (define reversed-lines
    (let loop ([lines '()])
      (define line (read-line port))
      (if (eof-object? line)
        lines
        (loop (cons (get-chars line) lines)))))

  (reverse reversed-lines))

(define CLOSE-LOOKUP
  (hash ")" "("
        "]" "["
        "}" "{"
        ">" "<"))

(define OPEN-LOOKUP
  (hash "(" ")"
        "[" "]"
        "{" "}"
        "<" ">"))

(define OPEN-CHARS (set "(" "[" "{" "<"))


(define (score-line line)
  (define (score/acc line stack)
    (cond
      [(empty? line) ""]
      [(set-member? OPEN-CHARS (car line))
       (score/acc (cdr line) (cons (car line) stack))]
      [(string=? (hash-ref CLOSE-LOOKUP (car line)) (car stack))
       (score/acc (cdr line) (cdr stack))]
      [else (car line)]))

  (define (get-score c)
    (match c
      ["" 0]
      [")" 3]
      ["]" 57]
      ["}" 1197]
      [">" 25137]))

  (get-score (score/acc line '())))

(define (score-lines lines)
  (for/sum ([l lines])
    (score-line l)))

(define (get-close line)
  (define (flip-chars chars)
    (for/list ([c chars])
      (hash-ref OPEN-LOOKUP c)))

  (define (close/acc line stack)
    (cond
      [(empty? line) stack]
      [(set-member? OPEN-CHARS (car line))
       (close/acc (cdr line) (cons (car line) stack))]
      [(string=? (hash-ref CLOSE-LOOKUP (car line)) (car stack))
       (close/acc (cdr line) (cdr stack))]
      [else '()]))

  (flip-chars (close/acc line '())))

(define (get-stacks lines)
  (let ([all-stacks (map get-close lines)])
    (for/list ([stack all-stacks]
               #:when (not (empty? stack)))
      stack)))

(define (score-stacks stacks)
  (define (score-char c)
    (match c
      [")" 1]
      ["]" 2]
      ["}" 3]
      [">" 4]))

  (define (score-stack stack)
    (for/fold ([res 0])
              ([c stack])
      (+ (score-char c) (* 5 res))))

  (define (get-mid-score scores)
    (let ([sorted-scores (sort scores <)]
          [len (length scores)])
      (list-ref sorted-scores (floor (/ len 2)))))

  (get-mid-score (map score-stack stacks)))

(score-stacks (get-stacks (get-input (current-input-port))))

