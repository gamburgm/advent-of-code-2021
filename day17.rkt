#lang racket

(define (get-input port)
  (define (read-target-axis spec)
    (match-define (list fst snd) (string-split (substring spec 2) ".."))
    (cons (string->number fst)
          (string->number snd)))

  (define line (read-line port))
  (define target-str (substring line 13)) ;; I think this is the right position
  (match-define (list x-target y-target) (string-split target-str ", "))
  (cons (read-target-axis x-target)
        (read-target-axis y-target)))

(define (get-sum n)
  (/ (* n (add1 n)) 2))

;; part 1
;; This solution depends on the following logic, culminating in a neat hack:
;; 1. We know that, if the y-velocity is positive, the position will eventually become 0 again.
;;    For example, take y-vel=3. The position goes up 3, then up 2, then up 1, then doesn't move,
;;    then down 1, then down 2, then down 3. After 7 steps, we're back at 0.
;;
;; 2. After the position returns to 0, it will move down (initial y-velocity) + 1. For example,
;;    in the above example, the position next goes down 4, ending up at -4.
;;
;; 3. If the target is entirely below the x-axis, then the highest y-velocity that you could
;;    possibly use is 1 - the y-lower-bound for the target. If you try a higher velocity, then
;;    the position will hit 0, then skip over the target immediately after. So, _if_ there was
;;    an x-velocity such that, coupled with that y-velocity, the position entered the target,
;;    that must be the right solution.
;;
;; 4. I _assume_ that such an x-velocity exists. This seems likely in the average case because
;;    the requirement is simply that either at least one x-velocity causes the x-position to
;;    max out within the target before the y-position enters the target, or that there is an
;;    x-velocity greater than (2 * y-velocity + 1) that hits the target at the right time.
;;    It is easy to construct counter-examples. Fortunately, I didn't have to deal with that.
(define (part-1-solution target)
  (match-define (cons (cons x-low x-up)
                      (cons y-low y-up))
                target)

  (get-sum (sub1 (abs y-low))))

;; part 2
;; brute force all sets of velocities that _could_ result in hitting the target
(define (part-2-solution target)
  (match-define (cons (cons x-lower x-upper)
                      (cons y-lower y-upper))
                target)

  (define (missed? x-pos y-pos y-vel)
    (or (> x-pos x-upper)
        (and (< y-pos y-lower)
             (< y-vel 0))))

  (define (hit? x-pos y-pos)
    (and (>= x-pos x-lower)
         (<= x-pos x-upper)
         (>= y-pos y-lower)
         (<= y-pos y-upper)))

  (define (move pos vel)
    (+ pos vel))

  (define (apply-drag x-vel)
    (cond
      [(zero? x-vel) 0]
      [(< x-vel 0) (add1 x-vel)]
      [(> x-vel 0) (sub1 x-vel)]))

  (define (apply-gravity y-vel)
    (sub1 y-vel))

  (define (hits-trench? x-pos y-pos curr-x-vel curr-y-vel)
    (define coord (cons x-pos y-pos))
    (cond
      [(missed? x-pos y-pos curr-y-vel) #f]
      [(hit? x-pos y-pos) #t]
      [else
       (hits-trench? (move x-pos curr-x-vel)
                     (move y-pos curr-y-vel)
                     (apply-drag curr-x-vel)
                     (apply-gravity curr-y-vel))]))

  (for*/sum ([x-vel (in-range (add1 x-upper))]
             [y-vel (in-range y-lower (abs y-lower))]
             #:when (hits-trench? 0 0 x-vel y-vel))
    1))

(part-2-solution (get-input (current-input-port)))
