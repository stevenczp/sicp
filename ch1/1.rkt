;1.2
(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5))))) (* 3 (- 6 2) (- 2 7)))

;1.3
(define (<= x y)
  (not (> x  y)))
(define (sum-of-max-two x y z)
  (cond ((and (<= x y) (<= x z)) (+ y z))
        ((and (<= y x) (<= y z)) (+ x z))
        (else (+ x y))))
(sum-of-max-two 1 2 3)
(sum-of-max-two 3 2 1)
(sum-of-max-two 2 3 1)
(sum-of-max-two 2 3 2)

;1.4 a + abs(b)

;1.5 infinite loop
(define (p) (p))
(define (test x y)
  (if (= x 0)
      0
      y))
;(test 0 (p))

;1.6 new-if不会做短路求值，所以会导致函数无限展开

;1.7
(define (good-enough? prev now)
  (< (abs (- prev now)) 0.0001))

(define (improve guess x)
  (/ (+ guess (/ x guess)) 2))

(define (sqrt-iter guess x)
  (if (good-enough? guess (improve guess x))
      guess
      (sqrt-iter (improve guess x) x)))

(define (sqrt x)
  (sqrt-iter 1.0 x))

(sqrt 4000000000000000000.0)

