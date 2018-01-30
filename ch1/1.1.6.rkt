;1.1.6 条件表达式和谓词(练习 1.1，练习 1.2，练习 1.3，练习 1.4，练习 1.5)

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