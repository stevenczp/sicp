;1.1.7 实例： 采用牛顿法求平方根(练习 1.6，练习 1.7，练习 1.8)
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

;1.8
(define (cube-improve y x)
  (/ (+ (/ x (* y y)) y y) 3))

(define (cube-iter guess x)
  (if (good-enough? guess (cube-improve guess x))
      guess
      (cube-iter (cube-improve guess x) x)))

(define (cube x)
  (cube-iter 1.0 x))

(cube 8000000000000000000000000000000)