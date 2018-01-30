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

;1.10
(define (A x y)
  (cond ((= y 0) )
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))

(A 1 10);1024
(A 2 4);65536
(A 3 3);65536

;1.11
(define (f n)
  (if (< n 4)
      n
      (+ (f (- n 1)) (* 2 (f (- n 2))) (* 3 (f (- n 3))))))

(f 10)

;todo 非递归法计算f

;1.12
(define (pascal row col)
  (cond ((= row 1) 1)
        ((= col row) 1)
        (else (+ (pascal (- row 1) col) (pascal (- row 1) (+ col 1))))))

(pascal 5 3)

;1.16 迭代快速幂
(define (square a)
  (* a a))

(define (iter-fast-expt b n a)
  (if (= n 0)
      a
      (if (even? n)
          (iter-fast-expt (square b) (/ n 2) a)
          (iter-fast-expt b (- n 1) (* a b)))))

(define (expt b n)
  (iter-fast-expt b n 1))

(expt 2 99)

;1.17 快速乘
(define (double n)
  (+ n n))

(define (halve n)
  (/ n 2))

(define (mul x y)
  (if (= y 1)
      x
      (+ x (mul x (- y 1)))))

(mul 50 60)
(define (fast-mul x y)
  (if (= y 1)
      x
      (if (even? y)
          (fast-mul (double x) (halve y))
          (+ x (fast-mul x (- y 1))))))

(fast-mul 50 60)

;1.18 迭代快速乘
(define (fast-mul-iter x y a)
  (if (= y 0)
      a
      (if (even? y)
          (fast-mul-iter (double x) (halve y) a)
          (fast-mul-iter x (- y 1) (+ x a)))))

(define (fast-multi x y)
  (fast-mul-iter x y 0))

(fast-multi 50 60)