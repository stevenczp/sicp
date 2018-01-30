;1.2.4 求幂(练习 1.16，练习 1.17，练习 1.18，练习 1.19)

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

;1.19
;p' = p^2 + q^2
;q' = 2pq + q^2