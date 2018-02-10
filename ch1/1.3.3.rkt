(define tolerance 0.000001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (display guess)
    (newline)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
    (try first-guess))

(fixed-point cos 1.0)

;1.3.3 过程作为一般性的方法(练习 1.35，练习 1.36，练习 1.37，练习 1.38，练习 1.39)
;1.35 计算黄金分割
(fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0)

;1.36
(fixed-point (lambda (x) (/ (log 1000) (log x))) 2.0)

;1.37
;a 递归
(define (cont-frac n d k)
  (define (rec i)
    (if (= i 0)
        0
        (/ (n i) (+ (d i) (rec (- i 1))))))
  (rec k))

((lambda (k) (/ 1 (cont-frac (lambda (i) 1.0)
                (lambda (i) 1.0)
                k))) 100)

;迭代
(define (cont-frac n d k)
  (define (iter i result)
    (if (= i 0)
        result
        (iter (- i 1) (/ (n i) (+ (d i) result)))))
  (iter k 0))

((lambda (k) (/ 1 (cont-frac (lambda (i) 1.0)
                (lambda (i) 1.0)
                k))) 100)

;1.38
(define d (lambda (x)
              (if (= 2 (remainder x 3))
                  (* 2 (+ 1 (/ (- x 2) 3)))
                  1)))

((lambda (k) (+ 2(cont-frac (lambda (i) 1.0) d k))) 100)

;1.39
(define (tan-cf x k)
  (define (d i)
    (- (* 2 i) 1))
  (define (n i)
    (if (= i 1)
        x
        (* -1.0 x x)))
  (cont-frac n d k))

(tan 10)
(tan-cf 10 100)