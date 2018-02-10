(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
    (try first-guess))

(fixed-point cos 1.0)

;1.3.3 过程作为一般性的方法(练习 1.35，练习 1.36，练习 1.37，练习 1.38，练习 1.39)
;1.35 计算黄金分割
(fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0)