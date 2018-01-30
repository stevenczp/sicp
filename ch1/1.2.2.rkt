;1.2.2 树形递归(练习 1.11，练习 1.12，练习 1.13)

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