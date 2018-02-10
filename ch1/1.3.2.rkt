(define (square x)
  (* x x))

;1.3.2 用 lambda 构造过程(练习 1.34)
(define (f g)
  (g 2))

(f square);4
(f (lambda (z) (* z (+ z 1))));6

(f f);编译出错
;因为(f f)最终会变成(f 2)，传给f的参数是数字2，但是根据定义，f的参数应该是一个函数过程