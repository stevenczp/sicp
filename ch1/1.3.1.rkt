;1.3.1 过程作为参数(练习 1.29，练习 1.30，练习 1.31，练习 1.32，练习 1.33)
(define (cube x)
  (* x x x))
(define (square x)
  (* x x))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

;1.29
(define (simpson-method f a b n)
  (let ((h (/ (- b a) n)))
    (define (y k)
      (f (+ a (* k h))))
    (define (add-two x)
      (+ x 2))
    (define (simpson-method-term k)
      (+ (* 4 (y (- k 1))) (* 2 (y (- k 2)))))
    (*
     (/ h 3.0)
     (+
      (y 0)
      (* 4 (y 1))
      (y n)
      (sum simpson-method-term 4 add-two n)))))

(simpson-method cube 0.0 1.0 2)
(simpson-method square 0.0 1.0 100)