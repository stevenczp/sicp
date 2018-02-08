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
;simpson与sum配合求积分
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

(simpson-method cube 0.0 1.0 100)
(simpson-method square 0.0 1.0 100)

;1.30
(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))

(simpson-method cube 0.0 1.0 100)
(simpson-method square 0.0 1.0 100)

;1.31
;a
(define (product term a next b)
  (if (> a b)
      1
      (* (product term (next a) next b) (term a))))

(define (factorial n)
  (define (next x)
    (+ x 1))
  (define (term x)
    x)
  (product term 1 next n))

(factorial 5);120

(define (pi n)
  (define (next x)
    (+ x 1))
  (define (up x)
    (+ x 1 (remainder (+ x 1) 2)))
  (define (down x)
    (- (+ x 2) (remainder (+ x 1) 2)))
  (define (term x)
    (/ (up x) (down x)))
  (* 4.0 (product term 1 next n)))

(pi 100)

;b
(define (product term a next b)
  (define (product-iter a result)
    (if (> a b)
        result
        (product-iter (next a) (* result (term a)))))
  (product-iter a 1))

;1.32
;a
(define (accumlate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a) (accumlate combiner null-value term (next a) next b))))

(define (sum term a next b)
  (define (term x)
    x)
  (define (next x)
    (+ x 1))
  (accumlate + 0 term a next b))