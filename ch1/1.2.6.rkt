;1.2.6 实例： 素数检测(练习 1.21，练习 1.22，练习 1.23，练习 1.24，练习 1.25，练习 1.26，练习 1.27，练习 1.28)
(define (square x) (* x x))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

;1.21
199
1999
7

;1.22
;查找n以后的k个素数
(define (search-for-prime n k)
  (search-for-prime-log-time n k (runtime )))

(define (search-for-prime-log-time n k start-time)
  (if (= k 0)
      (begin (display (- (runtime ) start-time)) (newline))
      (if (prime? n)
          (search-for-prime-log-time (next-odd n) (- k 1) start-time)
          (search-for-prime-log-time (next-odd n) k start-time))))

(define (next-odd n)
  (if (= (remainder n 2) 0)
      (+ n 1)
      (+ n 2)))

(search-for-prime 1000000 300);97756
(search-for-prime 10000000 300);309314
(search-for-prime 100000000 300);982565
(search-for-prime 1000000000 300);3053576
;在计算量比较大的时候，时间复杂度才会有所体现

;1.23
;对1.22的优化
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next-odd test-divisor)))))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (search-for-prime n k)
  (search-for-prime-log-time n k (runtime )))

(define (search-for-prime-log-time n k start-time)
  (if (= k 0)
      (begin (display (- (runtime ) start-time)) (newline))
      (if (prime? n)
          (search-for-prime-log-time (next-odd n) (- k 1) start-time)
          (search-for-prime-log-time (next-odd n) k start-time))))

(search-for-prime 1000000 300);70214
(search-for-prime 10000000 300);207421
(search-for-prime 100000000 300);650740
(search-for-prime 1000000000 300);1929370

;1.24
;用费马检查来测试素数
(define (search-for-prime-fast n k)
  (search-for-prime-fast-log-time n k (runtime )))

(define (search-for-prime-fast-log-time n k start-time)
  (if (= k 0)
      (begin (display (- (runtime ) start-time)) (newline))
      (if (fast-prime? n 10)
          (search-for-prime-fast-log-time (next-odd n) (- k 1) start-time)
          (search-for-prime-fast-log-time (next-odd n) k start-time))))
(search-for-prime-fast 1000000 300);40637
(search-for-prime-fast 10000000 300);53610
(search-for-prime-fast 100000000 300);64641
(search-for-prime-fast 1000000000 300);97227

;1.25
;直接计算会溢出，就算有内置的大整数支持，也会带来额外的开销

;1.26
;如果写成两个式子的乘法，会导致系统对这两个式子递归的展开，相当于构建一颗深度为log2(n)的满二叉树，这会带来O(n)的开销

;1.27
;(define (carmichael-test n))