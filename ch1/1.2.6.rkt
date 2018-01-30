;1.2.6 实例： 素数检测(练习 1.21，练习 1.22，练习 1.23，练习 1.24，练习 1.25，练习 1.26，练习 1.27，练习 1.28)

;1.21
199
1999
7

;1.22
(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))))

(define (report-prime elapsed-time)
  (display "****")
  (display elapsed-time))