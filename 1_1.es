;; 1.2
(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5))))) (* 3 (- 6 2) (- 2 7)))

;; 1.3
(define (sum_bigger a b c )
  (if (and (> a c) (> b c))
      (+ a b)
      (sum_bigger c a b)))

;; 1.5

(define (p) (p))

;; 正则序： 无法展开， 死循环, 应用序：可展开, 可以用来判断求值应用模型
(define (test x y)
  (if (= x 0)
      0
      y))

;; 无法判断正则序，应用序列，没有差别


;;1.1.7 牛顿法 求平方根

(define (sqrt x)
  (sqrt-iter 1.0 x))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (improve guess x)
  (average guess (/ x guess)))

(define (square x)
  (* x x))

(define (average a b)
  (/ (+ a b ) 2))

;; 注意其中编写的顺序，自上而下，在数学中运算十分有用

;; 1.6
(define (new-if predicate then-caluse else-caluse)
  (cond (predicate then-caluse)
        (else else-caluse)))


(new-if (= 2 3) 0 5)

;; 为什么不能代替 if，因为 new-if 是过程，(new-if predicate then-caluse else-caluse)是过程调用，会eval then-caluse and else-caluse，产生不到if的效果，不能够表达题目要求

;; 1.7 答案是可行的，设定初始值是个难点，其他的检验 good-enough 可能更好?
(define (i-sqrt x)
  (i-sqrt-iter 1.0 x x))
(define (i-sqrt-iter guess-a guess-b x)
  (if (i-good-enough? guess-a guess-b)
      guess-a
      (i-sqrt-iter (improve guess-a x)
                   guess-a x)))
(define (i-good-enough? a b)
  (< (abs (- (abs a) (abs b))) 0.0001))

;; 1.8  使用公式 (x/y^2 + 2 * y/ 3) 来更改improve，求立方根

;; (define (improve guess x)
;;   (/ (+ (× 2 guess) (/ x (* guess guess guess))) 3))

;; sqrt 使用块结构，重写，这里面基于一个事实： x并不会被改变和恢复，所以不需要在过程中传递参数来进行调用。

(define (another-sqrt x)
  (define (square a)
    (* a a))
  (define (average a b)
    (/ (+ a b) 2))
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.001))
  (define (improve guess)
    (average guess (/ x guess)))
  (define (sqrt-iter guess)
    (if (good-enough? guess)
        guess
        (sqrt-iter (improve guess))))

  (sqrt-iter x))


;; 1.9 其中inc  dec 是已经定义的函数，将操作数加减1， 使用代换模型 展开 过程 (+ 4 5) 的调用所产的计算过程如何？
(define (inc a)
  (+ a 1))

(define (+ a b )
  (if (= a b)
      b
      (inc (+ (dec a) b))))

;; 迭代的计算过程

(define (+ a b)
  (if (= a 0)
      b
      (+ (dec a) (inc b))))

;; 递归的计算过程， 因为 + 为两个参数需要 不断的保持2个参数，在展开过程中的数值

;; 1.10
(define (a x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (a (- x 1)
                 (a x (- y 1))))))

;;  没看打出来是什么数学公式

;; 1.11 n < 3 return n else fn(n) => fn(n - 1) + 2 * fn(n - 2) + 3 * fn(n - 3)

(define (f_11 n)
  (if (< n 3)
      n
      (+ (f_11 (- n 1)) (* 2 (f_11 (- n 2))) (* 3 (f_11 (- n 3))))))


(define (f_12 n)
  (define (fn start end n1 n2 n3)
    (if (= start end)
        n1
        (fn (+ start 1)
            end
            (+ n1 (* 2 n2) (* 3 n3))
            n1
            n2)))
  (define (warp_fn n)
    (if (< n 3)
        n
        (fn 2 n 2 1 0)))
  (warp_fn n))



;; 求幕

(define (expt b n)
  (if (= 0 n)
      1
      (* b (expt b (- n 1)))))

(define (i-expt b n)
  (define (expt-iter b counter product)
    (if (= counter 0)
        product
        (expt-iter b (- counter 1) (* b product))))

  (expt-iter b n 1))

(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))


(define (i-fast-expt b n)
  (define (i-fast-expt-iter b n sum)
    (cond ((= n 0) sum)
          ((even? n) (i-fast-expt-iter (square b) (/ n 2) sum))
          (else (i-fast-expt-iter b (- n 1) (* b sum)))))
  (i-fast-expt-iter b n 1))


;; 1.17 假设语言没有乘法，使用加法实现乘法
(define (i-* a b )
  (if (= b 0)
      0
      (+ a (i-* a (- b 1)))))

(define (double a)
  (+ a a))

(define (fast-* a b)

  (define (fast-*-iter a b sum)
    (cond ((= a 0) sum)
          ((= b 0) sum)
          ((even? a) (fast-*-iter (/ a 2) (double b) sum))
          (else (fast-*-iter (- a 1)
                             b
                             (+ b sum)))))
  (fast-*-iter a b 0))


;; 1.2.6

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n)
         test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b )
  (= (remainder b a) 0))

(define (prime? n)
  (= (smallest-divisor n) n))

;; 1.21

;; (smallest-divisor 199) 199
;; (smallest-divisor 1999) 1999
;; (smallest-divisor 19999) 7

;; 1.22

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))))

(define (report-prime elspsed-time)
  (display "*****")
  (display elspsed-time))


(define (search-for-primes start counter)
  (cond ((= counter 0)
         (newline)
         (display "done----"))
        ((prime? start)
         (display start)
         (newline)
         (search-for-primes (+ start 1) (- counter 1)))
        (else
         (search-for-primes (+ start 1) counter))))

(define (search-for-primes-time start counter)
  (define time-start (real-time-clock))
  (search-for-primes start counter)
  (- (real-time-clock) time-start))

;; (define (search-for-primes small big)
;;   (define start-time (runtime))
;;   (define (test-prime? num)
;;     (if (and (>= num small) (< num big))
;;         (if (prime? num)
;;             (show-prime-num num)
;;             (test-prime? (+ num 1)))))

;;   (define (show-prime-num num)
;;     (display "numer is:")
;;     (display num)
;;     (define diff (- (runtime) start-time))
;;     (display "time used")
;;     (display diff))

;;   (test-prime? small))

;; 并没有证实耗时时间上的 ^ 10 的比例， 关于计算步骤也同样不正确。
;; 1000 => 2 10000 => 3 100000 => 9 1000000 => 13 10000000 => 25


;; 1.23

(define (find-divisor n test-divisor)
  (define (next-divisor num)
    (cond ((= num 2) 3)
          (else (+ num 2))))
  (define (find-divisor-iter test)
    (cond ((> (square test) n) n)
          ((divides? test n)
           test)
          (else (find-divisor-iter (next-divisor test)))))
  (find-divisor-iter test-divisor))

;; 1000 => 2, 10000 => 3, 100000 => 7 1000000 => 9, 10000000 => 19

;; 并不符合2倍的事实， 如何解释，计算next的函数改成了函数调用。增加了函数的调用时间。

;; 1.24

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test 0) (fast-prime? n (- times 1)))
        (else false)))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m)) m))
        (else (remainder (* base (expmod base (- exp 1) m))
                         m))))


(define (start-prime-test n start-time)
  (if (fast-prime? n 10)
      (report-prime (- (runtime) start-time))))

(define (prime? n)
  (fast-prime? n 10))

(define (search-for-primes start counter)
  (cond ((= counter 0)
         (newline)
         (display "done----"))
        ((prime? start)
         (display start)
         (newline)
         (search-for-primes (+ start 1) (- counter 1)))
        (else
         (search-for-primes (+ start 1) counter))))


;; 1.3

(define (cube x) (* x x x))
(define (sum-integer a b)
  (if (> a b)
      0
      (+ a (sum-integer (+ a 1) b))))
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (inc a) (+ a 1))

;; i-sum-integer equal sum-integer, build base on sum
(define (identity z) z)
(define (i-sum-integer a b)
  (sum identity a inc b))



;; (define (xinpule-integer f a b n)
;;   (define (i-f x)
;;     (let x (/ (- x a) h)
;;       (cond x))
;;     (cond (())))
;;   (define h (/ (- b a) n))
;;   (define (next x) (+ h x))
;;   (sum f a next b)

;; 1.29 todo

;; 1.30
(define (sum term a next b)
  (define (iter num sum-num)
    (if (> num b)
        sum-num
        (iter (next num) (+ sum-num (term num)))))

  (iter a 0))


;; 1.31

(define (product term a next b)
  (if (> a b)
      1
      (* (term a) (product term (next a) next b))))

(define (product term a next b)
  (define (iter num product-num)
    (if (> num b)
        product-num
        (iter (next num) (* (term num) product-num))))
  (iter a 1))


(define (pai n)
  (define (term k)
    (define (fenzi k)
      (+ 2 (* 2 (integer-floor k 2))))
    (define (fenmu k)
      (+ (* (integer-floor (- k 1) 2) 2) 3))

    (/ (fenzi k)
       (fenmu k)))

  (product term 1 (lambda (x) (+ x 1)) n))

;; 1.32

(define (accumulate combiner null-value term a next b)
  (define (iter num sum-num)
    (if (> num b)
      sum-num
      (iter (next num) (combiner (term num) sum-num))))
  (iter a null-value))

(define (accumulate combiner null-value term a next b)
  (if (> a b)
    null-value
    (combiner (term a) (accumulate combiner null-value term (next a) next b)))

;; 测试迭代版本跟递归版本的区别
(define (combiner a b)
  (newline)
  (display a)
  (display "----")
  (display b)
  (+ a b))

(define (sum term a next b)
  (accumulate + 0 term a next b))

(define (product term a next b)
  (accumulate * 1 term a next b))

;;1.33

(define (filtered-accumulate filter combiner null-value term a next b)
  (define (iter num sum-num)
    (if (> num b)
      sum-num
      (iter (next num)
        (if (filter num)
            (combiner (term num) sum-num)
            sum-num))))
  (iter a null-value))
