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

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (improve guess)
  (average guess (/ x guess)))

(define (square x)
  (* x x))

(define (average a b)
  (/ (+ a b ) 2))

;; 注意其中编写的顺序，自上而下，在数学中运算十分有用
