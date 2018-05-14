;;

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (denom x) (numer y))))

;; 这称为 按愿望思维方式， 忽略基本的构造，方式，在一个假设的基础界面中操作

(define (make-rat x y) (cons x y))
(define (numer x) (car x))
(define (denom x) (cdr x))
(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

(define one-half (make-rat 1 2))
(define one-third (make-rat 1 3))

(print-rat (add-rat one-half one-third))
(print-rat (mul-rat one-half one-third))
(print-rat (add-rat one-third one-third))

(define (gcd a b)
  (if (= a 0)
      b
      (gcd (remainder b a) a)))

(define (make-rat x y)
  (let ((g (gcd x y)))
    (cons (/ x g) (/ y g))))

;; 2.1
(define (make-rat x y)
  (let ((g (gcd (abs x) (abs y))))
    (cond ((and (negative? x) (negative? y))
           (cons (abs x) (abs y)))
          ((or (negative? x) (negative? y))
           (cons (- (abs x)) (abs y)))
          (else (cons x y)))))

;; 2.2

(define (make-point x y)
  (cons x y))
(define (x-point point)
  (car point))
(define (y-point point)
  (cdr point))
(define (make-segment point1 point2)
  (cons point1 point2))

(define (start-segment line)
  (car line))
(define (end-segment line)
  (cdr line))


(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display "-")
  (display (y-point p))
  (display ")"))

(define zero-p (make-point 0 0))
(define p-one (make-point 10 10))
(print-point zero-p)
(print-point p-one)

(define (midpoint-segment line)
  (make-point (average (x-point (start-segment line)) (x-point (end-segment line)))
              (average (y-point (start-segment line)) (y-point (end-segment line)))))


(define midpoint (midpoint-segment (make-segment zero-p p-one)))
(print-point midpoint)

;; 2.3 矩形的对于周长和面积的，抽象屏障应该是长宽

;; 2.4

(define (cons x y)
  (lambda (m) (m x y)))
(define (car z)
  (z (lambda (x y) x)))
(define (cdr z)
  (z (lambda (x y) y)))

(define add-del (cons (lambda (x) (+ x 1)) (lambda (y) (- y 1))))


;; 2.5
(define (i-cons x y)
  (33333))

;; 2.6 church 数的意思就是传递参数f, x 0 表示应用f 0次，two 表示应用 f两次，
(define zero (lambda (f) (lambda (x) x)))
(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

(define two (lambda (f) ((lambda (x) (f (f x))))))

;; lambda(f) (lambda (x) (f (((lambda (z) (lambda (y) y)) f) x)))

;; lambda(f) (lambda (x) (f ((lambda (y) y) x)))
;; lambda(f) (lambda (x) (f x))


(define (make-interval a b)
  (cons a b))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (upper-bound y)))
        (p4 (* (upper-bound x) (lower-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (mul-interval x
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y)))))

;; 2.7

(define (lower-bound x)
  (car x))

(define (upper-bound x)
  (cdr x))

;; 2.8

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))


;; 2.9 不用证明了

;; 2.10 2.8 中的sub 会不会出现负数？

(define (make-interval x y)
  (cond ((and (< x 0) (> y 0))
         (error "range include 0"))
        (else (cons x y))))


(define (make-center-width center width)
  (make-interval (- center width) (+ center width)))

;; 2.12
(define (make-center-percent center percent)
  (make-interval (* (- 1 percent) center) (* (+ 1 percent) center)))


(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))
(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval one
                  (add-interval (div-interval one r1)
                                (div-interval one r2)))))

(define par_one (make-interval 0.8 1.1))
(define par_two (make-interval 0.9 1.1))

(par1 par_one par_two)
(par2 par_one par_two)


;; 个人认为其中的误差应该来自于运算，而非公式，如果分数保持为两个整数的表示方法的话，误差是不会出现的。所以误差应该来自与频繁的运算，

;; todo

;;
(define ary (list 1 2 3 4))
(cons 1 (cons 2 (cons 3 nil)))

(define (list-len list)
  (if (null? list)
      0
      (+ 1 (list-len (cdr list)))))

(define (append-list list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append-list (cdr list1) list2))))

(define list1 (list 1 3 4 10))
(define list2 (list 2 4 6 9))

(append-list list1 list2)

(define (last-pair data-list)
  (define (i-last-pair i-list)
    (if (null? (cdr i-list))
        (list (car i-list))
        (i-last-pair (cdr i-list))))
  (cond ((null? data-list) (error "empty list"))
        (else (i-last-pair data-list))))

(last-pair (list 23 72 149 34))

(define (reverse data-list)
  (define (i-ter data i-list)
    (if (null? i-list)
        data
        (i-ter (cons (car i-list) data) (cdr i-list))))
  (i-ter (list) data-list))

(define reverse-value (reverse (list 1 4 9 16 25)))


;;  2.20

(define (same-parity sample . data)
  (define (iter data-list sign)
    (cond ((null? data-list)
           (list))
          ((= (remainder (car data-list) 2) sign)
           (cons (car data-list) (iter (cdr data-list) sign)))
          (else (iter (cdr data-list) sign))))
  (let ((even_data (remainder sample 2)))
    (iter data even_data)))

(same-parity 1 2 3 4 9)

(define (scale-list items factor)
  (if (null? items)
      '()
      (cons (* (car items) factor)
            (scale-list (cdr items) factor))))

(scale-list (list 1 2 3 4 5) 10)

(define (map proc items)
  (if (null? items)
      '()
      (cons (proc (car items))
            (map proc (cdr items)))))


(define (scale-list items factor)
  (map (lambda (x) (* x 10))
       items))


;; 2.21

(define (square-list items)
  (if (null? items)
      '()
      (cons (square (car items))
            (square-list (cdr items)))))
(square-list (list 1 2 3 4 5))


(define (square-list items)
  (map square items))


;; 2.22， 为什么顺序是反向的，因为这个需要关联到 迭代，递归的顺序，

(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons (square (car things))
                    answer))))
  (iter items '()))

(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons answer
                    (square (car things))))))
  (iter items '()))

;; 2.23

(define (for-each proc items)
  (if (null? items)
      (display "--------")
      (begin (proc (car items))
             (for-each proc (cdr items)))))



(for-each (lambda (x) (newline) (display x)) (list 57 321 88))

;; 2.24

(list 1 (list 2 (list 3 4)));;=>(1 (2 (3 4)))

;; 2.25


(define list_1 (list 1 3 (list 5 7) 9))

(cons 1 (cons 3 (cons (cons 5 (cons 7 '())) 9)))


(cdr (car (cdr (cdr list_1))))
(define list_2 (cons (cons 7 '()) '()))

(define list_3 (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))

(cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr list_3)))))))))))



(define (count-leaves tree)
  (cond ((null? tree) 0)
        ((not (pair? tree)) 1)
        (else (+ (count-leaves (car tree))
                 (count-leaves (cdr tree))))))

(define x (cons (list 1 2) (list 3 4)))
(count-leaves (list  x x))


;; 2.26

(define x (list 1 2 3))
(define y (list 4 5 6))
(append x y)
(append-list x y)

(cons x y)

(list x y)

;;  2. 27
(define x (list (list 1 2) (list 3 4)))
(reverse x)

(define (deep-reverse tree)
  (cond ((null? tree) '())
        ((not (pair? tree)) tree)
        (else
         (map deep-reverse (reverse tree)))))


(deep-reverse x)

(define x (list (list 1 2) (list 3 4) (list 5 6)))

;; 2.28 todo need accumulate, list-accumulate

(define (accumulate combiner null-value term items)
  (if (null? items)
    null-value
    (combiner (term (car items)) (accumulate combiner null-value term (cdr items)))))

(define (fringe tree)
  (cond ((null? tree) '())
      ((not (pair? tree)) (list tree))
      (else
        (accumulate (lambda (x y) (append x y)) '() (lambda (x) (fringe x)) tree))))

(define x (list (list 1 2) (list 3 4)))
(define x (list (list 1 2) (list 3 4) (list 20 10)))
(fringe x)

;; 2.29

(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

(define (left-branch mobile)
  (car mobile))
(define (right-branch mobile)
  (car (cdr mobile)))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (car (cdr branch)))

(define (total-weight mobile)
  (define (branch-weight branch)
    (if (not (pair? (branch-structure branch)))
        (branch-structure branch)
        (let ((struct (branch-structure branch)))
          (+ (branch-weight (left-branch struct))
             (branch-weight (right-branch struct))))))
  (+ (branch-weight (left-branch mobile))
     (branch-weight (right-branch mobile))))


(define mobile
  (make-mobile (make-branch 10 10)
               (make-branch 10 (make-mobile (make-branch 10 20)
                                            (make-branch 10 5)))))

(total-weight (branch-structure (right-branch mobile)))

(total-weight mobile)

(define (weight-comfort mobile)
  (define (branch-weight branch)
    (let ((struct (branch-structure branch)))
      (if (not (pair? struct))
          (* (branch-length branch) struct)
          (* (branch-length branch) (total-weight struct)))))
  (= (branch-weight (left-branch mobile))
     (branch-weight (right-branch mobile))))

;;  d 因为存在 界面函数，所以只需要改动4个函数即可
