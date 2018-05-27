`;;

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

;;

(define (branch-weight branch)
  (if (pair? (branch-structure branch))
      (* (total-weight (branch-structure branch)) (branch-length branch))
      (* (branch-structure branch) (branch-length branch))))

(define (weight-comfort mobile)
  (and (= (branch-weight (left-branch mobile))
          (branch-weight (right-branch mobile)))
       (if (pair? (branch-structure (left-branch mobile)))
           (weight-comfort (branch-structure (left-branch mobile)))
           true)
       (if (pair? (branch-structure (right-branch mobile)))
           (weight-comfort (branch-structure (right-branch mobile)))
           true)))


(define comfort-mobile
  (make-mobile (make-branch 1 (make-mobile (make-branch 10 10)
                                           (make-branch 5 20)))
               (make-branch 2 15)))


(total-weight (branch-structure (left-branch comfort-mobile)))
(total-weight (branch-structure (left-branch comfort-mobile)))



(branch-length (left-branch comfort-mobile))
(total-weight (branch-structure (left-branch comfort-mobile)))

(branch-weight (left-branch comfort-mobile))

(branch-structure (left-branch mobile))

(weight-comfort mobile)

;;  d 因为存在 界面函数，所以只需要改动4个函数即可

(define (make-mobile left right)
  (cons left right))
(define (make-branch length structure)
  (cons length structure))


(define (left-branch mobile)
  (car mobile))
(define (right-branch mobile)
  (cdr mobile))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (cdr branch))


;; 2.30

(define (square-tree tree)
  (cond ((null? tree) '())
        ((not (pair? tree)) (* tree tree))
        (else (cons (square-tree (car tree))
                    (square-tree (cdr tree))))))


(square-tree (list 1 (list 2 (list 3 4) 5) (list 5 7)))

(define (square-tree tree)
  (map (lambda (x)
         (if (not (pair? x))
             (* x x)
             (square-tree x)))
       tree))


(define (tree-map square tree)
  (map (lambda (x)
         (if (not (pair? x))
             (square x)
             (tree-map square x)))
       tree))

(define (square-tree tree)
  (tree-map square tree))

;; 2.32
;; 排列组合的简单的方式，可以总结为存在A与不存在A 的集合相加

(define (subsets s)
  (if (null? s)
      (list '())
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (x) (cons (car s) x)) rest)))))

(subsets (list 1 2 3))


(define (sum-odd-squares tree)
  (cond ((null? tree) 0)
        ((not (pair? tree))
         (if (odd? tree)
             (square tree)
             0))
        (else (+ (sum-odd-squares (car tree))
                 (sum-odd-squares (cdr tree))))))


(define (even-fibs n)
  (define (next k)
    (if (> k n)
        '()
        (let ((f (fib k)))
          (if (even? f)
              (cons f (next (+ k 1)))
              (next (+ k 1))))))

  (next 0))


(define (filter predicate sequence)
  (cond ((null? sequence) '())
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))


(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low (enumerate-interval (+ low 1) high))))

(define (even-fibs n)
  (accumulate cons
              '()
              (filter even?
                      (map fib
                           (enumerate-interval 0 n)))))




(define (enumerate-tree tree)
  (cond ((null? tree) '())
        ((not (pair? tree)) (list tree))
        (else (append (enumerate-tree (car tree))
                      (enumerate-tree (cdr tree))))))


(define (sum-odd-squares tree)
  (accumulate +
              0
              (map square
                   (filter odd?
                           (enumerate-tree tree)))))


;; 2.33

(define (map p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) '() sequence))

(map (lambda (x) (* x x)) (list 1 2 3 4 5))


(define (i-append seq1 seq2)
  (accumulate cons seq2 seq1))

(append (list 1 3 4) (list 2 4 5))
(i-append (list 1 3 4) (list 2 4 5))

(define (i-length sequence)
  (accumulate (lambda (y x) (+ x 1)) 0 sequence))

(i-length (list 1 2 3 4 5 6))

;; 2.34

(define (horner-eval x sequence)
  (accumulate (lambda (left right)
                (+ left (* x right)))
              0 sequence))

(horner-eval 2 (list 1 3 0 5 0 1))

(+ 32 40 6 1)

;; 2.35

(define (i-count-leaves tree)
  (accumulate (lambda (x y) (+ y x)) 0 (map (lambda (x) (count-leaves x)) tree)))


(i-count-leaves (list  x x))

(define (accumulate-n op init sequence)
  (if (null? sequence)
      '()
      (cons (accumulate op init (map car sequence))
            (accumulate-n op init (if (null? (cdr (car sequence)))
                                      '()
                                      (map cdr sequence))))))


(accumulate-n + 0 (list (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12)))


(define (accumulate-n op init sequence)
  (if (null? (car sequence))
      '()
      (cons (accumulate op init (map car sequence))
            (accumulate-n op init (map cdr sequence)))))


;; 2.37 TODO

;; 2.38 op 函数应该满足 (op a b) = (op b a)

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))

  (iter initial sequence))

(fold-left / 1 (list 1 2 3))

(accumulate / 1 (list 1 2 3))

(accumulate list '() (list 1 2 3))

(fold-left list '() (list 1 2 3))


;;2.39

(define (fold-left-reverse sequence)
  (fold-left (lambda (x y) (cons y x)) '() sequence))

(fold-left-reverse (list 1 2 4))

(define (fold-right-reverse sequence)
  (accumulate (lambda (x y) (append y (list x))) '() sequence))

(fold-right-reverse (list 1 2 4))


(define (flatmap pro seq)
  (accumulate append '() (map pro seq)))


;; 2.40

(define (unique-pair n)
  (flatmap (lambda (i) (map (lambda (y) (list i y)) (enumerate-interval 1 (- i 1)))) (enumerate-interval 1 n)))

(unique-pair 10)

;; 2.41

(define (three-combine n)
  (flatmap (lambda (one)
             (map (lambda (two) (map (lambda (three) (list one two three)) (enumerate-interval 1 (- two 1))))
                  (enumerate-interval 2 (- one 1)))) (enumerate-interval 3 n)))

(define (three-sum-equal s n)
  (filter (lambda (data)
            (cond ((null? data) false)
                  ((= (+ (car data) (cadr data) (caddr data)) s) true))) (three-combine n)))
(three-combine 10)

(three-sum-equal 45 10)

(caddr (list 1 2 3))



;; 2.42
(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (position) (safe? k position))
         (flatmap
          (lambda (rest-of-position)
            (map
             (lambda (row) (adjoin-position row k rest-of-position))
             (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))

  (queen-cols board-size))
(define empty-board '())
(define (adjoin-position row k rest-of-position)
  (display "-----------")
  (display rest-of-position)
  (newline)
  (let ((curr (append (list (cons  k row)) rest-of-position)))
    (display curr)
    curr))

(define (safe? k positions)
  (define (safe-check pos1 pos2)
    (let ((post1_x (car pos1))
          (post1_y (cdr pos1))
          (post2_x (car pos2))
          (post2_y (cdr pos2)))
      (and (not (= post1_x post2_x))
           (not (= post1_y post2_y))
           (not (= (abs (- post1_x post2_x)) (abs (- post1_y post2_y)))))))

  (define (iter point points)
    (if (null? points)
        true
        (and (safe-check point (car points))
             (iter point (cdr points)))))
  (iter (car positions) (cdr positions)))

(safe? 1 (list (cons 1 2) (cons 3 3)))



(queens 5)

;; 2.43 因为queen-clos 被求值多次
(define (memq item x)
  (cond ((null? x) false)
        ((eq? item (car x)) x)
        (else (memq item (cdr x)))))

(memq 'apple '(x (apple sauce) y apple pear))


(list 'a 'baa)
(car (cdr '(a b c)))


;; 2.53
(list 'a 'b 'c)
(list (list 'georage))
(cdr '((x1 x2) (y1 y2)))
(cadr '((x1 x2) (y1 y2)))
(pair? (car '(a short list)))
(pair? '11)
(memq 'red '((red shoes) (blue socks)))
(memq 'red '(red shoes blue shocks))

;; 2.54

(pair? '(a b c))

(define (equal? a b)
  (cond ((and (null? a) (null? b)) true)
        ((and (not (pair? a)) (not (pair? b))) (eq? a b))
        ((and (pair? a) (pair? b))
         (and (eq? (car a) (car b)) (equal? (cdr a) (cdr b))))
        (else false)))

(equal? '(this is a table) '(this is a table))
(equal? '(this is a table) '(this (is a) table))

;; 2.55

(car '''abcd)
''abcd
'''abcd

;;从打印结果来看，　''abcd === (quote abcd) '''abcd == (quote (quote abcd))


;;


(define (variable? x)
  (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))


(define (make-sum a1 a2)
  (list '+ a1 a2))

(define (make-product a1 a2)
  (list '* a1 a2))

(define (sum? v1)
  (and (pair? v1) (eq? (car v1) '+)))

(define (addend a)
  (cadr a))
(define (augend a)
  (caddr a))

(define (product? a)
  (and (pair? a) (eq? (car a) '*)))

(define (multipiler a)
  (cadr a))

(define (multipilcand a)
  (caddr a))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum (make-product (multipiler exp)
                                 (deriv (multipilcand exp) var))
                   (make-product (deriv (multipiler exp) var)
                                 (multipilcand exp))))))


(deriv '(+ x 2) 'x)


(deriv '(+ x y) 'x)
(deriv '(* x y) 'x)


(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))

(define (=number? a v)
  (and (number? a) (= a v)))


(define (make-product a1 a2)
  (cond ((or (=number? a1 0) (=number? a2 0)) 0)
        ((=number? a1 1) a2)
        ((=number? a2 1) a1)
        ((and (number? a1) (number? a2)) (* a1 a2))
        (else (list '* a1 a2))))


(deriv '(+ x 3) 'x)
(deriv '(* x y) 'x)
(deriv '(* (* x y) (+ x 3)) 'x) ;; 并不是最简单的形式

;; 2.56

(define (make-exponentiation num var)
  (list '^ num var))


(list '+ 10 20)
(define (exponentiation? var)
  (eq? (car var) '^))

(define (base-exponentiation exp)
  (cadr exp))

(define (exponentiation-var exp)
  (caddr exp))
(base-exponentiation (make-exponentiation 10 'x))


(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((exponentiation? exp)
         (cond ((eq? (exponentiation-var exp) var)
                (make-product (base-exponentiation exp)
                              (make-exponentiation (- (base-exponentiation exp) 1) var)))
               ((not (eq? (exponentiation-var exp)) var)
                0)
               ((= (base-exponentiation exp) 0)
                1)
               ((= (base-exponentiation exp) 1)
                var)))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum (make-product (multipiler exp)
                                 (deriv (multipilcand exp) var))
                   (make-product (deriv (multipiler exp) var)
                                 (multipilcand exp))))))


(deriv (make-exponentiation 10 'x) 'x)
(define (variable? x)
  (display "-variable")
  (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))


(define (make-sum a1 a2)
  (list '+ a1 a2))

(define (make-product a1 a2)
  (list '* a1 a2))

(define (sum? v1)
  (and (pair? v1) (eq? (car v1) '+)))

(define (addend a)
  (cadr a))
(define (augend a)
  (caddr a))

(define (product? a)
  (and (pair? a) (eq? (car a) '*)))

(define (multipiler a)
  (cadr a))

(define (multipilcand a)
  (caddr a))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum (make-product (multipiler exp)
                                 (deriv (multipilcand exp) var))
                   (make-product (deriv (multipiler exp) var)
                                 (multipilcand exp))))))


(deriv '(+ x 2) 'x)


(deriv '(+ x y) 'x)
(deriv '(* x y) 'x)


(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))

(define (=number? a v)
  (and (number? a) (= a v)))


(define (make-product a1 a2)
  (cond ((or (=number? a1 0) (=number? a2 0)) 0)
        ((=number? a1 1) a2)
        ((=number? a2 1) a1)
        ((and (number? a1) (number? a2)) (* a1 a2))
        (else (list '* a1 a2))))


(deriv '(+ x 3) 'x)
(deriv '(* x y) 'x)
(deriv '(* (* x y) (+ x 3)) 'x) ;; 并不是最简单的形式

;; 2.56

(define (make-exponentiation num var)
  (list '^ num var))


(list '+ 10 20)
(define (exponentiation? var)
  (eq? (car var) '^))

(define (base-exponentiation exp)
  (cadr exp))

(define (exponentiation-var exp)
  (caddr exp))
(base-exponentiation (make-exponentiation 10 'x))


(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((exponentiation? exp)
         (cond ((not (eq? (exponentiation-var exp) var))
                0)
               ((= (base-exponentiation exp) 0)
                1)
               ((= (base-exponentiation exp) 1)
                var)
               (else
                (make-product (base-exponentiation exp)
                              (make-exponentiation (- (base-exponentiation exp) 1) var)))))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum (make-product (multipiler exp)
                                 (deriv (multipilcand exp) var))
                   (make-product (deriv (multipiler exp) var)
                                 (multipilcand exp)))))))


(deriv (make-exponentiation 10 'x) 'x)
(deriv (make-exponentiation 0 'x) 'x)


;; 2.57

(define (augend exp)
  (cond ((null? (cdddr exp))
         (caddr exp))
        (else
         (append (list '+) (list (caddr exp)) (cdddr exp)))))
(append (list '+) (cddr exp)))
(define (multipilcand exp)
  (cond ((null? (cdddr exp))
         (caddr exp))
        (else
         (append (list '*) (list (caddr exp)) (cdddr exp)))))


(cadr '(* x y (+ x 3)))
(deriv '(* x y (+ x 3)) 'x)

(augend (multipilcand (multipilcand '(* x y (+ x 3)))))


;; 2.58

(define (make-sum a1 a2)
  (list a1 '+ a2))

(define (sum? exp)
  (and (pair? exp)
       (eq? (cadr exp) '+)))

(sum? (make-sum 10 'x))


(define (addend exp)
  (car exp))
(define (augend exp)
  (caddr exp))

(addend (make-sum 10 'x))
(augend (make-sum 10 'x))


(define (make-product a1 a2)
  (list a1 '* a2))

(define (product? exp)
  (and (pair? exp)
       (eq? (cadr exp) '*)))

(product? (make-product 10 'x))

(define (multipiler exp)
  (car exp))

(define (multipilcand exp)
  (caddr exp))

(multipiler (make-product 10 'x))
(multipilcand (make-product 10 'x))


;; 因为存在表达式优先级的概念，应该不会简单的使用组合，　递归，闭包就可以简单的完成

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else
         (element-of-set? x (cdr set)))))


(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))


(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((element-of-set? (car set1) set2)
         (union-set (cdr set1) set2))
        (else (union-set (cdr set1) (cons (car set1) set2)))))

(union-set (list 1 2 4) (list 1 4 5))

;; 2.60




(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (car set)) true)
        ((< x (car set)) false)
        (else (element-of-set? x (cdr set)))))


(define (adjoin-set x set)
  (define (insert-set i-set)
    (cond ((null? i-set) (list x))
          ((< x (car i-set)) (cons x i-set))
          ((> x (car i-set)) (cons (car i-set) (insert-set (cdr i-set))))))
  (if (element-of-set? x set)
      set
      (insert-set set)))


(adjoin-set 10 (list 1 3 4 101))
(adjoin-set 10 (list 1 111 12121))


(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((= (car set1) (car set2))
         (cons (car set1) (intersection-set (cdr set1) (cdr set2))))
        ((> (car set1) (car set2))
         (intersection-set set1 (cdr set2)))
        ((< (car set1) (car set2))
         (intersection-set (cdr set1) set2))))


(intersection-set (list 1 2 3) (list 2 4))


;; 2.61


(define (adjoin-set x set)
  (define (insert-set i-set)
    (cond ((null? i-set) (list x))
          ((< x (car i-set)) (cons x i-set))
          ((> x (car i-set)) (cons (car i-set) (insert-set (cdr i-set))))))
  (if (element-of-set? x set)
      set
      (insert-set set)))


;; 2.62

(define (union-set set1 set2)
  (cond ((and (null? set1) (null? set2)) '())
        ((null? set1) set2)
        ((null? set2) set1)
        (else
         (let ((x1 (car set1)) (x2 (car set2)))
           (cond ((= x1 x2)
                  (cons x1 (union-set (cdr set1) (cdr set2))))
                 ((< x1 x2)
                  (cons x1 (union-set (cdr set1) set2)))
                 ((> x1 x2)
                  (cons x2 (union-set set1 (cdr set2)))))))))


(union-set (list 1 2 4 100) (list 1 2 4 6 10))



(define (make-tree entry left right)
  (list entry left right))
(define (left-branch tree)
  (cadr tree))
(define (right-branch tree)
  (caddr tree))

(define (entry tree)
  (car tree))

(define tree  (make-tree 10 (make-tree 5 '() '())
                         (make-tree 12 '() '())))

(right-branch (make-tree 1 (make-tree 1 '() '())
                         (make-tree 2 '() '())))

(entry (make-tree 1 (make-tree 1 '() '())
                  (make-tree 2 '() '())))

(define (element-of-set? x tree)
  (cond ((null? tree) false)
        ((= (entry tree) x) true)
        ((< x (entry tree)) (element-of-set? x (left-branch tree)))
        ((> x (entry tree)) (element-of-set? x (right-branch tree)))))

(element-of-set? 1 tree)


(define (adjoin-tree x tree)
  (cond ((null? tree) (make-tree x '() '()))
        ((= x (entry tree)) tree)
        ((< x (entry tree))
         (make-tree (entry tree)
                    (adjoin-tree x (left-branch tree))
                    (right-branch tree)))
        ((> x (entry tree))
         (make-tree (entry tree)
                    (left-branch tree)
                    (adjoin-tree x (right-branch tree))))))

()

(adjoin-tree 100 tree)

;; 2.63

(define (tree-list-1 tree)
  (if (null? tree)
      '()
      (append (tree-list-1 (left-branch tree))
              (cons (entry tree)
                    (tree-list-1 (right-branch tree))))))



(tree-list-1 (adjoin-tree -2 (adjoin-tree 6 tree)))

(define (tree-list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree) result-list)))))
  (copy-to-list tree '()))

(tree-list-2 (adjoin-tree -2 (adjoin-tree 6 tree)))

;; 从结果上看是相同的, b) cons　整体的循环递归是一样的，cons 更高效一点

;; 2.64

(define (list-tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elements len)
  (if (= len 0)
      (cons '() elements)
      (let ((left-size (quotient len 2)))
        (let ((left-result (partial-tree elements left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- len (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result (partial-tree (cdr non-left-elts)
                                              right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts (cdr right-result)))
                (cons (make-tree this-entry left-tree right-tree)
                      remaining-elts))))))))


(list-tree (list 1 3 5 7 9 11))


;; 2.65 可以将函数结合起来，　转比那成list 求和之后，在转换为tree


;; 2.66

(define (make-entry key value)
  (cons key value))
(define (entry-key entry)
  (car entry))
(define (entry-value entry)
  (cdr value))

(define (look-up key tree)
  (if (null? tree)
      '()
      (let  ((value (entry-key (entry tree)))
             (i-entry (entry tree)))
        (cond ((= value key)
               i-entry)
              ((> value key)
               (look-up key (left-branch tree)))
              ((< value key)
               (look-up key (right-branch tree)))))))




;; huffman

(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? object)
  (eq? (car object) 'leaf))

(define (symbol-leaf x)
  (cadr x))

(define (weight-leaf x)
  (caddr x))

(define (make-code-tree left right)
  (display left)
  (display '*****)
  (display right)
  (display '-------------)
  (newline)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))


(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set)))
         (cons x set))
        (else (cons
               (car set)
               (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)
                               (cadr pair))
                    (make-leaf-set (cdr pairs))))))


(define pairs (list (list 'A 4) (list 'B 2) (list 'C 1) (list 'D 1)))

(make-leaf-set pairs)
(define (find-mix array)
  (cons (list (car array) (cadr array)) (cddr array)))

(define (huffman pairs)
  (if (= 1 (length pairs))
      (make-code-tree (make-leaf " " 0)
                      (car pairs))
      (let ((mix-pairs (find-mix pairs)))
         (let ((mix (car mix-pairs))
               (elts (cdr mix-pairs)))
           (huffman (adjoin-set (make-code-tree (car mix) (cadr mix)) elts))))))


(huffman (make-leaf-set pairs))
(cdr (find-mix (make-leaf-set pairs)))

(define (decode bits tree)
  (define (decode-i bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-i (cdr bits) tree))
              (decode-i (cdr bits) next-branch)))))

  (decode-i bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit ---" bit))))


;; 2.67

(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree (make-leaf 'D 1)
                                   (make-leaf 'C 1)))))


(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

(decode sample-message sample-tree)


;; 2.68

(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(define (encode-symbol message tree pre)
  (cond ((null? tree) (cons false pre))
        ((leaf? tree)
         (if (eq? (symbol-leaf tree) message)
             (cons true pre)
             (cons false pre)))
        (else
         (let ((left-result (encode-symbol message (left-branch tree) (append pre '0)))
               (right-result (encode-symbol message (right-branch tree) (append pre (list '1)))))
           (cond ((car left-result) left-result)
                 ((car right-result) right-result))))))





(define (encode-symbol-list message tree)
  (encode-symbol message tree '()))

(encode-symbol-list 'c sample-tree)

(null? (car result))
(right-branch (right-branch (right-branch sample-tree)))
