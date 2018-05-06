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
