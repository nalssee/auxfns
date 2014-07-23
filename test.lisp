(defpackage :test
  (:use :cl :auxfns))

(in-package :test)



;; continuation passing macro
(toplevel-k)

(=defun add (x y)
  (=values (+ x y)))

(=defun prod (x y)
  (=values (* x y)))

(=bind (a) (add 1 2)
  (=bind (b) (prod a 3)
    (=values b)))

(=defun two-numbers ()
  (amb-bind n1 '(0 1 2 3 4 5)
    (amb-bind n2 '(0 1 2 3 4 5)
      (=values n1 n2))))

(=defun parlor-trick (sum)
  (=bind (n1 n2) (two-numbers)
    (if (= (+ n1 n2) sum)
	(=values (format nil "~A = ~A + ~A" sum n1 n2))
	(backtrack))))

(bag-of (two-numbers))
(bag-of (parlor-trick 5))


(defun smallest-divisor (n)
  (find-divisor n 2))

(defun find-divisor (n test-divisor)
  (cond ((> (* test-divisor test-divisor) n) n)
	((divides? test-divisor n) test-divisor)
	(t (find-divisor n (1+ test-divisor)))))

(defun divides? (a b)
  (= (rem b a) 0))
(defun prime? (n)
  (= n (smallest-divisor n)))


(init-paths)

(=defun an-element-of (xs)
  (amb-bind x xs
    (=values x)))

(=defun prime-sum-pair (list1 list2)
  (=bind (a) (an-element-of list1)
    (=bind (b) (an-element-of list2)
      (only-when
       (prime? (+ a b))
       (=values (list a b))))))


(defun distinct? (items)
  (cond ((null items) t)
	((null (cdr items)) t)
	((member (car items) (cdr items)) nil)
	(t (distinct? (cdr items)))))

(=defun multiple-dwelling ()
  (amb-let* ((baker '(1 2 3 4 5))
	    (cooper '(1 2 3 4 5))
	    (fletcher '(1 2 3 4 5))
	    (miller '(1 2 3 4 5))
	    (smith '(1 2 3 4 5)))
    (only-when
      (distinct? (list baker cooper fletcher miller smith))
      (not (= baker 5))
      (not (= cooper 1))
      (not (= fletcher 5))
      (not (= fletcher 1))
      (> miller cooper)
      (not (= (abs (- smith fletcher)) 1))
      (not (= (abs (- fletcher cooper)) 1))
      (=values `((baker ,baker)
		 (cooper ,cooper)
		 (fletcher ,fletcher)
		 (miller ,miller)
		 (smith ,smith))))))

(toplevel-k #'print)
(multiple-dwelling)
(bag-of (prime-sum-pair '(1 3 5 8) '(20 35 110)))
(bag-of (prime-sum-pair '(19 27 30) '(11 36 58)))

;;===================================
;; def
;;===================================


(def

  e 2.718
  (fact1 n) (cond ((= n 1) 1)
  		  (t (* n (fact1 (- n 1)))))


  (fib1 0) 0
  (fib1 1) 1
  (fib1 n) (+ (fib1 (- n 1)) (fib1 (- n 2)))

  (fib2 n) (cond ((= n 0) 0)
		 ((= n 1) 1)
		 (t (+ (fib2 (- n 1)) (fib2 (- n 2)))))
  
  (my-append x nil) x
  (my-append nil y) y
  (my-append (cons x y) z) (cons x (my-append y z))

  (sqrt1 x) ((sqrt-iter 1.0)
	     where
	     (good-enough? guess) (< (abs (- (* guess guess) x)) 0.001)
	     (improve guess) (average guess (/ x guess))
	     (average a b) (/ (+ a b) 2)
	     (sqrt-iter guess) (cond ((good-enough? guess) guess)
				     (t (sqrt-iter (improve guess)))))
  

  (capital-city 'Korea) 'Seoul
  (capital-city 'China) 'Beijing
  (capital-city 'Japan) 'Tokyo
  (capital-city _) 'no-idea

  (mylast (list x)) x
  (mylast (cons _  xs)) (mylast xs)

  )

(defparameter ones (make-pipe 1 ones))

(defparameter integers (make-pipe 0 (pipe-map #'+ ones integers)))
(defparameter fibs
  (make-pipe 0
	     (make-pipe 1
			(pipe-map #'+ (tail fibs)
				  fibs))))

(defun integers-starting-from (n)
  (make-pipe n (integers-starting-from (1+ n))))

(defparameter primes
  (make-pipe
   2
   (pipe-filter #'prime? (integers-starting-from 3))))

(memoize 'fib1)
;; compare (fib1 40) and (fib2 40)

(multiple-value-bind (a b) (numbering '(a a b a c c b b a))
  (print a)
  (print b))




