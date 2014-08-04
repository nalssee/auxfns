(defpackage :cont_test
  (:use :cl :auxfns.cont))

(in-package :cont_test)
;; continuation passing macro
;; (toplevel-k)

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

(print (bag-of (two-numbers)))
(print (bag-of (parlor-trick 5)))


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

;; (toplevel-k #'print)
(print (multiple-dwelling))
(print (bag-of (prime-sum-pair '(1 3 5 8) '(20 35 110))))
(print (bag-of (prime-sum-pair '(19 27 30) '(11 36 58))))



