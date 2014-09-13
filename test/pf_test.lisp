(defpackage :pf_test
  (:use :cl :auxfns.pf))

(in-package :pf_test)

(defun fib (n)
  (if (< n 2)
      n
      (+ (fib (- n 1))
	 (fib (- n 2)))))

(defun foo (n)
  (rem (fib n) 20))



