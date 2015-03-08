(defpackage :def_test
  (:use :cl
	:auxfns.def
	:clunit)
  (:import-from :auxfns :curry))

(in-package :def_test)

(def
  e 2.718
  
  (fib1 !! fixnum) fixnum
  (fib1 0) 0
  (fib1 1) 1
  (fib1 n) (+ (fib1 (1- n)) (fib1 (- n 2)))

  ;; This is faster
  (fib2 !! fixnum) fixnum
  (fib2 n) (cond ((= n 0) 0)
  		 ((= n 1) 1)
  		 (t (+ (fib2 (- n 1)) (fib2 (- n 2)))))

  ;; type specification doesnt work so well for multiple clause defs
  (quicksort nil) nil
  (quicksort (cons x xs)) (append (quicksort (remove-if (curry #'<= x) xs))
  				  (list x)
  				  (quicksort (remove-if (curry #'> x) xs)))

  (my-append x nil) x
  (my-append nil y) y
  (my-append (cons x y) z) (cons x (my-append y z))

  (sqrt1 x) (with-local-definitions
		((good-enough? guess) (< (abs (- (* guess guess) x)) 0.001)
		 (improve guess) (average guess (/ x guess))
		 (average a b) (/ (+ a b) 2.0)
		 (sqrt-iter guess) (cond ((good-enough? guess) guess)
					 (t (sqrt-iter (improve guess)))))
	      (sqrt-iter 1.0))
  
  (capital-city 'Korea) 'Seoul
  (capital-city 'China) 'Beijing
  (capital-city 'Japan) 'Tokyo
  (capital-city _) 'no-idea

  (my-last (list x)) x
  (my-last (cons _  xs)) (my-last xs))

(defsuite simple-test-suite ())
(deftest test1 (simple-test-suite)
  (assert-true (= (fib1 10) (fib2 10) 55))
  (assert-true (equal (quicksort '(-3 4 10 0 1))
		      '(-3 0 1 4 10)))
  (assert-true (= (sqrt1 10) 3.1622777))  ; depends on systems
  (assert-true (equal (capital-city 'Brasil) 'no-idea)))

(print (run-suite 'simple-test-suite))


