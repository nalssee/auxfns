(defpackage :def_test
  (:use :cl :auxfns.def))

(in-package :def_test)

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


