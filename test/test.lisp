;; (eval-when (:compile-toplevel :load-toplevel :execute)
;;   (ql:quickload :auxfns))

(defpackage :test
  (:use :cl :auxfns))

(in-package :test)





;; (memoize 'fib1)
;; compare (fib1 40) and (fib2 40)

(multiple-value-bind (a b) (numbering '(a a b a c c b b a))
  (print a)
  (print b))


