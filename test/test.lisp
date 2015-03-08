(defpackage :test
  (:use :cl :auxfns))

(in-package :test)

(multiple-value-bind (a b) (numbering '(a a b a c c b b a))
  (print a)
  (print b))

