(defpackage :pipe_test
  (:use :cl :auxfns.pipe))

(in-package :pipe_test)


(defparameter ones (make-pipe 1 ones))

(defparameter integers (make-pipe 0 (pipe-map #'+ ones integers)))
(defparameter fibs
  (make-pipe 0
	     (make-pipe 1
			(pipe-map #'+ (tail fibs)
				  fibs))))

(defun integers-starting-from (n)
  (make-pipe n (integers-starting-from (1+ n))))


