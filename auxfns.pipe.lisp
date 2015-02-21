;; Lazy list
(defpackage :auxfns.pipe
  (:use :cl)
  (:export :delay
	   :force
	   :make-pipe
	   :empty-pipe
	   :empty-pipe-p
	   :head
	   :tail
	   :pipe-elt
	   ;; :integers
	   ;; :enumerate
	   :pipe-append
	   :pipe-filter
	   :pipe-map
	   ))

(in-package :auxfns.pipe)

;;====================================================
;; For Laziness
;;====================================================
(defstruct delay (forced nil) (closure nil))

(defmacro delay (expr)
  `(make-delay :closure #'(lambda () ,expr)))

(defun force (x)
  (if (not (delay-p x))
      x
      (progn (when (delay-closure x)
	       (setf (delay-forced x)
		     (funcall (delay-closure x)))
	       (setf (delay-closure x) nil))
	     (delay-forced x))))

(defmacro make-pipe (head tail)
  `(cons ,head #'(lambda () ,tail)))

(defvar empty-pipe nil)
(defun head (pipe) (first pipe))

(defun empty-pipe-p (x)
  (eq x empty-pipe))

(defun tail (pipe)
  (if (functionp (rest pipe))
      (setf (rest pipe) (funcall (rest pipe)))
      (rest pipe)))

(defun pipe-elt (pipe i)
  (cond ((empty-pipe-p pipe) nil)
	((= i 0) (head pipe))
	(t (pipe-elt (tail pipe) (1- i)))))

(defun pipe-filter (pred pipe)
  (if (funcall pred (head pipe))
      (make-pipe (head pipe)
                 (pipe-filter pred (tail pipe)))
      (pipe-filter pred (tail pipe))))

(defun pipe-map (fn &rest pipes)
  (if (empty-pipe-p (first pipes))
      empty-pipe
      (make-pipe
       (apply fn (mapcar #'head pipes))
       (apply #'pipe-map
	      (cons fn (mapcar #'tail pipes))))))

(defun pipe-append (&rest xs)
  (cond ((null xs) empty-pipe)
	((empty-pipe-p (car xs))
	 (apply #'pipe-append (cdr xs)))
	(t (make-pipe (head (car xs))
		      (apply #'pipe-append
			     (tail (car xs))
			     (cdr xs))))))

