(defpackage :auxfns.def
  (:use :cl :auxfns :optima)
  (:export :def))

(in-package :auxfns.def)

;; 05/28/2014
;; Syntactic sugar for pattern matching based definition
;; See the below of this file for example

(defmacro def (&body sexps)
  `(progn ,@(mapcar
	     ;; form defparameter or defun
	     #'definition
	     (group-by-definition-name (group sexps :n 2)))))


(defun group-by-definition-name (clauses)
  (group clauses
	 :key #'definition-name))

(defun definition-name (clause)
  (if (atom (car clause))
      (car clause)
      (caar clause)))

(defun definition (clauses)
  (if (and (null (cdr clauses)) (atom (caar clauses)))
      (cons 'defparameter (var-value clauses))
      (cons 'defun (var-args-value clauses))))


(defun var-value (clauses)
  `(,(caar clauses) ,(cadar clauses)))

(defun var-args-value (clauses)
  (let* ((cl1 (car clauses)))
    (if (null (cdr clauses))
	(if (not (every #'variable-p (pattern cl1)))
	    (error "Single clause definition must not have any constant in a pattern ~A"
		   cl1)
	    `(,(caar cl1) ,(cdar cl1)
	       ,(let-labels-body (second cl1))))
	(let ((params (params cl1)))
	  `(,(caar cl1) ,params
	     ,(function-body-for-multiple-clauses clauses params))))))

(defun function-body-for-multiple-clauses (clauses params)
  `(match (list ,@params)
     ,@(mapcar #'(lambda (c)
		   (list (cons 'list (pattern c))
			 (let-labels-body (second c))))
	       clauses)))

(defun params (clause)
  (let ((result '()))
    (dotimes (i (length (pattern clause)))
      (push (gensym) result))
    result))

(defun variable-p (x)
  (and (symbolp x) (not (eq x nil)) (not (eq x t))))

(defun pattern (clause) (cdar clause))

(defun let-labels-body (clause-value)
  (if (contains-where-p clause-value)
      (let* ((internal-clauses (group (cddr clause-value) :n 2))
	     ;; let binds come before labels binds automatically.
	     (let-binds (remove-if #'function-clause-p internal-clauses))
	     (labels-binds (remove-if-not #'function-clause-p internal-clauses)))
	(if (null let-binds)
	    `(labels ,(mapcar #'var-args-value (group-by-definition-name labels-binds))
	       ,(car clause-value))
	    `(let ,let-binds
	       (labels ,(mapcar #'var-args-value (group-by-definition-name labels-binds))
		 ,(car clause-value)))))
      clause-value))

(defun contains-where-p (clause-value)
  (and (consp clause-value)
       (consp (first clause-value))
       (find (intern "WHERE") clause-value)))

(defun function-clause-p (clause)
  (consp (car clause)))
