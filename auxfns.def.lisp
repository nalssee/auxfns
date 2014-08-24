(defpackage :auxfns.def
  (:use :cl :auxfns :optima)
  (:export :def))

(in-package :auxfns.def)


;; =======================================
;; def
;; =======================================


(defmacro def (&body sexps)
  `(progn ,@(mapcar
	     ;; form defparameter or defun
	     #'definition
	     (group-by-definition-name (group sexps :n 2)))))


(defun group-by-definition-name (clauses)
  (group clauses
	 :key #'clause-name))


(defun definition (clauses)
  "Form defition from clauses"
  (cond ((and (null (cdr clauses)) (atom (caar clauses)))
	 (cons 'defparameter (var-value clauses)))
	(t (cons 'defun (var-parms-value clauses)))))

(defun arg-types (type-clause)
  (cddr (clause-head type-clause)))

(defun return-type (type-clause)
  (second type-clause))

(defun clause-head (clause) (first clause))
(defun clause-body (clause) (second clause))
(defun clause-name (clause)
  (if (atom (car clause))
      (car clause)
      (caar clause)))
;; function parameters if any
(defun clause-pattern (clause) (cdar clause))



(defun type-specification? (clause)
  "Tests if the given clause includes type specification"
  (and (not (null (rest (first clause))))
       (eql (second (first clause)) (intern "!!"))))


(defun var-value (clauses)
  `(,(caar clauses) ,(cadar clauses)))



(defun var-parms-value (clauses)
  (let ((cl1 (car clauses))
	(cls (cdr clauses)))
    (if (type-specification? cl1)
	(let ((arg-types (arg-types cl1))
	      (return-type (return-type cl1))
	      (pattern (clause-pattern (car cls))))
	  ;; type spec included
	  (if (null (cdr cls)) ;single clause
	     `(,(clause-name (car cls)) ,(clause-pattern (car cls))
		(declare (optimize (speed 3) (safety 0)))
		(declare ,@(mapcar #'list arg-types pattern))
		(the ,return-type ,(let-labels-body (clause-body (car cls)))))
	     (let ((params (params (car cls))))
	       `(,(caar (car cls)) ,params
		  (declare (optimize (speed 3) (safety 0)))
		  (declare ,@(mapcar #'list arg-types params))
		  (the ,return-type
		       ,(match-params-type-specification
			 arg-types
			 (function-body-for-multiple-clauses cls params)))))))
	;; no type spec
	(if (null cls)
	    `(,(clause-name cl1) ,(clause-pattern cl1)
	       ,(let-labels-body (clause-body cl1)))
	    (let ((params (params cl1)))
	      `(,(clause-name cl1) ,params
		 ,(function-body-for-multiple-clauses clauses params)))))))



(defun match-params-type-specification (arg-types match-form)
  "As for multiple clause definitions parameters for the definitions are generated
   So the parameters for each match clauses are not type specified"
  (flet ((parameter? (x) (and (symbolp x)
			      (not (eql x nil))
			      (not (eql x t)))))
    (list* (first match-form) (second match-form)
	   (loop for c in (cddr match-form) collect
		(let ((pairs
		       ;; ex) pairs:: ((fixnum x) (list y)) 
		       (if (null (cdr arg-types)) ; a single parmeter case
				 (when (parameter? (first c))
				   (list (list (first arg-types) (first c))))
				 (mapcan (lambda (a p)
					   (when (parameter? p)
					     (list (list a p))))
					 arg-types
					 ;; remove 'list' tag
					 (cdr (first c))))))
		  (if (null pairs)
		      c
		      (list (first c)
			    `(declare ,@pairs)
			    (second c))))))))





(defun function-body-for-multiple-clauses (clauses params)
  ;; when there's only one parameter
  ;; Might not be a great lift for performance
  ;; but easier to read when macroexpanded at list
  
  (if (null (cdr params))
      `(match ,(car params)
	      ,@(mapcar #'(lambda (c)
			    (list (car (clause-pattern c))
				  (let-labels-body (second c))))
			clauses))
      `(match (list ,@params)
	      ,@(mapcar #'(lambda (c)
			    (list (cons 'list (clause-pattern c))
				  (let-labels-body (second c))))
			clauses))))


(defun params (clause)
  "Generate symbols as many as function parameters"
  (let ((result '()))
    (dotimes (i (length (clause-pattern clause)))
      (push (gensym) result))
    result))

(defun variable-p (x)
  (and (symbolp x) (not (eq x nil)) (not (eq x t))))

(defun let-labels-body (clause-value)
  (if (contains-where-p clause-value)
      (let* ((internal-clauses (group (cddr clause-value) :n 2))
	     ;; let binds come before labels binds automatically.
	     (let-binds (remove-if #'function-clause-p internal-clauses))
	     (labels-binds (remove-if-not #'function-clause-p internal-clauses)))
	;; Have faith in CL compilers. They handle when let-binds or labels-binds are null
	`(let* ,let-binds
	   (labels ,(mapcar #'var-parms-value (group-by-definition-name labels-binds))
	     ,(car clause-value))))
      clause-value))
      
(defun contains-where-p (clause-value)
  (and (consp clause-value)
       (consp (first clause-value))
       (find (intern "WHERE") clause-value)))

(defun function-clause-p (clause)
  (consp (car clause)))



