(defpackage :auxfns.def
  (:use :cl :auxfns :optima)
  (:export :def))

(in-package :auxfns.def)

(defmacro def (&body exps)
  `(progn ,@(mapcar
	     ;; form defparameter or defun
	     #'form-definition
	     (group (group exps :n 2)
		    :key #'clause-name))))

;; Data abstractions
(defun clause-head (clause) (first clause))
(defun clause-body (clause) (second clause))
(defun clause-name (clause)
  (if (atom (clause-head clause)) ; parameter def
      (clause-head clause)
      (first (clause-head clause))))

(defun type-spec-clause-p (clause)
  "Tests if the given clause specifies types"
  (and (listp (clause-head clause))
       (<= 2 (length (clause-head clause)))
       (eql (second (clause-head clause)) (intern "!!"))))

(defun clause-parameters (clause)
  (rest (clause-head clause)))

(defun clause-parameter-types (type-spec-clause)
  (cddr (clause-head type-spec-clause)))

(defun clause-return-type (type-spec-clause)
  (clause-body type-spec-clause))

;; make a list of gensyms for paramters
(defun make-parameters (clause)
  (loop repeat (1- (length (clause-head clause))) collect (gensym)))

(defun variable-pattern-p (x)
  (and (atom x) (symbolp x)
       (not (eql x t)) (not (eql x nil))))

(defun variable-pattern-only-p (parameters)
  (loop for p in parameters always (variable-pattern-p p)))



;;;
(defvar *def-dispatch-table* (make-hash-table))

(defun register-def (name fn)
  (setf (gethash name *def-dispatch-table*) fn))

(defun form-definition (clauses)
  (if (and (null (rest clauses)) (atom (clause-head (first clauses))))
      (let ((clause (first clauses)))
	`(defparameter ,(clause-name clause) ,(clause-body clause)))
      `(defun ,@(funcall (gethash (analyze-clauses clauses) *def-dispatch-table*)
			 clauses))))

;; 1 clause, no type spec, no pattern variables
(register-def
 :simple-form
 #'(lambda (clauses)
     (let ((cl1 (first clauses)))
       `(,(clause-name cl1) ,(clause-parameters cl1)
	  ,(handle-local-definitions (clause-body cl1))))))

;; requires match form (multiple clauses or pattern variable)
;; no type spec 
(register-def
 :match-form
 #'(lambda (clauses)
     (let* ((cl1 (first clauses))
	    (params (make-parameters cl1)))
       `(,(clause-name cl1) ,params
	  (match (list ,@params)
	    ,@(loop for (pattern value) in clauses collect
		   (list `(list ,@(rest pattern)) (handle-local-definitions value))))))))

;; 1 body clause, type spec, no pattern variables
(register-def
 :simple-form-type
 #'(lambda (clauses)
     (destructuring-bind (tsc cl1) clauses
       `(,(clause-name cl1) ,(clause-parameters cl1)
	  ,(form-declaration tsc (clause-parameters cl1))
	  (the ,(clause-return-type tsc) ,(handle-local-definitions
				    (clause-body cl1)))))))


;; Hardest part
;; requires match form with type spec
(register-def
 :match-form-type
 #'(lambda (clauses)
     (destructuring-bind (tsc cl1 &rest cls) clauses
       (declare (ignore cls))
       (let ((params (make-parameters cl1)))
	 `(,(clause-name cl1) ,params
	    ,(form-declaration tsc params)
	    (match (list ,@params)
	      ,@(loop for (pattern value) in (rest clauses) collect
		     `((list ,@(rest pattern))
		       ,@(maybe-form-declaration tsc (rest pattern))
		       (the ,(clause-return-type tsc) ,(handle-local-definitions
						 value))))))))))


(defun handle-local-definitions (exp)
  (if (and (listp exp) (eql (first exp) (intern "WITH-LOCAL-DEFINITIONS")))
      (let* ((clauses (group (second exp) :n 2))
	     ;; todo:
	     ;; Maybe I should restrict let bindings come before labels bindings
	     (let-clauses (remove-if
			   #'(lambda (c) (listp (clause-head c))) clauses))
	     (labels-clauses (remove-if-not
			      #'(lambda (c) (listp (clause-head c))) clauses))
	     (labels-form
	      `(labels
		   ,(loop for cls in (group labels-clauses :key #'clause-name) collect
			 (funcall (gethash (analyze-clauses cls) *def-dispatch-table*) cls))
		 ,@(cddr exp))))
	;; At least one labels-bindings exists	
	;; 	(assert (not (null labels-clauses)))
	(if (null let-clauses)
	    labels-form
	    `(let ,let-clauses
	       ,labels-form)))
      exp))


(defun analyze-clauses (clauses)
  (destructuring-bind (cl1 &rest cls) clauses
    (cond ((and (null cls)
		(variable-pattern-only-p (clause-parameters cl1)))
	   :simple-form)
	  ;; todo, error case
	  ;; spec clause only definition may need to be handled as well
	  ((type-spec-clause-p cl1)
	   (if (and (null (rest cls))
		    (variable-pattern-only-p (clause-parameters (first cls))))
	       :simple-form-type
	       :match-form-type))
	  (t :match-form))))

(defun form-declaration (type-spec-clause parameters)
  `(declare (optimize (speed 3) (safety 0))
	    ,@(loop for type in (clause-parameter-types type-spec-clause)
		   for param in parameters collect
		   `(type ,type ,param))))

(defun maybe-form-declaration (type-spec-clause parameters)
  (let ((types (loop for type in (clause-parameter-types type-spec-clause)
		  for param in parameters when (variable-pattern-p param)
		  collect `(type ,type ,param))))
    (when types
      `((declare ,@types)))))



