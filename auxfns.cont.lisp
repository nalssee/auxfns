(defpackage :auxfns.cont
  (:use :cl :auxfns)
  (:export :=defun
	   :=lambda
	   :=bind
	   :=values
	   :=funcall
	   :=apply
	   :toplevel-k
	   :_cont_

	   :amb
	   :amb-bind
	   :init-paths
	   :backtrack
	   :bag-of
	   :amb-let*
	   :only-when))

(in-package :auxfns.cont)



;;===========================================================
;;; Continuation Passing Macro
;;===========================================================

;; Very problematic
;; (setf _cont_ #'values)

;; "http://www.cliki.net/on lisp"
(defvar *actual-cont* #'values)
(define-symbol-macro _cont_ *actual-cont*)


;; define toplevel continuation first
(defmacro toplevel-k (&optional (fn '(function values)))
  `(setf _cont_ ,fn))


(defmacro =lambda (parms &body body)
  `#'(lambda (_cont_ ,@parms) ,@body))

(defmacro =defun (name parms &body body)
  (let ((f (intern (concatenate 'string
				"=" (symbol-name name)))))
    `(progn
       (defmacro ,name ,parms
	 `(,',f _cont_ ,,@parms))
       (defun ,f (_cont_ ,@parms) ,@body))))

(defmacro =bind (parms expr &body body)
  `(let ((_cont_ #'(lambda ,parms ,@body))) ,expr))

(defmacro =values (&rest retvals)
  `(funcall _cont_ ,@retvals))

(defmacro =funcall (fn &rest args)
  `(funcall ,fn _cont_ ,@args))

(defmacro =apply (fn &rest args)
  `(apply ,fn _cont_ ,@args))


;; For nondeterministic search
(defparameter *paths* nil)
(defvar failsym '@)

(defun init-paths () (setf *paths* nil))

;; choose
(defmacro amb (&rest choices)
  (if choices
      `(progn
	 ,@(mapcar #'(lambda (c)
		       `(push #'(lambda () ,c) *paths*))
		   (reverse (cdr choices)))
	 ,(car choices))
      '(backtrack)))



;; choose-bind
(defmacro amb-bind (var choices &body body)
  `(ab #'(lambda (,var) ,@body) ,choices))

;; cb
(defun ab (fn choices)
  (if choices
      (progn
	(if (cdr choices)
	    (push #'(lambda () (ab fn (cdr choices)))
		  *paths*))
	(funcall fn (car choices)))
      (backtrack)))

(defmacro amb-let* (binds &body body)
  (if (null (cdr binds))
      `(amb-bind ,@(first binds)
	   ,@body)
      `(amb-bind ,@(first binds)
	   (amb-let* ,(cdr binds) ,@body))))


(defmacro only-when (&body exps)
  (if (null (cddr exps))
      `(if ,(first exps)
	   ,(second exps)
	   (amb))
      `(if ,(first exps)
	   (only-when ,@(cdr exps))
	   (amb))))



;; fail
(defun backtrack ()
  (if *paths*
      (funcall (pop *paths*))
      failsym))


(defmacro bag-of (expr)
  (with-gensyms (result x)
    `(let ((,result '()))
       (=bind (&rest ,x) ,expr
	 (push (if (null (cdr ,x)) (car ,x) ,x)
	       ,result)
	 (backtrack))
       (=values (nreverse ,result)))))



