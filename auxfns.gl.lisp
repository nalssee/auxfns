;; global lexical variables

(defpackage :auxfns.gl
  (:use :cl)
  (:export :deflexical
	   :defc))

(in-package :auxfns.gl)


;; http://blog.rongarret.info/2009/08/global-variables-done-right.html
;; global variables redefined
;; deflexical and defc are useful
(defun get-dynamic-cell (symbol)
  (or (get symbol 'dynamic-cell)
      (setf (get symbol 'dynamic-cell)
	    (copy-symbol symbol))))


(defun dynamic-value (symbol)
  (symbol-value symbol))


(defmacro defv (var val)
  "Defines VAR to be a global dynamic variable with initial value VAL"
  `(progn
     (setf (symbol-value ',(get-dynamic-cell var))
	   ,val)
     (define-symbol-macro ,var (dynamic-value ',(get-dynamic-cell var)))))


(defmacro dval (var)
  "Returns the current dynamic binding of VAR, even if there is a lexical binding in scope"
  `(symbol-value ',(get-dynamic-cell var)))

(defmacro dlet (bindings &body body)
  "Unconditionally create new dynamic bindings"
  (if (atom bindings)
      (setf bindings `((,bindings ,(pop body)))))
  (let* ((vars (mapcar 'first bindings))
	 (dvars (mapcar 'get-dynamic-cell vars))
	 (vals (mapcar 'second bindings)))

    (dolist (v vars)
      (let ((e (macroexpand v)))
	(if (or (atom e)
		(not (eq (car e)
			 'dynamic-value)))
	    (error "~A is not a dynamic variable" v))
	(if (eq (car e)
		'non-settable-value)
	    (error "~A is immutable" v))))
    `(let ,(mapcar 'list dvars vals)
       (declare (special ,@dvars))
       ,@body)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun get-lexical-cell (sym)
    (or (get sym 'lexical-cell)
	(setf (get sym 'lexical-cell)
	      (copy-symbol sym)))))


(defun non-settable-value (s)
  (symbol-value s))

(defun (setf non-settable-value)
    (val var)
  (declare (ignore val))
  (error "~A is immutable" var))


(defmacro defc (var val &optional force-rebind)
  "Immutably binds VAR to VAL.  If FORCE-REBIND is T then VAR is forcibly rebound."
  (let ((cell (get-lexical-cell var)))
    `(progn
       ,(if force-rebind
	    `(setf (symbol-value ',cell)
		   ,val)
	    `(unless (boundp ',cell)
	       (setf (symbol-value ',cell)
		     ,val)))
       (define-symbol-macro ,var (non-settable-value ',cell)))))


(defmacro deflexical (var val)
  "Defines VAR to be a global lexical variable"
  (let ((cell (get-lexical-cell var)))
    `(progn
       (setf (symbol-value ',cell)
	     ,val)
       (define-symbol-macro ,var (symbol-value ',cell)))))


(defmacro lval (var)
  "Unconditionally returns the global lexical binding of VAR"
  `(symbol-value ',(get-lexical-cell var)))


