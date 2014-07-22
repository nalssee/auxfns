(defpackage :auxfns
  (:use :cl :optima)
  (:export :with-gensyms
	   :once-only
	   :nlet

	   :range
	   ;; :\#\[
	   
	   :lcomp
	   ;; :\[

	   :alambda
	   :rec

	   :mappend

	   ;; Lazy list
	   :delay
	   :force
	   :make-pipe
	   :empty-pipe
	   :head
	   :tail
	   :pipe-elt
	   ;; :integers
	   ;; :enumerate
	   :filter-pipe
	   :map-pipe

	   ;; :deflex


	   :memoize

	   :minf
	   :maxf

	   :for*/list

	   :aif
	   :aif2
	   :acond
	   :acond2
	   :it

	   :group
	   :file-string
	   :numbering
	   :split-list
	   :str2file

	   :def

	   
	   :deflexical
	   :defc

	   :=defun
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
	   :only-when
	   
	   ))



(in-package :auxfns)

(defmacro alambda (parms &body body)
  `(labels ((rec ,parms ,@body))
     #'rec))

(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop for n in names collect `(,n (gensym)))
     ,@body))

;; (defmacro once-only ((&rest names) &body body)
;;   (let ((gensyms (loop for n in names collect (gensym (string n)))))
;;     `(list 'let
;;            (list ,@(loop for g in gensyms for n in names collect `(list ',g ,n)))
;;            (let ,(loop for n in names for g in gensyms collect `(,n ',g))
;;              ,@body))))

(defmacro once-only ((&rest names) &body body)
  (let ((gensyms (loop repeat (length names) collect (gensym))))
    `(let (,@(loop for g in gensyms collect `(,g (gensym))))
      `(let (,,@(loop for g in gensyms for n in names collect ``(,,g ,,n)))
        ,(let (,@(loop for n in names for g in gensyms collect `(,n ,g)))
           ,@body)))))


(defmacro nlet (name pairs &body body)
  `(labels ((,name ,(mapcar #'first pairs) ,@body))
     (,name .,(mapcar #'second pairs))))

(defun mappend (fn &rest lsts)
  (apply #'append (apply #'mapcar fn lsts)))


;; Replacing it with range
;; (set-macro-character #\] (get-macro-character #\)))

;; (set-dispatch-macro-character
;;  #\# #\[
;;  #'(lambda (s c1 c2)
;;      (declare (ignore c1 c2))
;;      (let ((args (read-delimited-list #\] s t)))
;;        (let ((i (gensym)))
;; 	 `(loop for ,i from ,(first args) to ,(second args)
;; 	     by (or ,(third args) 1) collect ,i)))))



(defmacro abbrev (short long)
  `(defmacro ,short (&rest args)
     `(,',long ,@args)))

;; (defmacro abbrev (short long)
;;   (list 'defmacro short (list '&rest 'args)
;; 	(list 'cons (list 'quote long) 'args)))
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

(defun tail (pipe)
  (if (functionp (rest pipe))
      (setf (rest pipe) (funcall (rest pipe)))
      (rest pipe)))

(defun pipe-elt (pipe i)
  (if (= i 0)
      (head pipe)
      (pipe-elt (tail pipe) (1- i))))

;; (defun integers (&optional (start 0) end)
;;   (if (or (null end) (<= start end))
;;       (make-pipe start (integers (1+ start) end))
;;       nil))


;; (defun enumerate (pipe &key count key (result pipe))
;;   (if (or (eq pipe empty-pipe) (eql count 0))
;;       result
;;       (progn
;;         (unless (null key) (funcall key (head pipe)))
;;         (enumerate (tail pipe) :count (if count (1- count))
;;                    :key key :result result))))


(defun filter-pipe (pred pipe)
  (if (funcall pred (head pipe))
      (make-pipe (head pipe)
                 (filter-pipe pred (tail pipe)))
      (filter-pipe pred (tail pipe))))

;; (defun sieve (pipe)
;;   (make-pipe (head pipe)
;;              (filter #'(lambda (x) (/= (mod x (head pipe)) 0))
;;                      (sieve (tail pipe)))))

;; (defvar *primes* (sieve (integers 2)))

(defun map-pipe (fn pipe)
  (if (eq pipe empty-pipe)
      empty-pipe
      (make-pipe (funcall fn (head pipe))
                 (map-pipe fn (tail pipe)))))

(defun append-pipes (x y)
  (if (eq x empty-pipe)
      y
      (make-pipe (head x)
                 (append-pipes (tail x) y))))

(defun mappend-pipe (fn pipe)
  (if (eq pipe empty-pipe)
      empty-pipe
      (let ((x (funcall fn (head pipe))))
        (make-pipe (head x)
                   (append-pipes (tail x)
                                 (mappend-pipe
                                  fn (tail pipe)))))))


;;=========================================================





(defun memoize (fn)
  (let ((cache (make-hash-table :test #'equal)))
    #'(lambda (&rest args)
	(multiple-value-bind (val win) (gethash args cache)
	  (if win
	      val
	      (setf (gethash args cache)
		    (apply fn args)))))))



(defun minf (fn xs)
  (labels ((aux (x v xs)
	     (if (null xs)
		 (values x v)
		 (let ((nv (funcall fn (car xs))))
		   (if (< nv v)
		       (aux (Car xs) nv (cdr xs))
		       (aux x v (cdr xs)))))))
    (aux (car xs) (funcall fn (car xs)) (cdr xs))))

(defun maxf (fn xs)
  (labels ((aux (x v xs)
	     (if (null xs)
		 (values x v)
		 (let ((nv (funcall fn (car xs))))
		   (if (> nv v)
		       (aux (car xs) nv (cdr xs))
		       (aux x v (cdr xs)))))))
    (aux (car xs) (funcall fn (car xs)) (cdr xs))))


;;;
(defmacro for*/list (binds &body body)
  (labels ((expand (binds body result)
             (if (null (cdr binds))
                 `(loop for ,(caar binds) in ,(cadar binds) do
                       (push ,(car body) ,result))
                 `(loop for ,(caar binds) in ,(cadar binds) do
                       ,(expand (cdr binds) body result)))))
    (let ((result (gensym)))
      `(let (,result)
         ,(expand binds body result)
         (nreverse ,result)))))



(defmacro aif (test-form then-form &optional else-form)
  `(let ((it ,test-form))
     (if it ,then-form ,else-form)))

(defmacro aif2 (test &optional then else)
  (let ((win (gensym)))
    `(multiple-value-bind (it ,win) ,test
       (if (or it ,win) ,then ,else))))

;;; On lisp page 191, chapter 14.
;;; Anaphoric macro: acond
(defmacro acond (&rest clauses)
  (if (null clauses)
      nil
      (let ((cl1 (car clauses))
	    (sym (gensym)))
	`(let ((,sym ,(car cl1)))
	   (if ,sym
	       (let ((it ,sym)) ,@(cdr cl1))
	       (acond ,@(cdr clauses)))))))


(defmacro acond2 (&rest clauses)
  (if (null clauses)
      nil
      (let ((cl1 (car clauses))
            (val (gensym))
            (win (gensym)))
        `(multiple-value-bind (,val ,win) ,(car cl1)
           (if (or ,val ,win)
               (let ((it ,val)) ,@(cdr cl1))
               (acond2 ,@(cdr clauses)))))))





;; 
;; (group '(1 2 3 4 5) :n 2)
(defun group (xs &key (test #'eql) (key #'identity)
		   (n nil))
  (let* ((len-1 (1- (length xs)))
	 (cnt len-1)
	 (vresult (make-array (1+ cnt) :initial-element nil)))
    (and
     xs
     (if n
	 (labels ((iter (result remaining)
		    (if (<= (length remaining) n)
			(reverse (cons remaining result))
			(iter (cons (subseq remaining 0 n) result)
			      (nthcdr n remaining)))))
	   (iter '() xs))
	 (progn
	   (let ((rxs (reverse xs)))
	     (push (car rxs) (svref vresult cnt))
	     (loop for x in (cdr rxs) do
		  (if (funcall test
			       (funcall key x)
			       (funcall key (car (svref vresult cnt))))
		      (push x (svref vresult cnt))
		      (progn (decf cnt)
			     (push x (svref vresult cnt))))))
	   (let ((result '()))
	     (loop for i from len-1 downto cnt do
		  (push (svref vresult i) result))
	     result))))))


;; (defun file-string (path)
;;   (with-open-file (stream path)
;;     (let ((data (make-string (file-length stream))))
;;       (read-sequence data stream)
;;       data)))


;; whole file to a string
(defun file-string (path)
  (with-open-file (stream path)
    (let ((data (make-array (file-length stream)
			    :element-type 'character :fill-pointer t)))
      (setf (fill-pointer data)
	    (read-sequence data stream))
      data)))

(defun numbering (xs &key (key #'identity) (start 1) (test #'eql))
  (let ((count-list '()))
    (values
     (loop for x in xs collect
	  (let ((p (assoc (funcall key x) count-list :key key :test test)))
	    (cond (p (list x (incf (second p))))
		  (t (push (list x start) count-list)
		     (list x start)))))
     (nreverse count-list))))


(defun str2file (x file)
  (with-open-file (s file
		     :direction :output
		     :if-exists :supersede)
    (format s "~A" x)))


(defun range (start end &optional (step 1))
  (if (> end start)
      (loop for i from start below end by step
	 collect i)
      (loop for i from start above end by step
	 collect i)))



(defun split-list (a xs &key (test #'eql))
  (let ((p (position a xs :test test)))
    (if p
	(list (subseq xs 0 p)
	      (nthcdr p xs))
	(error "Nothing matches ~A" a))))



;; =======================================
;; def
;; =======================================

;; 05/28/2014
;; Syntactic sugar for pattern matching based definition
;; See the below of this file for example

(defmacro def (&body sexps)
  `(progn ,@(mapcar
	     ;; form defparameter or defun
	     #'definition
	     (same-feathered (group sexps :n 2)))))

;; bind clauses with the same name    
(defun same-feathered (clauses)
  (group clauses
	 :test
	 (lambda (a b)
	   (when (and (not (atom (car a)))
		      (not (atom (car b))))
	     (eql (caar a) (caar b))))))

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
	    `(labels ,(mapcar #'var-args-value (same-feathered labels-binds))
	       ,(car clause-value))
	    `(let ,let-binds
	       (labels ,(mapcar #'var-args-value (same-feathered labels-binds))
		 ,(car clause-value)))))
      clause-value))
      
(defun contains-where-p (clause-value)
  (and (consp clause-value)
       (consp (first clause-value))
       (find (intern "WHERE") clause-value)))

(defun function-clause-p (clause)
  (consp (car clause)))
;; ============================================
;; End of def
;; ============================================





;; ============================================
;; List comprehension
;; ============================================
;; combinations of three natural numbers that sum to 20.
;; [(list a b c) a <- #[1 10] b <- #[1 10] c <- #[1 10] (= (+ a b c) 20) (>= a b) (>= b c)]


;; (set-macro-character
;;  #\[
;;  #'(lambda (stream char)
;;      (declare (ignore char))
;;      (let ((expr (read-delimited-list #\] stream t)))
;;        (apply #'comprehend (rbp expr)))))


;; (lcomp ((x (range 1 11))
;; 		(y (range 1 11))
;; 		(z (range 1 11)))
;; 	  (= (+ (* x x) (* y y)) (* z z))
;; 	  (>= y x)
;; 	  (list x y z))
(defmacro lcomp (binds &body body)
  (comprehend (first (last body))
	      binds
	      (butlast body)))


;; might not be very efficient
(defun comprehend (result binds preds)
  (if (null binds)
      `(and ,@preds (list ,result))
      (let ((bind (car binds)))	
	`(mapcan #'(lambda (,(car bind))
		     ,(comprehend result (cdr binds) preds))
		 ,(second bind)))))

;; expr -> (result binds preds)
(defun rbp (expr)
  (labels ((rec (expr binds preds)
	     (cond ((null expr) (list (nreverse binds) (nreverse preds)))
		   ((eql '<- (second expr))
		    (rec (cdddr expr)
			 (cons (list (first expr) (third expr)) binds)
			 preds))
		   (t (rec (cdr expr) binds (cons (car expr) preds))))))
    (cons (car expr) (rec (cdr expr) '() '()))))











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

;; ==============================================
;; End of Global Variables Redefined
;; ==============================================







;;===========================================================
;;; Continuation Passing Macro
;;===========================================================

;; Very problematic
;; (setf _cont_ #'identity)
(deflexical _cont_ #'identity)

;; define toplevel continuation first
(defmacro toplevel-k (&optional (fn '(function identity)))
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




