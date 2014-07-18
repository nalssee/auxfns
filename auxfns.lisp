(defpackage :auxfns
  (:use :cl :optima)
  (:export :with-gensyms
	   :once-only
	   :nlet
	   ;; :\#\[
	   
	   :lcomp
	   ;; :\[

	   :<-
	   :alambda
	   :rec

	   :mappend

	   :delay
	   :force
	   :make-pipe
	   :empty-pipe
	   :head
	   :tail
	   :pipe-elt
	   :integers
	   :enumerate
	   :filter
	   :map-pipe

	   ;; :deflex


	   :memoize

	   :minf
	   :maxf

	   :for*/list

	   :acond

	   :group
	   :file-string
	   :numbering
	   :split-list
	   :str2file
	   :range


	   
	   :def

	   :deflexical
	   :defc
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

(defconstant empty-pipe nil)
(defun head (pipe) (first pipe))

(defun tail (pipe)
  (if (functionp (rest pipe))
      (setf (rest pipe) (funcall (rest pipe)))
      (rest pipe)))

(defun pipe-elt (pipe i)
  (if (= i 0)
      (head pipe)
      (pipe-elt (tail pipe) (1- i))))

(defun integers (&optional (start 0) end)
  (if (or (null end) (<= start end))
      (make-pipe start (integers (1+ start) end))
      nil))


(defun enumerate (pipe &key count key (result pipe))
  (if (or (eq pipe empty-pipe) (eql count 0))
      result
      (progn
        (unless (null key) (funcall key (head pipe)))
        (enumerate (tail pipe) :count (if count (1- count))
                   :key key :result result))))

(defun filter (pred pipe)
  (if (funcall pred (head pipe))
      (make-pipe (head pipe)
                 (filter pred (tail pipe)))
      (filter pred (tail pipe))))

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



;; Defining lexical variable.
;; (defmacro deflex (var val)
;;   (let ((storage (gensym)))
;;     `(progn
;;        (setf (symbol-value ',storage) ,val)
;;        (define-symbol-macro ,var (symbol-value ',storage)))))


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

(defun take (xs n)
  (butlast xs (- (length xs) n)))



(defun group (xs &key (test #'equal) (n nil))
  (let* ((cnt (1- (length xs)))
	 (vresult (make-array (1+ cnt) :initial-element nil)))
    (and
     xs
     (if n
	 (labels ((iter (result remaining)
		    (if (<= (length remaining) n)
			(reverse (cons remaining result))
			(iter (cons (take remaining n) result)
			      (nthcdr n remaining)))))
	   (iter '() xs))
	 (progn
	   (let ((rxs (reverse xs)))
	     (push (car rxs) (svref vresult cnt))
	     (loop for x in (cdr rxs) do
		  (if (funcall test x (car (svref vresult cnt)))
		      (push x (svref vresult cnt))
		      (progn (decf cnt)
			     (push x (svref vresult cnt))))))
	   (let ((result '()))
	     (loop for i from (1- (length xs)) downto cnt do
		  (push (svref vresult i) result))
	     result))))))



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


;; (defun file-string (path)
;;   (with-open-file (stream path)
;;     (let ((data (make-string (file-length stream))))
;;       (read-sequence data stream)
;;       data)))

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



;; Global variables done right
;; If you've read my Idiot's Guide to Special Variables you will already know that I am not a big fan of the design of Common Lisp's global variables system. There are (at least)
;;  three problems with CL's design:

;; 1. There are no global lexicals.

;; 2. DEFCONSTANT doesn't actually define a constant, it defines a global variable with the less-than-useful property that the consequences of attempting to change its value are undefined. Thus, a conforming implementation of Common Lisp could expand DEFCONSTANT as DEFVAR. (I am, frankly, at a loss to understand why DEFCONSTANT was even included in the language. As far as I can tell there is nothing that you can do in portable CL with DEFCONSTANT that you could not do just as well with DEFINE-SYMBOL-MACRO.)


;; 3. There is no way to declare a global variable without also making the name of the variable pervasively special. In other words, once you've created a global variable named X it is no longer possible to write new code that creates lexical bindings for X. It is, of course, still possible for code evaluated before X was declared special to create lexical bindings for X. IMHO this is just insane. The only reason for this design is for backwards compatibility with code written for dynamically scoped dialects of Lisp. Well, guess what, folks. It's 2009. There are no more dynamically scoped dialects of Lisp. (Well, there's eLisp, but it lives safely sequestered in its own world and can be safely ignored by rational people.)
;;  And if you're still unconvinced that Common Lisp's pervasive special declarations are a bad design, consider the rule that all global variables should have named that are bookended by asterisks. Any time you need to impose a rule like that on your programmers, your language design is broken.

;; Fortunately, the situation is not all that hard to remedy. All that is needed is to implement the hypothetical L-LET and D-LET constructs from the Idiot's Guide, and to provide a way to declare global variables in a way that doesn't make them globally special.

;; Ladies and Gentlegeeks, I give you Global Variables Done Right:


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


(defun get-lexical-cell (sym)
  (or (get sym 'lexical-cell)
      (setf (get sym 'lexical-cell)
	    (copy-symbol sym))))


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






;; The best way to show what this code does is with an example:




;; ? (defc constant1 "Constant value")

;; CONSTANT1
;; ? (setf constant1 "Can't change a constant")

;; > Error: CONSTANT1 is immutable
;; > While executing: (SETF NON-SETTABLE-VALUE)
;; , in process Listener (6)
;; .
;; > Type cmd-. to abort, cmd-\ for a list of available restarts.
;; > Type :? for other options.
;; 1 >
;; ? (defc constant1 "Can't change a constant value, take 2")

;; CONSTANT1
;; ? constant1
;; "Constant value"
;; ? (defc constant1 "Can rebind a constant by specifying FORCE-REBIND" t)

;; CONSTANT1
;; ? constant1
;; "Can rebind a constant by specifying FORCE-REBIND"

;; ? (defv v1 "Global dynamic variable")

;; V1
;; ? (deflexical l1 "Global lexical variable")

;; L1
;; ? (defun test1 ()
;;      (list v1 l1))

;; TEST1
;; ? (test1)

;; ("Global dynamic variable" "Global lexical variable")

;; ? (let ((v1 1)
;; 	 (l1 1))
;;      (list v1 l1 (test1)))

;; (1 1 ("Global dynamic variable" "Global lexical variable"))

;; ? (dlet ((v1 "Dynamic binding 1")
;; 	  (l1 "Dynamic binding 2"))
;;      (list v1 l1 (test1)))

;; ("Dynamic binding 1" "Dynamic binding 2" ("Dynamic binding 1" "Global lexical variable"))

;; ? (let ((v1 1))
;;      (list v1 (dval v1)))

;; (1 "Global dynamic variable")

;; ? (let ((l1 1))
;;      (list l1 (lval l1)))

;; (1 "Global lexical variable")


;; ; Watch this trick!
;; ? (deflexical v1 "New global lexical binding for what was a dynamic variable")

;; V1
;; ? (defun foo ()
;;      v1)

;; FOO
;; ? (let ((v1 1))
;;      (list v1 (dval v1)
;; 	    (lval v1)
;; 	     (foo)))

;; (1 "Global dynamic variable" "New global lexical binding for what was a dynamic variable" "New global lexical binding for what was a dynamic variable")

;; ? (dlet ((v1 1))
;;      v1)

;; > Error: V1 is not a dynamic variable




;; Things to note:

;; 1. The design is completely orthogonal. In fact, a single variable can have a local lexical binding, a global lexical binding, and a dynamic binding all at the same time, and all of which are accessible in a single scope. No more pervasive special declarations. (In fact, no more special declarations at all. They are replaced with the DLET macro.)


;; 2. Constants are enforced to be immutable unless this is explicitly overridden by specifying FORCE-REBIND to be true.

;; 3. Because the design is orthogonal, it is actually a design choice whether dynamically binding a global lexical should be an error. There is no reason why this couldn't be allowed to proceed, to create a dynamic binding that could then be accessed (only)
;;  via the DVAL macro. But I decided that although it's possible to have both global lexical and dynamic bindings for the same variable at the same time, it's probably not a good idea. 



