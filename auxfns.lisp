(defpackage :auxfns
  (:use :cl)
  (:export :with-gensyms
	   :once-only
	   :nlet

	   :range
	   
	   :lcomp

	   :alambda
	   :rec

	   :mappend

	   :memoize
	   :clear-memoize
	   :curry

	   :aif
	   :aif2
	   :acond
	   :acond2
	   :it

	   :group
	   :file->string
	   :string->file
	   :numbering

	   :print-tree

	   
	   
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


(defmacro abbrev (short long)
  `(defmacro ,short (&rest args)
     `(,',long ,@args)))

;; (defmacro abbrev (short long)
;;   (list 'defmacro short (list '&rest 'args)
;; 	(list 'cons (list 'quote long) 'args)))


;;=========================================================

(defun memo (fn name key test)
  (let ((table (make-hash-table :test test)))
    (setf (get name 'memo) table)
    #'(lambda (&rest args)
	(let ((k (funcall key args)))
	  (multiple-value-bind (val found-p) (gethash k table)
	    (if found-p
		val
		(setf (gethash k table)
		      (apply fn args))))))))


(defun memoize (fn-name &key (key #'first) (test #'eql))
  (setf (symbol-function fn-name)
	(memo (symbol-function fn-name) fn-name key test)))

(defun clear-memoize (fn-name)
  (let ((table (get fn-name 'memo)))
    (when table (clrhash table))))


(defun curry (fn &rest args)
  (lambda (&rest more-args)
    (apply fn (append args more-args))))






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




;; whole file to a string
(defun file->string (path)
  (with-open-file (stream path)
    (let ((data (make-array (file-length stream)
			    :element-type 'character :fill-pointer t)))
      (setf (fill-pointer data)
	    (read-sequence data stream))
      data)))

(defun string->file (x file)
  (with-open-file (s file
		     :direction :output
		     :if-exists :supersede)
    (format s "~A" x)))

;; (numbering '(a a b a c c b b a))
(defun numbering (xs &key (key #'identity) (start 1) (test #'eql))
  "Number a list"
  (let ((count-list '()))
    (values
     (loop for x in xs collect
	  (let ((p (assoc (funcall key x) count-list :key key :test test)))
	    (cond (p (list x (incf (second p))))
		  (t (push (list x start) count-list)
		     (list x start)))))
     (nreverse count-list))))


(defun range (start end &optional (step 1))
  (if (> end start)
      (loop for i from start below end by step
	 collect i)
      (loop for i from start above end by step
	 collect i)))



;; ============================================
;; List comprehension
;; ============================================
;; (lcomp
;;  :binds ((a '(1 2 3 4))
;; 	 (b '(1 2 3)))
;;  :test (> (+ a b) 3)
;;  :result (list a b))
(defmacro lcomp (&key binds result (test t))
  (labels ((expand (binds result test accum)
	     (if (null (cdr binds))
		 `(loop for ,(caar binds) in ,(cadar binds) when ,test do
		       (push ,result ,accum))
		 `(loop for ,(caar binds) in ,(cadar binds) do
		       ,(expand (cdr binds) result test accum)))))
    (let ((accum (gensym)))
      `(let (,accum)
	 ,(expand binds result test accum)
	 (nreverse ,accum)))))





(defun print-tree (tree &optional (offset 0))
  (loop for node in tree do
       (terpri)
       (loop repeat offset do (princ " |"))
       (format t "-~a" (car node))
       (print-tree (cdr node) (1+ offset))))









