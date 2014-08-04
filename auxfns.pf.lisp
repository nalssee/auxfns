;; profiling from PAIP

(defpackage :auxfns.pf
  (:use :cl)
  (:export :with-profiling))

(in-package :auxfns.pf)





(defun profiled-fn (fn-name fn)
  "Return a function taht increments the count"
  (lambda (&rest args)
    (incf (get fn-name 'profile-count))
    (apply fn args)))



(defun profile-count (fn-name) (get fn-name 'profile-count))
(defun profile-report (fn-names &optional (key #'profile-count))
  "Report profiling statistics on given functions"
  (loop for name in (sort fn-names #'> :key key) do
       (format t "~&~7D ~A" (profile-count name) name)))


(defvar *profiled-functions* nil
  "Function names that are currently profiled")

(defmacro _profile (&rest fn-names)
  "Profile fn-names. With no args, list profiled functions"
  `(mapcar #'profile1
	   (setf *profiled-functions*
		 (union *profiled-functions* ',fn-names))))

(defmacro _unprofile (&rest fn-names)
  "Stop profiling fn-names. With no args, stop all profiling."
  `(progn
     (mapcar #'unprofile1
	     ,(if fn-names `',fn-names '*profiled-functions*))
     (setf *profiled-functions*
	   ,(if (null fn-names)
		nil
		`(set-difference *profiled-functions*
				 ',fn-names)))))


(defun profile1 (fn-name)
  "Make the function count how often it is called"
  ;; First save away the old, unprofiled function
  ;; Then make the name be a new function that increments
  ;; a counter and the calls the original function
  (let ((fn (symbol-function fn-name)))
    (unless (eq fn (get fn-name 'profiled-fn))
      (let ((new-fn (profiled-fn fn-name fn)))
	(setf (symbol-function fn-name) new-fn
	      (get fn-name 'profiled-fn) new-fn
	      (get fn-name 'unprofiled-fn) fn
	      (get fn-name 'profile-time) 0
	      (get fn-name 'profile-count) 0))))
  fn-name)


(defun unprofile1 (fn-name)
  "Make the function stop counting how often it is called"
  (setf (get fn-name 'profile-time) 0)
  (setf (get fn-name 'profile-count) 0)
  
  (when (eq (symbol-function fn-name) (get fn-name 'profiled-fn))
    ;; Normal case: restore unprofiled version
    (setf (symbol-function fn-name)
	  (get fn-name 'unprofiled-fn)))
  fn-name)








