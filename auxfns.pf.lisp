;; profiling from PAIP

(defpackage :auxfns.pf
  (:use :cl)
  (:export :with-profiling))

(in-package :auxfns.pf)

(declaim (inline profile-enter profile-exit inc-profile-time))

(defvar *profile-call-stack* nil)

(defun profiled-fn (fn-name fn)
  "Return a function taht increments the count"
  #'(lambda (&rest args)
      (profile-enter fn-name)
      (multiple-value-prog1 (apply fn args)
	(profile-exit fn-name))))


(defun profile-count (fn-name) (get fn-name 'profile-count))


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

(defun get-fast-time ()
  (get-internal-real-time))

(defun fast-time-difference (end start)
  (- end start))

(defun fast-time->seconds (time)
  (/ time internal-time-units-per-second))


(defun profile-enter (fn-name)
  (incf (get fn-name 'profile-count))
  (unless (null *profile-call-stack*)
    ;; Time charged against the calling function:
    (inc-profile-time (first *profile-call-stack*)
		      (car (first *profile-call-stack*))))
  ;; Put a new entry on the stack
  (push (cons fn-name (get-fast-time))
	*profile-call-stack*))



(defun profile-exit (fn-name)
  ;; Time chared against the calling function:
  (inc-profile-time (pop *profile-call-stack*)
		    fn-name)
  ;; Changed the top entry to reflect current time
  (unless (null *profile-call-stack*)
    (setf (cdr (first *profile-call-stack*))
	  (get-fast-time))))


(defun inc-profile-time (entry fn-name)
  (incf (get fn-name 'profile-time)
       (fast-time-difference (get-fast-time) (cdr entry))))

(defun profile-report (&optional
			 (fn-names (copy-list *profiled-functions*)) 
			 (key #'profile-count))
  "Report profiling statistics on given functions."
  (let ((total-time (reduce #'+ (mapcar #'profile-time fn-names))))
    (unless (null key)
      (setf fn-names (sort fn-names #'> :key key)))
    (format t "~&Total elapsed time: ~f seconds."
	    (fast-time->seconds total-time))
    ;; These kind of events may happen 
    (cond ((zerop total-time)
	   (format t "~&The whole process takes too little time"))
	  (t
	   (format t "~&~10:<Count~> ~7:<Secs~> ~5:<Time~>% Name")
	   (loop for name in fn-names do
		(format t "~&~10D ~7,2F ~5d% ~A"
			(profile-count name)
			(fast-time->seconds (profile-time name))
			(round (/ (profile-time name) total-time) 0.01)
			name))))))


(defun profile-time (fn-name) (get fn-name 'profile-time))


(defmacro with-profiling (fn-names &body body)
  `(progn
     (_unprofile ,@fn-names)
     (_profile ,@fn-names)
     (setf *profile-call-stack* nil)
     (unwind-protect
	  (progn ,@body)
       (profile-report ',fn-names)
       (_unprofile ,@fn-names))))


