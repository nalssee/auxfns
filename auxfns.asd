(defsystem :auxfns
  :name "auxfns"
  :depends-on (:optima) ; used for "auxfns.def"
  :components ((:file "auxfns")
	       (:file "auxfns.pipe")
	       ;; (:file "auxfns.gl")
	       (:file "auxfns.cont")
	       (:file "auxfns.def" :depends-on ("auxfns"))))




