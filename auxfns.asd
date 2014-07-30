(defsystem :auxfns
  :name "auxfns"
  :depends-on (:optima) ; used for "auxfns.def"
  :components ((:file "auxfns")
	       (:file "auxfns.pipe")
	       (:file "auxfns.gl")
	       (:file "auxfns.cont" :depends-on ("auxfns.gl"))
	       (:file "auxfns.def" :depends-on ("auxfns"))))




