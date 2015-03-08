(defsystem :auxfns
  :name "auxfns"
  :depends-on (:optima ; used for "auxfns.def"
	       :clunit) 
  :components ((:file "auxfns")
	       (:file "auxfns.pipe")
	       (:file "auxfns.cont")
	       (:file "auxfns.def" :depends-on ("auxfns"))
	       ))



