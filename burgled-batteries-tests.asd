(defsystem burgled-batteries-tests
  :name "burgled-batteries-tests"
  :description "burgled-batteries tests"
  :long-description "rest-server tests"
  :components
  ((:module :test
	    :pathname "t/"
	    :components
	    ((:file "package")
	     (:file "tests")
	     (:file "numeric")
	     (:file "refcnts")
	     (:file "sanity")
	     (:file "sequences"))
	    :serial t))
  :serial t
  :depends-on (:burgled-batteries
	       :lift
	       :cl-quickcheck)
  :perform (asdf:test-op (o c)
			 (uiop:symbol-call :python-cffi.test :run-tests)))
