;;; burgled-batteries-tests.asdf --- system definition for the burgled-batteries
;;; test suite

(defsystem "burgled-batteries-tests"
  :name "burgled-batteries-tests"
  :description "burgled-batteries tests"
  :author "pinterface <pix@kepibu.org>"
  :license "MIT"
  :serial t
  :components
  ((:module "test"
    :pathname "t/"
    :components
    ((:file "packages")
     (:file "tests")
     (:file "sanity")
     (:file "numeric")
     (:file "refcnts")
     (:file "sequences")
     (:file "callbacks"))
    :serial t))
  :depends-on (#:burgled-batteries #:lift #:cl-quickcheck)
  :perform (test-op (o c) #+asdf3 (uiop:symbol-call '#:python-cffi.test '#:run-tests)))
