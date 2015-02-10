(defpackage #:python-cffi.test
  (:use #:cl #:cl-quickcheck)
  (:import-from #:lift #:deftestsuite #:addtest)
  (:shadow #:assert)
  (:export #:run-tests))
