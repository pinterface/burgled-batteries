(defpackage #:python-cffi.test
  (:use #:cl #:cl-quickcheck))
(in-package #:python-cffi.test)

(pyffi::startup-python)

(quickcheck
  (let ((*size* (1- (expt 2 31))))
    (for-all ((v an-integer))
      (is= v (python.cffi:int.from-long v)))
    (for-all ((v a-boolean))
      (is= v (python.cffi:bool.from-long (if v 1 0))))
    (for-all ((v an-integer))
      (is= v (python.cffi:long.from-long v))))
  (let ((*size* (/ most-positive-single-float 2)))
    (for-all ((v a-real))
      (let ((v (coerce v 'double-float)))
        (is= v (python.cffi:float.from-double v))))))
