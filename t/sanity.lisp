(defpackage #:python-cffi.test
  (:use #:cl #:cl-quickcheck))
(in-package #:python-cffi.test)

(burgled-batteries:startup-python)

(let (type-vars)
  (do-external-symbols (s '#:cpython type-vars)
    (let ((symbol-name (symbol-name s)))
      (when (and (string= '#:+ symbol-name :end2 1)
                 (string= '#:.type+ symbol-name :start2 (- (length symbol-name) 6)))
        (push s type-vars))))
  (loop :for type-var :in type-vars
        :do (assert (plusp (cpython::%object.refcnt (eval type-var)))
                    ()
                    "Python type ~S does not have a positive reference count."
                    type-var)
            (assert (string= "<type '" (cpython:object.str (eval type-var)) :end2 7)
                    ()
                    "Python type ~S is not stringified as a type."
                    type-var)))
