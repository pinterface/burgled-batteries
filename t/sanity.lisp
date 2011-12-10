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

(let ((nums (alexandria:shuffle (list 1 2 3 4 5 6 7 8 9 10))))
  (burgled-batteries::with-cpython-pointer (min-fn (burgled-batteries:run* "min"))
    (assert (= (apply #'burgled-batteries:apply min-fn nums)
               (apply #'min nums))
            ()
            "Something seems to be wrong with APPLY.  Have types been switched again?")))

(burgled-batteries:defpyfun ("max" pymax) (&rest args))
(let ((nums (alexandria:shuffle (list 1 2 3 4 5 6 7 8 9 10))))
  (assert (= (pymax nums) (apply #'max nums))
          ()
          "Either MAX is broken or DEFPYFUN is having issues."))
