(in-package #:python-cffi.test)

(lift:addtest (burgled-batteries)
  refcnt-test
  (let (type-vars)
    (do-external-symbols (s '#:cpython type-vars)
      (let ((symbol-name (symbol-name s)))
	(when (and (string= '#:+ symbol-name :end2 1)
		   (string= '#:.type+ symbol-name :start2 (- (length symbol-name) 6)))
	  (push s type-vars))))
    (loop :for type-var :in type-vars
       :do (lift:ensure (plusp (cpython::%object.refcnt (eval type-var)))
                    :report
                    "Python type ~S does not have a positive reference count."
		    :arguments (type-var))
            (lift:ensure (string= "<type '" (cpython:object.str (eval type-var)) :end2 7)
                    :report
                    "Python type ~S is not stringified as a type."
		    :arguments (type-var)))))

(lift:addtest (burgled-batteries)
  apply-test
  (let ((nums (alexandria:shuffle (list 1 2 3 4 5 6 7 8 9 10))))
    (burgled-batteries::with-cpython-pointer (min-fn (burgled-batteries:run* "min"))
      (lift:ensure (= (apply #'burgled-batteries:apply min-fn nums)
		      (apply #'min nums))
            :report
            "Something seems to be wrong with APPLY.  Have types been switched again?"))))

#+nil(lift:addtest (burgled-batteries)
  defpyfun-test
  (burgled-batteries:defpyfun ("max" pymax) (&rest args))
  (let ((nums (alexandria:shuffle (list 1 2 3 4 5 6 7 8 9 10))))
    (lift:ensure (= (pymax nums) (apply #'max nums))
		 :report
		 "Either MAX is broken or DEFPYFUN is having issues.")))
