(in-package #:python-cffi.test)

(defparameter *callback-ret* nil)

;; SELF is NIL.  ARGS is NIL.  (Or a null pointer, if no translation.)
(python.cffi::defpycallback test-no-arguments python.cffi::bool ()
  (format t "arg: self=~A args=~A~%" python.cffi::self python.cffi::args)
  (setf *callback-ret* (list python.cffi::self python.cffi::args)))

;; SELF tends to be NIL.  ARGS is a list of arguments.
(python.cffi::defpycallback test-arguments python.cffi::bool ((arg1 (bool :borrowed)))
  (format t "arg: self=~A args=~A arg1=~A~%" 
	  python.cffi::self 
	  python.cffi::args
	  arg1)
  (setf *callback-ret* 
	(list python.cffi::self 
	      python.cffi::args
	      arg1)))

;; called as test_key_args(a=1, b=2, ...)
;; * SELF is NIL.  ARGS is an empty array.  DICT is a hashtable of name=value pairs
;; HOWEVER, if called as test_key_args(1, 2, a=3, b=4, ...)
;; * SELF is NIL.  ARGS is #(1 2).  DICT is a hashtable {a=3 b=4}
(python.cffi::defpycallback test-key-args python.cffi::bool 
    (&key (arg1 (bool :borrowed)))
  (format t "key: self=~A args=~A dict=~A arg1=~A~%" 
	  python.cffi::self 
	  python.cffi::args 
	  python.cffi::dict
	  arg1)
  (setf *callback-ret* (list python.cffi::self 
			     python.cffi::args 
			     python.cffi::dict
			     arg1)))

;; SELF is NIL.  ARGS is an array of the positional (non-keyword) parameters.
;; DICT is a hashtable of the keyword parameters.
(python.cffi::defpycallback test-pos+key-args python.cffi::bool 
    ((arg1 (bool :borrowed)) &key (arg2 (bool :borrowed)))
  (format t "key: self=~A args=~A dict=~A arg1=~A arg2=~A~%" 
	  python.cffi::self 
	  python.cffi::args 
	  python.cffi::dict
	  arg1
	  arg2)
  (setf *callback-ret* (list python.cffi::self 
			     python.cffi::args 
			     python.cffi::dict
			     arg1
			     arg2)))

(python.cffi::defpycallback test-no-translation :pointer ()
  (python.cffi::null-pointer))

(defun make-callbacks-test-module ()
  (python.cffi::build-module "callbacks_test"
   '(("no_args"   .   test-no-arguments)
     ("args"      .   test-arguments)
     ("key_args"  .   test-key-args)
     ("pos_key_args" . test-pos+key-args)
     ("no_trans"  .   test-no-translation))))

(addtest (burgled-batteries)
  callbacks-test
  
  (make-callbacks-test-module)

  (burgled-batteries:import "callbacks_test")

  ;; callbacks_test.no_args()

  (LET ((TRANSFORMED1078
       (CFFI:CONVERT-FROM-FOREIGN
	(burgled-batteries::with-cpython-pointer (ptr (burgled-batteries::object.get-attr-string* (burgled-batteries::run* "callbacks_test") "no_args")) 
	  (burgled-batteries::object.call-object* ptr nil))
	'PYTHON.CFFI::OBJECT!)))
  TRANSFORMED1078)

  (lift:ensure (equalp *callback-ret* (list nil nil))) 

  ;; callbacks_test.args(22, 'a')

  (LET ((TRANSFORMED1078
       (CFFI:CONVERT-FROM-FOREIGN
	(burgled-batteries::with-cpython-pointer (ptr (burgled-batteries::object.get-attr-string* (burgled-batteries::run* "callbacks_test") "args")) 
	  (burgled-batteries::object.call-object* ptr 
						  (list (PYTHON.CFFI:NUMBER.INT* 22) (PYTHON.CFFI:STRING.FROM-STRING* "a"))))
	'PYTHON.CFFI::OBJECT!)))
  TRANSFORMED1078)
  
  (lift:ensure (equalp *callback-ret* (list nil (list 22 "a") 22)))

  ;; callbacks_test.key_args(arg1='foo')

  (LET ((TRANSFORMED1102
       (CFFI:CONVERT-FROM-FOREIGN
        (LET ((KWARGS1104 (MAKE-HASH-TABLE)))
          (SETF (GETHASH "arg1" KWARGS1104) (PYTHON.CFFI:STRING.FROM-STRING* "foo"))
          (BURGLED-BATTERIES::WITH-CPYTHON-POINTER (PYFUN1103
                                                    (PYTHON.CFFI:OBJECT.GET-ATTR-STRING*
                                                     (BURGLED-BATTERIES::RUN*
                                                      "callbacks_test")
                                                     "key_args"))
            (PYTHON.CFFI:OBJECT.CALL* PYFUN1103
                                      nil
                                      KWARGS1104)))
        'PYTHON.CFFI::OBJECT!)))
  TRANSFORMED1102)

  (destructuring-bind (self args dict arg1) *callback-ret*
    (lift:ensure (equalp args nil))
    (lift:ensure (equalp (gethash "arg1" dict) "foo"))
    (lift:ensure (equalp arg1 "foo"))))
