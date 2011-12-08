(in-package #:python.cffi)
#||

We want to expand into a CFFI defcallback, then parse the arguments so we can
pretend the Lisp function was defined as (defun fun (positional &key keyword
keyword) ...), and /then/ run the &body.

||#
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *callback-types* (make-hash-table))
  (defun set-callback-type (lisp-name flags)
    (setf (gethash lisp-name *callback-types*) flags))
  (defun get-callback-type (lisp-name)
    (or (gethash lisp-name *callback-types*)
        (error "No such callback ~A" lisp-name)))

  (defun filter-callback-args (args)
    (remove '&key args)))

(defmacro defpycallback (name return-type (&rest args) &body body)
  "Defines a Lisp function which is callable from Python.

RETURN-TYPE should be either :pointer, in which case type translation will not occur on arguments and you will be working with raw pointers, or a Python type (object, bool, etc.) in which case type translation of arguments will occur."
  (let ((self-type (if (eql return-type :pointer) :pointer '(object :borrowed)))
        (args-type (if (eql return-type :pointer) :pointer '(tuple  :borrowed)))
        (dict-type (if (eql return-type :pointer) :pointer '(dict   :borrowed))))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (defcallback ,name ,return-type
           ,@(cond ((find '&key args) `(((self ,self-type) (args ,args-type) (dict ,dict-type))
                                        (declare (ignorable self args dict))))
                   (t `(((self ,self-type) (args ,args-type))
                        (declare (ignorable self args)))))
         ,@body)
       (set-callback-type ',name
                          ,(cond
                             ((zerop (length args))    :no-arguments)
                             ((eql '&key (first args)) :keyword-arguments)
                             ((find '&key args)        :mixed-arguments)
                             (t                        :positional-arguments))))))

;; SELF is NIL.  ARGS is NIL.  (Or a null pointer, if no translation.)
(defpycallback test-no-arguments bool ()
  (format t "arg: self=~A args=~A~%" self args)
  t)

;; SELF tends to be NIL.  ARGS is a list of arguments.
(defpycallback test-arguments bool ((arg1 (bool :borrowed)))
  (format t "arg: self=~A args=~A~%" self args)
  t)

;; called as test_key_args(a=1, b=2, ...)
;; * SELF is NIL.  ARGS is an empty array.  DICT is a hashtable of name=value pairs
;; HOWEVER, if called as test_key_args(1, 2, a=3, b=4, ...)
;; * SELF is NIL.  ARGS is #(1 2).  DICT is a hashtable {a=3 b=4}
(defpycallback test-key-args bool (&key (arg1 (bool :borrowed)))
  (format t "key: self=~A args=~A dict=~A~%" self args dict)
  t)

;; SELF is NIL.  ARGS is an array of the positional (non-keyword) parameters.
;; DICT is a hashtable of the keyword parameters.
(defpycallback test-pos+key-args bool ((arg1 (bool :borrowed)) &key (arg2 (bool :borrowed)))
  (format t "key: self=~A args=~A dict=~A~%" self args dict)
  t)

(defpycallback test-no-translation :pointer ()
  (null-pointer))

(defun init-func-def (ptr name flags meth &optional (doc (null-pointer)))
  (setf (foreign-slot-value ptr 'method-def 'name)  name
        (foreign-slot-value ptr 'method-def 'flags) flags
        (foreign-slot-value ptr 'method-def 'meth)  meth
        (foreign-slot-value ptr 'method-def 'doc)   doc))

(defun make-pytype (&key name c-struct documentation)
  (let ((ptr (foreign-alloc '%type)))
    (setf (%object.refcnt       ptr) 1
          (%object.type*        ptr) (null-pointer) ; +Type.Type+?
          (%var.size            ptr) 0
          (%type.name           ptr) name
          (%type.basicsize      ptr) (foreign-type-size c-struct)
          (%type.itemsize       ptr) 0
          (%type.dealloc        ptr) (null-pointer) ; FIXME: should point to a C callback or something
          (%type.print          ptr) (null-pointer)
          (%type.getattr        ptr) (null-pointer)
          (%type.setattr        ptr) (null-pointer)
          (%type.compare        ptr) (null-pointer)
          (%type.repr           ptr) (null-pointer)
          (%type.as-number      ptr) (null-pointer)
          (%type.as-sequence    ptr) (null-pointer)
          (%type.as-mapping     ptr) (null-pointer)
          (%type.hash           ptr) (null-pointer)
          (%type.call           ptr) (null-pointer)
          (%type.str            ptr) (null-pointer)
          (%type.getattro       ptr) (null-pointer)
          (%type.setattro       ptr) (null-pointer)
          (%type.as-buffer      ptr) (null-pointer)
          (%type.flags          ptr) '()
          (%type.doc            ptr) (or documentation (null-pointer))
          (%type.traverse       ptr) (null-pointer)
          (%type.clear          ptr) (null-pointer)
          (%type.richcompare    ptr) (null-pointer)
          (%type.weaklistoffset ptr) 0
          (%type.iter           ptr) (null-pointer)
          (%type.iternext       ptr) (null-pointer)
          (%type.methods        ptr) (null-pointer) ; FIXME
          (%type.members        ptr) (null-pointer) ; FIXME
          (%type.getset         ptr) (null-pointer)
          (%type.base           ptr) (null-pointer) ; FIXME
          (%type.dict*          ptr) (null-pointer)
          (%type.descr-get      ptr) (null-pointer)
          (%type.descr-set      ptr) (null-pointer)
          (%type.dictoffset     ptr) 0
          (%type.init           ptr) (null-pointer)
          (%type.alloc          ptr) (null-pointer)
          (%type.new            ptr) (foreign-symbol-pointer "PyType_GenericNew")
          (%type.free           ptr) (null-pointer)
          (%type.is-gc          ptr) (null-pointer)
          (%type.bases          ptr) (null-pointer)
          (%type.mro            ptr) (null-pointer)
          (%type.cache          ptr) (null-pointer)
          (%type.subclasses     ptr) (null-pointer)
          (%type.weaklist       ptr) (null-pointer))
    (type.ready ptr)))

(defun make-test-module ()
  (let* ((funcs '(("no_args"      test-no-arguments)
                  ("args"         test-arguments)
                  ("key_args"     test-key-args)
                  ("pos_key_args" test-pos+key-args)
                  ("no_trans"     test-no-translation)))
         (ptr (foreign-alloc 'method-def :count (1+ (length funcs)))))
    (loop :for i :from 0
          :for (py lisp) :in funcs
          :for defptr = (mem-aref ptr 'method-def i)
          :do (init-func-def defptr py (get-callback-type lisp) (get-callback lisp)))
    (init-func-def (mem-aref ptr 'method-def (length funcs)) (null-pointer) 0 (null-pointer))
    (.init-module* "lisp_test" ptr)))

#+(or) (make-test-module)
#+(or) (burgled-batteries:import "lisp_test")
