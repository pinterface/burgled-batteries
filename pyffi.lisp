(defpackage :pyffi
  (:use :common-lisp :cffi))

(in-package pyffi)

(define-foreign-library python-library                                     
  (:darwin (:framework "Python"))                                               
  (:unix (:or "libpython2.5.so.1.0" "libpython2.4.so.1.0" "libpython2.3.so.1.0")
)                                                                               
  (:windows (:or "python25.dll" "python24.dll" "python23.dll") )                
  (t (:default "libpython")))

(use-foreign-library python-library)

(define-condition python-error ()
  ((code :initarg :exc :reader exception))
  (:report (lambda (c s) 
	     (format s "Python error: ~a" (exception c)))))

(defmacro defpytype (name to from)
  (let ((borrowed (intern (format nil "~a-BORROWED" name) :keyword)))
    `(progn
       (defctype ,name :pointer)
       (defctype ,borrowed :pointer)
       (defmethod cffi:translate-to-foreign (value (type (eql ,name))) 
	 (progn ,@to))
       (defmethod cffi:translate-from-foreign (value (type (eql ,borrowed)))
	 (progn ,@from))
       (defmethod cffi:translate-from-foreign (value (type (eql ,name)))
	 (unwind-protect (progn ,@from) (py-decref value))))))

(defun raise-python-exception ()
  (let ((exc (pyerr-occurred)))
    (let ((desc (pyobject-str exc)))
      (error 'python-error :exc desc))))

(defpytype :py-int
    ((pyint-fromlong value))
  ((pyint-aslong value)))

(defpytype :py-tuple
  ((let ((len (length value)))
     (let ((tup (pytuple-new len)))
       (if (null-pointer-p tup) (raise-python-exception))
       (dotimes (i len tup)
	 (if (not (zerop (pytuple-setitem tup i (svref value i))))
	     (raise-python-exception))))))
  ((let ((len (pytuple-size value)))
    (let ((tup (make-array len)))
      (dotimes (i len tup) 
	(setf (svref tup i) (pytuple-getitem value i)))))))

(defpytype :py-list
  ((let ((len (length value)))
     (let ((lst (pylist-new len)))
       (if (null-pointer-p lst) (raise-python-exception))
       (dotimes (i len lst)
	 (if (not (zerop (pylist-setitem lst i (nth i value))))
	     (raise-python-exception))))))
  ((let ((len (pylist-size value)))
    (let (lst)
      (dotimes (i len) (push (pylist-getitem value i) lst))
      (reverse lst)))))

(defpytype :py-string
    ((pystring-fromstring value))
  ((pystring-asstring value)))

(defpytype :py-dict ;accoc-list?
    ((let ((dct (pydict-new)))
       (if (null-pointer-p dct) (raise-python-exception))
       (maphash 
	(lambda (k v) 
	  (if (not (zerop (pydict-setitem dct k v)))
	      (raise-python-exception)))
	value)
       dct))
  ((let ((dct (make-hash-table :test #'equal)) 
	 (its (pydict-items value)))
     (dolist (i its dct) (setf (gethash (svref i 0) dct) (svref i 1))))))
  
(defpytype :py-object
  ((cond
     ((integerp value) (translate-to-foreign value :py-int))
     ((listp value) (translate-to-foreign value :py-list))
     ((and (stringp value) (every (lambda (c) (> 256 (char-code c))) value))
      (translate-to-foreign value :py-string))
     ((vectorp value) (translate-to-foreign value :py-tuple))
     ((hash-table-p value) (translate-to-foreign value :py-dict))
     ((pointerp value) value)))
  ((if (null-pointer-p value) (raise-python-exception))
   (let ((typ (pyobject-type value)))
     (let ((typ-name (pyobject-str typ)))
       (py-decref typ)
       (let ((typ-key
	      (cond
		((string= "<type 'int'>" typ-name) :py-int-borrowed)
		((string= "<type 'list'>" typ-name) :py-list-borrowed)
		((string= "<type 'str'>" typ-name) :py-string-borrowed)
		((string= "<type 'dict'>" typ-name) :py-dict-borrowed)
		((string= "<type 'tuple'>" typ-name) :py-tuple-borrowed))))
	 (if typ-key
	     (translate-from-foreign value typ-key)
	     (progn
	       (if (not (eql type :py-object-borrowed)) (py-incref value))
	       value)))))))
	   
(defcfun "Py_Initialize" :void)
(defcfun "Py_Finalize" :void)
(defcfun "Py_IncRef" :void (o :pointer))
(defcfun "Py_DecRef" :void (o :pointer))
(defcfun "PyCallable_Check" :boolean (o :pointer))
(defcfun "PyDict_New" :pointer)
(defcfun "PyDict_Keys" :py-list (d :pointer))
(defcfun "PyDict_Size" :int (d :pointer))
(defcfun "PyDict_GetItem" :py-object-borrowed (d :pointer) (key :py-object))
(defcfun "PyDict_GetItemString" :py-object-borrowed (d :pointer) (key :string))
(defcfun "PyDict_Items" :py-list (d :pointer))
(defcfun "PyDict_SetItem" :int (d :pointer) (key :py-object) (val :py-object))
(defcfun "PyErr_Occurred" :pointer)
(defcfun "PyImport_GetModuleDict" :py-dict)
(defcfun "PyImport_Import" :pointer (name :py-string))
(defcfun "PyInt_AsLong" :long (o :pointer))
(defcfun "PyInt_FromLong" :pointer (i :long))
(defcfun "PyList_New" :pointer (size :int))
(defcfun "PyList_Size" :int (lst :pointer))
(defcfun "PyList_GetItem" :py-object-borrowed (lst :pointer) (index :int))
(defcfun "PyList_SetItem" :int (lst :pointer) (index :int) (o :py-object))
(defcfun "PyObject_CallObject" :py-object (o :pointer) (args :py-tuple))
(defcfun "PyObject_GetAttrString" :py-object (o :pointer) (attr :string))
(defcfun "PyObject_Str" :py-string (o :pointer))
(defcfun "PyObject_Type" :pointer (o :pointer))
(defcfun "PyString_AsString" :string (s :pointer))
(defcfun "PyString_FromString" :pointer (s :string))
(defcfun "PyTuple_New" :pointer (size :int))
(defcfun "PyTuple_Size" :int (lst :pointer))
(defcfun "PyTuple_GetItem" :py-object-borrowed (lst :pointer) (index :int))
(defcfun "PyTuple_SetItem" :int (lst :pointer) (index :int) (o :py-object))

(defun py-apply (func &rest args)
  (pyobject-callobject func (apply #'vector args)))

;should add keyword arguments
(defmacro defpyfun (module name &rest args)
 (let ((mdl (gensym)) (func (gensym)))
   `(defun 
	,(intern (format nil "~a.~a" (string-upcase module) 
			      (string-upcase name))) 
	,args
     (let ((,mdl (pyimport-import ,module)))
       (if (null-pointer-p ,mdl) (raise-python-exception)
	   (unwind-protect
		(let ((,func (pyobject-getattrstring ,mdl ,name)))
		  (if (null-pointer-p ,func) (raise-python-exception)
		      (unwind-protect
			   (if (not (pycallable-check ,func))
			       (raise-python-exception)
			       (pyobject-callobject ,func (vector ,@args)))
			(py-decref ,func))))
	     (py-decref ,mdl)))))))

;two lame macros follow

(defmacro defpyslot (name)
  `(defun ,(intern (string-upcase name)) (obj)
     (pyobject-getattrstring obj ,name)))

(defmacro defpymethod (name)
  `(defun ,(intern (string-upcase name)) (obj &rest args)
     (pyobject-callobject 
      (pyobject-getattrstring obj ,name) 
      (apply #'vector args))))

;(defmacro with-python-string ((var str) &body body)
;  `(let ((,var (pystring-fromstring ,str)))
;     (unwind-protect (progn ,@body) (py-decref ,var))))

