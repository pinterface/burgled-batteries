(defpackage :pyffi
  (:use :common-lisp :cffi))

(in-package pyffi)

(define-foreign-library python-library
  (:darwin (:framework "Python"))
  (:unix (:or "libpython2.7.so.1.0" "libpython2.6.so.1.0" "libpython2.5.so.1.0" "libpython2.4.so.1.0" "libpython2.3.so.1.0"))
  (:windows (:or "python27.dll" "python26.dll" "python25.dll" "python24.dll" "python23.dll"))
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

(defpytype :py-float
    ((pyfloat-fromdouble value))
  ((pyfloat-asdouble value)))

(defpytype :py-string
    ((pystring-fromstring value))
  ((pystring-asstring value)))

(defpytype :py-unicode
    ((let ((len (length value)))
       (with-foreign-object (buf :uint32 len)
	 (dotimes (i len) 
	   (setf (mem-aref buf :uint32 i) (char-code (char value i))))
	 (pyunicodeucs4-fromunicode buf len))))
  ((let ((buf (pyunicodeucs4-asunicode value))
	 (len (pyunicodeucs4-getsize value)))
     (map 'string #'code-char
	  (loop for i below len collect (mem-aref buf :uint32 i))))))

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

(defvar *python-types*
  '(("<type 'int'>" . :py-int-borrowed)
    ("<type 'float'>" . :py-float-borrowed)
    ("<type 'list'>" . :py-list-borrowed)
    ("<type 'str'>" . :py-string-borrowed)
    ("<type 'unicode'>" . :py-unicode-borrowed)
    ("<type 'dict'>" . :py-dict-borrowed)
    ("<type 'tuple'>" . :py-tuple-borrowed)))
  
(defpytype :py-object
  ((cond
     ((integerp value) (translate-to-foreign value :py-int))
     ((floatp value) (translate-to-foreign value :py-float))
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
       (let ((typ-key (cdr (assoc typ-name *python-types* :test #'string=))))
	 (if typ-key
	     (translate-from-foreign value typ-key)
	     (progn
	       (if (not (eql type :py-object-borrowed)) (py-incref value))
	       value)))))))

(defconstant +py-file-input+ 257)
(defconstant +py-single-input+ 256)
(defconstant +py-eval-input+ 258)
	   
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
(defcfun "PyFloat_AsDouble" :double (s :pointer))
(defcfun "PyFloat_FromDouble" :pointer (s :double))
(defcfun "PyImport_GetModuleDict" :py-dict)
(defcfun "PyImport_Import" :pointer (name :py-string))
(defcfun "PyImport_ImportModule" :pointer (name :string))
;; FIXME: Python docs say ImportModuleEx became an alias for ImportModuleLevel
;;        in 2.6, which apparently means "no longer exists as a function in the
;;        shared library".  If we're going to support older versions of Python,
;;        we'll have to work around that.
(defcfun "PyImport_ImportModuleEx" :pointer (name :string) (globals :pointer) (locals :pointer) (fromlist :pointer))
(defcfun "PyImport_ImportModuleLevel" :pointer (name :string) (globals :pointer) (locals :pointer) (fromlist :pointer) (level :int))
(defcfun "PyImport_AddModule" :pointer (name :string))
(defcfun "PyInt_AsLong" :long (o :pointer))
(defcfun "PyInt_FromLong" :pointer (i :long))
(defcfun "PyList_New" :pointer (size :int))
(defcfun "PyList_Size" :int (lst :pointer))
(defcfun "PyList_GetItem" :py-object-borrowed (lst :pointer) (index :int))
(defcfun "PyList_SetItem" :int (lst :pointer) (index :int) (o :py-object))
(defcfun "PyModule_GetDict" :py-dict (m :pointer))
(defcfun ("PyModule_GetDict" pymodule-getdict-as-ptr) :pointer (m :pointer))
(defcfun "PyObject_CallObject" :py-object (o :pointer) (args :py-tuple))
(defcfun "PyObject_GetAttrString" :py-object (o :pointer) (attr :string))
(defcfun "PyObject_SetAttrString" :int (o :pointer) (s :string) (a :py-object))
(defcfun "PyObject_Str" :py-string (o :pointer))
(defcfun "PyObject_Type" :pointer (o :pointer))
(defcfun "PyRun_String" :py-object (str :string) (start :int) (globals :pointer) (locals :pointer))
(defcfun "PyString_AsString" :string (s :pointer))
(defcfun "PyString_FromString" :pointer (s :string))
(defcfun "PyTuple_New" :pointer (size :int))
(defcfun "PyTuple_Size" :int (lst :pointer))
(defcfun "PyTuple_GetItem" :py-object-borrowed (lst :pointer) (index :int))
(defcfun "PyTuple_SetItem" :int (lst :pointer) (index :int) (o :py-object))
(defcfun "PyUnicodeUCS4_AsUnicode" :pointer (s :pointer))
(defcfun "PyUnicodeUCS4_FromUnicode" :pointer (s :pointer) (size :int))
(defcfun "PyUnicodeUCS4_GetSize" :int (u :pointer))
(defcfun "PyUnicodeUCS2_AsUnicode" :pointer (s :pointer))
(defcfun "PyUnicodeUCS2_FromUnicode" :pointer (s :pointer) (size :int))
(defcfun "PyUnicodeUCS2_GetSize" :int (u :pointer))

(defvar *py-main-module*)
(defvar *py-main-module-dict*)
(defvar *pyphuns* (make-hash-table))

(defun pyth-on ()
  (py-initialize)
  (setf *py-main-module* (pyimport-addmodule "__main__"))
  (setf *py-main-module-dict* (pymodule-getdict-as-ptr *py-main-module*))
  (let ((tmp (pyrun-string "from __builtin__ import *" +py-single-input+ 
			   *py-main-module-dict* (null-pointer))))
    (py-decref tmp)))

(defun pyth-off ()
  (setf *py-main-module* nil *py-main-module-dict* nil)
  (clrhash *pyphuns*)
  (py-finalize))

(defun py-require (name)
  (let ((p (position #\. name)))
    (let ((m (pyimport-importmodulelevel
	      name *py-main-module-dict* *py-main-module-dict* (null-pointer) -1)))
      (unwind-protect
	   (if (= -1 (pyobject-setattrstring 
		      *py-main-module* (if p (subseq name 0 p) name) m))
	       (raise-python-exception))
	(py-decref m))))) 

(defun py-eval (expression)
  (pyrun-string 
   expression +py-eval-input+ *py-main-module-dict* *py-main-module-dict*))

(defun py-apply (func &rest args)
  (pyobject-callobject func (apply #'vector args)))

;should add keyword arguments
(defmacro defpyfun (expression args)
 (let ((func (gensym))
       (name (if (listp expression) 
		 (second expression) 
		 (intern (string-upcase expression))))
       (form (if (listp expression) (first expression) expression)))
   `(defun ,name ,args
      (let ((,func (gethash ',name *pyphuns*)))
	(if (null ,func)
	    (progn
	      (setf ,func (py-eval ,form))
	      (if (not (pycallable-check ,func))
		  (progn
		    (py-decref ,func)
		    (raise-python-exception)))
	      (setf (gethash ',name *pyphuns*) ,func)))
	(py-apply ,func ,@args)))))

(defmacro defpyslot (name &optional lisp-name)
  `(defun ,(intern (string-upcase (or lisp-name name))) (obj)
     (pyobject-getattrstring obj ,name)))

(defmacro defpymethod (name &optional lisp-name)
  `(defun ,(intern (string-upcase (or lisp-name name))) (obj &rest args)
     (pyobject-callobject 
      (pyobject-getattrstring obj ,name) 
      (apply #'vector args))))

