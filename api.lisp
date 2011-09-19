(in-package #:pyffi)

(defvar *py-main-module*)
(defvar *py-main-module-dict*)
(defvar *pyphuns* (make-hash-table))

(defun startup-python ()
  (.initialize)
  (setf *py-main-module* (import.add-module "__main__"))
  (setf *py-main-module-dict* (module.getdict/as-ptr *py-main-module*))
  (let ((tmp (run.string "from __builtin__ import *" +single-input+
                         *py-main-module-dict* (cffi:null-pointer))))
    (.dec-ref tmp)))

(defun shutdown-python ()
  (setf *py-main-module* nil *py-main-module-dict* nil)
  (clrhash *pyphuns*)
  (.finalize))

(defun import (name)
  (let ((p (position #\. name)))
    (let ((m (import.import-module-ex name *py-main-module-dict* *py-main-module-dict* (cffi:null-pointer))))
      (unwind-protect
	   (object.set-attr-string *py-main-module* (if p (subseq name 0 p) name) m)
	(.dec-ref m)))))

(defun eval (expression)
  (run.string expression +eval-input+ *py-main-module-dict* *py-main-module-dict*))

(defun apply (func &rest args)
  (object.call-object func (cl:apply #'vector args)))

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
	      (setf ,func (eval ,form))
	      (if (not (callable.check ,func))
		  (progn
		    (.dec-ref ,func)
		    (python.cffi::raise-python-exception)))
	      (setf (gethash ',name *pyphuns*) ,func)))
	(apply ,func ,@args)))))

(defmacro defpyslot (name &optional lisp-name)
  `(defun ,(intern (string-upcase (or lisp-name name))) (obj)
     (object.get-attr-string obj ,name)))

(defmacro defpymethod (name &optional lisp-name)
  `(defun ,(intern (string-upcase (or lisp-name name))) (obj &rest args)
     (object.call-object
      (object.get-attr-string obj ,name)
      (apply #'vector args))))
