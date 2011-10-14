(in-package #:burgled-batteries)

(defvar *py-main-module*)
(defvar *py-main-module-dict*)

(defun startup-python ()
  (.initialize)
  (setf *py-main-module* (import.add-module "__main__"))
  (setf *py-main-module-dict* (module.get-dict* *py-main-module*))
  (let ((tmp (run.string* "from __builtin__ import *" :statement
                          *py-main-module-dict* (cffi:null-pointer))))
    (.dec-ref tmp)))

(defun shutdown-python ()
  (setf *py-main-module* nil *py-main-module-dict* nil)
  (.finalize))

(defun import (name)
  (let ((p (position #\. name)))
    (let ((m (import.import-module-ex name *py-main-module-dict* *py-main-module-dict* (cffi:null-pointer))))
      (unwind-protect
	   (object.set-attr-string *py-main-module* (if p (subseq name 0 p) name) m)
	(.dec-ref m)))))

(defun eval (expression)
  "Evaluates a Python expression and returns a Lisp object (or a pointer, if no
method of translation is known)."
  (run.string expression :expression *py-main-module-dict* *py-main-module-dict*))
(defun eval* (expression)
  "Like EVAL, but always returns a pointer to a Python object."
  (run.string* expression :expression *py-main-module-dict* *py-main-module-dict*))

(defgeneric run* (thing)
  (:documentation "Runs some code.  When given a string, tries to interpret that string as if it were Python code.  Given a pathname, runs that file.  Returns a pointer."))

(defmethod run* ((code string))
  (let ((code-ptr
          ;; Python's eval doesn't return a value for statements, but only for
          ;; expressions.  So we first attempt to parse code as an expression
          ;; (allowing us to get a value), and only if that fails do we fall
          ;; back to a statement (for which no value will be returned).
          (handler-case
              (.compile-string code "<string>" :expression)
            (syntax-error () (.compile-string code "<string>" :statement)))))
    (eval.eval-code* code-ptr *py-main-module-dict* *py-main-module-dict*)))

(defmethod run* ((file pathname))
  (error "Sorry, running a Python file is not yet supported."))

;; Rather than duplicate all the defmethods of RUN*, we just call RUN* here and
;; translate the value ourselves.
(defun run (thing)
  "Like RUN*, but makes an effort to return a Lispy value."
  (let* ((val (run* thing))
         (ret (multiple-value-list (cffi:convert-from-foreign val '(python.cffi::can-error python.cffi::object)))))
    (unless (cffi:pointerp (first ret)) (.dec-ref val))
    (values-list ret)))

(defun apply (func &rest args)
  (object.call-object func (cl:apply #'vector args)))

(defmacro with-decrements (pointer-vars &body body)
  `(unwind-protect
        (progn ,@body)
     (progn
       ,@(mapcar (lambda (x) `(.dec-ref ,x)) pointer-vars))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %get-function (python-name)
    (let* ((pyfunc (eval* python-name)))
      (cond
        ((callable.check pyfunc) pyfunc)
        (t (.dec-ref pyfunc)
           (error "The Python function ~S does not appear to exist." python-name))))))

;; FIXME: support for optional and keyword arguments undoubtedly needs to be improved
(defmacro defpyfun (names args)
  "Defines a Lisp function which calls a Python function.  If the Python
function has a docstring, that docstring will be used as the Lisp function's
docstring as well.

Note that the Python interpreter must have been started and done any necessary
imports for this macro to expand successfully."
  (let* ((names (ensure-list names))
         (python-name (first names))
         (lisp-name (or (second names) (intern (string-upcase python-name))))
         (pyfunc (%get-function python-name)))
    (with-decrements (pyfunc)
      (multiple-value-bind (required optional rest keywords) (parse-ordinary-lambda-list args)
        (declare (ignore rest))
        (let* ((docstring (object.get-attr-string pyfunc "__doc__")))
          (with-unique-names (pyfunc)
            `(defun ,lisp-name ,args
               ,@(when (stringp docstring) `(,docstring))
               (let ((,pyfunc (%get-function ,python-name)))
                 (with-decrements (,pyfunc)
                   (object.call-object ,pyfunc (vector ,@required ,@(mapcar #'first optional) ,@(mapcar #'cadar keywords))))))))))))

;; FIXME: nonsensical for translated values
(defmacro defpyslot (name &optional lisp-name)
  `(defun ,(intern (string-upcase (or lisp-name name))) (obj)
     (object.get-attr-string obj ,name)))

(defmacro defpymethod (name &optional lisp-name)
  `(defun ,(intern (string-upcase (or lisp-name name))) (obj &rest args)
     (object.call-object
      (object.get-attr-string obj ,name)
      (apply #'vector args))))
