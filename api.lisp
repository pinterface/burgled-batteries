(in-package #:burgled-batteries)

(define-symbol-macro main-module*
    (import.add-module* "__main__"))
(define-symbol-macro main-module-dict*
    (module.get-dict* main-module*))

(defun startup-python ()
  (.initialize))

(defun shutdown-python ()
  (.finalize))

(defmacro with-cpython-pointer ((var-or-vars form) &body body)
  (let ((vars (ensure-list var-or-vars)))
    `(multiple-value-bind ,vars ,form
       (unwind-protect
            (progn ,@body)
         ,@(loop :for var :in vars :collect `(.dec-ref ,var))))))

(defun warn-if-uninitialized ()
  (unless (.is-initialized)
    (restart-case
        (error 'simple-error :format-control "The Python interpreter has not yet been initialized." :format-arguments nil)
      (continue ()
        :report "Initialize cpython and continue."
        :test (lambda (c) (declare (ignore c)) (not (.is-initialized)))
        (startup-python)))))

(defun import (name)
  "Imports a Python module into the current namespace.  Should be equivalent
to (run \"import NAME\")."
  (warn-if-uninitialized)
  (with-cpython-pointer (module (import.import* name))
    (object.set-attr-string main-module* (subseq name 0 (position #\. name)) module)))

(defgeneric run* (thing)
  (:documentation "Runs some code.  When given a string, tries to interpret that string as if it were Python code.  Given a pathname, runs that file.  Returns a pointer."))

(defmethod run* :before (thing)
  (warn-if-uninitialized))

(defmethod run* ((code string))
  (with-cpython-pointer
      (code-ptr
       ;; Python's eval doesn't return a value for statements, but only for
       ;; expressions.  So we first attempt to parse code as an expression
       ;; (allowing us to get a value), and only if that fails do we fall back
       ;; to a statement (for which no value will be returned).
       (handler-case
           (.compile-string* code "<string>" :expression)
         (syntax-error () (.compile-string* code "<string>" :statement))))
    (eval.eval-code* code-ptr main-module-dict* main-module-dict*)))

(defmethod run* ((file pathname))
  (error "Sorry, running a Python file is not yet supported."))

;; Rather than duplicate all the defmethods of RUN*, we just call RUN* here and
;; translate the value ourselves.
(defun run (thing)
  "Like RUN*, but makes an effort to return a Lispy value."
  (cffi:convert-from-foreign (run* thing) 'cpython::object!))

(defun apply (func &rest args)
  (warn-if-uninitialized)
  (object.call-object func args))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %get-function (python-name)
    (let* ((pyfunc (run* python-name)))
      (cond
        ((callable.check pyfunc) pyfunc)
        (t (.dec-ref pyfunc)
           (error "The Python function ~S does not appear to exist." python-name))))))

(defun translate-python-name (name)
  (intern (string-upcase name)))

(defun parse-name (name)
  (let* ((names (ensure-list name))
         (python-name (first names))
         (lisp-name (or (second names) (translate-python-name python-name))))
    (values python-name lisp-name)))

;; FIXME: support for optional and keyword arguments undoubtedly needs to be improved
(defmacro defpyfun (names args)
  "Defines a Lisp function which calls a Python function.  If the Python
function has a docstring, that docstring will be used as the Lisp function's
docstring as well.

Note that the Python interpreter must have been started and done any necessary
imports for this macro to expand successfully."
  (warn-if-uninitialized)
  (with-refcnt-barrier
    (multiple-value-bind (python-name lisp-name) (parse-name names)
      (multiple-value-bind (required optional rest keywords) (parse-ordinary-lambda-list args)
        (when (and rest keywords) (error "&rest with &key is not currently supported."))
        (let* ((pyfunc (%get-function python-name))
               (docstring (object.get-attr-string pyfunc "__doc__")))
          (with-unique-names (pyfunc)
            (flet ((make-defun (name call-fn)
                     `(defun ,name ,args
                        ,@(when (stringp docstring) `(,docstring))
                        (with-cpython-pointer (,pyfunc (%get-function ,python-name))
                          (,call-fn ,pyfunc (list* ,@required ,@(mapcar #'first optional) ,@(mapcar #'cadar keywords) ,rest))))))
              `(progn
                 ,(make-defun lisp-name 'object.call-object)
                 ,(make-defun (symbolicate lisp-name '*) 'object.call-object*)
                 ',lisp-name))))))))

;; FIXME: nonsensical for translated values
(defmacro defpyslot (names)
  (warn-if-uninitialized)
  (multiple-value-bind (python-name lisp-name) (parse-name names)
    `(defun ,lisp-name (obj)
       (object.get-attr-string obj ,python-name))))

(defmacro defpymethod (names)
  (warn-if-uninitialized)
  (multiple-value-bind (python-name lisp-name) (parse-name names)
    `(defun ,lisp-name (obj &rest args)
       (with-cpython-pointer (ptr (object.get-attr-string* obj ,python-name))
         (object.call-object ptr args)))))
