(in-package #:python.cffi)

(define-foreign-type output-arg (cffi::enhanced-foreign-type)
  ((real-type :initarg :real-type :reader real-type)))

(define-foreign-type return (output-arg) ()
  (:documentation "A RETURN foreign-type is like Clisp's :OUT."))
(define-parse-method return (real-type)
  (make-instance 'return :actual-type :pointer :real-type (parse-type real-type)))

(define-foreign-type place (output-arg) ()
  (:documentation "A PLACE foreign-type is like Clisp's :IN-OUT."))
(define-parse-method place (real-type)
  (make-instance 'place :actual-type :pointer :real-type (parse-type real-type)))

(defgeneric include-in-argument-list-p (foreign-type)
  (:documentation "Returns true if the given foreign-type should be included in the argument list of a function, false if it should be excluded.")
  (:method ((foreign-type return)) nil)
  (:method ((foreign-type t)) t))

(defvar *values-accumulator*)

(defmethod expand-to-foreign-dyn (value var body (type output-arg))
  (let* ((real-type (real-type type))
         (canonical-type (cffi::canonicalize real-type)))
    `(with-foreign-object (,var :pointer)
       ,@body
       (push ,(expand-from-foreign `(mem-ref ,var ,canonical-type) real-type) ,*values-accumulator*))))

(defmethod expand-to-foreign-dyn (value var body (type place))
  (let* ((real-type (real-type type))
         (canonical-type (cffi::canonicalize real-type)))
    (expand-to-foreign-dyn
     value var
     `(,(call-next-method var value `((setf (mem-ref ,value ,canonical-type) ,var) ,@body) type))
     real-type)))

(defun normalize-arg (arg)
  "Returns (list var gensym actual-type parsed-type)"
  (cond
    ((eq arg '&rest) arg)
    ((listp arg)
     (destructuring-bind (var type) arg
       (let ((parsed-type (parse-type type)))
         (cond
           ((typep parsed-type 'output-arg) (cl:list var (gensym (symbol-name var)) :pointer parsed-type))
           (t (cl:list var (gensym (symbol-name var)) type parsed-type))))))
    (t (error "oh noez!"))))

(defun %choose-symbol (arg)
  (typecase (fourth arg)
    (return (second arg))
    (place (first arg))
    (t (second arg))))

#+(or) ; Just an example
(defmacro defcfun* (name return-type args)
  (multiple-value-bind (lisp-name foreign-name options)
      (cffi::parse-name-and-options name)
    (let* ((internal-lisp-name (symbolicate "%" lisp-name))
           (args (mapcar #'normalize-arg args))
           (*values-accumulator* (gensym "ACCUM"))
           (retval (gensym "RETVAL"))
           (lisp-args (mapcar #'first (remove-if-not #'include-in-argument-list-p args :key #'fourth))))
      `(progn
         (defcfun (,foreign-name ,internal-lisp-name ,@options)
             ,return-type
           ,@(mapcar (lambda (arg) (cl:list (first arg) (third arg))) args))
         (defun ,lisp-name ,lisp-args
           (let ((,*values-accumulator* (cl:list))
                 (,retval '#:you-should-never-see-this-value))
             ,(loop :for (value var actual-type parsed-type) :in (cons '(nil nil nil nil) (reverse args))
                    :for body = `(setf ,retval (,internal-lisp-name ,@(mapcar #'%choose-symbol args)))
                    :then (expand-to-foreign-dyn value var (cl:list body) parsed-type)
                    :finally (cl:return body))
             (push ,retval ,*values-accumulator*)
             (values-list ,*values-accumulator*)))))))

#+(or) (defcfun* "foo" :void ((a :int) (b (return :boolean)) (c (place :boolean))))
