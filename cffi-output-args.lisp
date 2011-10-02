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

(defun %wrap-arg (arg-list body accum)
  (destructuring-bind (val var real-type parsed-type)
      (first arg-list)
    (declare (ignore real-type))
    (let* ((to-wrap (if (rest arg-list)
                        (%wrap-arg (rest arg-list) body accum)
                        body))
           (real-type (ignore-errors (real-type parsed-type)))
           (canonical-type (when real-type (cffi::canonicalize real-type)))
           #+(or) (translate-out `(push ,(expand-from-foreign `(mem-ref ,var ,canonical-type) real-type) ,accum)))
      (typecase parsed-type
        (return `((with-foreign-object (,var :pointer)
                    ,@to-wrap
                    (push ,(expand-from-foreign `(mem-ref ,var ,canonical-type) real-type) ,accum))))
        (place `(,(expand-to-foreign-dyn val var
                                         `((with-foreign-object (,val :pointer)
                                             (setf (mem-ref ,val ,canonical-type) ,var)
                                             ,@to-wrap
                                             (push ,(expand-from-foreign `(mem-ref ,val ,canonical-type) real-type) ,accum)))
                                         real-type)))
        (t `(,(expand-to-foreign-dyn val var to-wrap parsed-type)))))))

(defun normalize-arg (arg)
  "Returns (list var real-type parsed-type)"
  (cond
    ((eq arg '&rest) arg)
    ((listp arg)
     (destructuring-bind (var type) arg
       (let ((parsed-type (parse-type type)))
         (cond
           ((typep parsed-type 'output-arg) (cl:list var (gensym (symbol-name var)) :pointer parsed-type))
           (t (cl:list var (gensym (symbol-name var)) type parsed-type))))))
    (t (error "oh noez!"))))

(defun normalize-args (args)
  (mapcar #'normalize-arg args))

(defun %choose-symbol (arg)
  (typecase (fourth arg)
    (return (second arg))
    (place (first arg))
    (t (second arg))))

#+(or)
(defmacro define-c-function (name return-type args)
  (multiple-value-bind (lisp-name foreign-name options)
      (cffi::parse-name-and-options name)
    (let* ((internal-lisp-name (symbolicate "%" lisp-name))
           (args (normalize-args args))
           (accum (gensym "ACCUM"))
           (retval (gensym "RETVAL"))
           #+(or) (ptr-name (symbolicate lisp-name "*"))
           (lisp-args (mapcar #'first (remove-if (rcurry #'typep 'return) args :key #'fourth))))
      `(progn
         (defcfun (,foreign-name ,internal-lisp-name ,@options)
             ,return-type
           ,@(mapcar (lambda (arg) (cl:list (first arg) (third arg))) args))
         (defun ,lisp-name ,lisp-args
           (let ((,accum (cl:list))
                 (,retval '#:you-should-never-see-this-value))
             ,@(%wrap-arg args ;(remove-if-not (rcurry #'typep 'output-arg) args :key #'fourth)
                          `((setf ,retval (,internal-lisp-name ,@(mapcar #'%choose-symbol args))))
                          accum)
             (push ,retval ,accum)
             (values-list ,accum)))
         #+(or)
         (defun ,ptr-name ,lisp-args
           (let ((,accum (cl:list))
                 (,retval '#:you-should-never-see-this-value))
             ,(%wrap-arg (remove-if-not (rcurry #'typep 'output-arg) args :key #'third)
                         `(setf ,retval (,internal-lisp-name ,@(mapcar #'first args)))
                         accum)
             (push ,retval ,accum)
             (values-list ,accum)))))))

#+(or) (define-c-function "foo" :void ((a :int) (b (return :boolean)) (c (place object))))
