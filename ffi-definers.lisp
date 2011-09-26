(in-package #:python.cffi)

;;;; FFI -> Lisp Name Translation
(defun translate-python-name (c-name)
  (let* ((start (cond
                  ((string= "_Py" c-name :end2 3) 3)
                  ((string= "Py"  c-name :end2 2) 2)
                  (t (error "Not a Python name."))))
         (sym (ensure-symbol
               (with-output-to-string (s)
                 (loop :with seen-underscore = nil
                       :for a :across (subseq c-name start)
                       :for b :across (subseq c-name (1+ start))
                       :do (cond
                             ((and (char= a #\_) (not seen-underscore)) (princ #\. s) (setf seen-underscore t))
                             ((char= a #\_) (princ #\- s))
                             ((and (not (upper-case-p a))
                                   (upper-case-p b))
                              (format s "~C~C" (char-upcase a) #\-))
                             (t (princ (char-upcase a) s))))
                 (princ (char-upcase (char c-name (1- (length c-name)))) s))
               #.*package*)))
    (if (eql (find-package '#:cl) (symbol-package sym))
        (error "The python name ~S produces a symbol ~A in the CL package!"
               c-name sym)
        sym)))

(defun parse-python-name (names)
  (etypecase names
    (cl:list   (values (first names) (second names) (third names)))
    (cl:string (values names (translate-python-name names) t))))

(defvar *type-map* (make-hash-table))
(defun register-python-type (python-type lisp-type)
  (setf (gethash python-type *type-map*) lisp-type))

(defun %filter-declarations (decls &key for not-for)
  (build-declarations 'declare (filter-declaration-env (parse-declarations decls)
                                                       :affecting for
                                                       :not-affecting not-for)))

(defun make-defpyfun-helper-forms (wrapped-args wrapped-forms declarations)
  (lambda (name lisp-name rettype args types options)
    (declare (ignore name lisp-name rettype types options))
    (values '()
            `(let ,(mapcar #'cl:list wrapped-args args)
               ,@(%filter-declarations declarations :for wrapped-args)
               ,@wrapped-forms))))

;;;; Interface for Python API Definitions
(defmacro defpyfun (name return-type args &body options)
  (labels ((%make-defcfun (c-name lisp-name return-type args alt-body)
             (let ((docstring (and (stringp (first alt-body)) (first alt-body))))
               `(defcfun (,c-name ,lisp-name) ,return-type
                  ,@(when docstring `(,docstring))
                  ,@args)))
           (%make-altfun (c-name lisp-name return-type c-args alt-body)
             (multiple-value-bind (alt-body decl doc)
                 (parse-body alt-body :documentation t)
               (let* ((realfn (symbol-function 'cffi::defcfun-helper-forms))
                      (lisp-args (mapcar (lambda (x) (if (consp x) (first x) x)) c-args))
                      (alt-body `(,@(when (and (length= 1 alt-body) (keywordp (first alt-body)))
                                      `((declare (ignorable ,@lisp-args))))
                                  ,@alt-body))
                      (ourfn (make-defpyfun-helper-forms lisp-args alt-body decl))
                      (doc+decl `(,@(ensure-list doc) ,@(%filter-declarations decl :not-for lisp-args))))
                 ;; We temporarily replace cffi::defcfun-helper-forms and call
                 ;; cffi::%defcfun to create the defun so we can ensure the body
                 ;; of our alternate function runs having been given pointers as
                 ;; if it were C.
                 (unwind-protect
                      (progn
                        (setf (symbol-function 'cffi::defcfun-helper-forms) ourfn)
                        (cffi::%defcfun lisp-name c-name return-type c-args nil doc+decl))
                   (setf (symbol-function 'cffi::defcfun-helper-forms) realfn)))))
           ;; Ugh.  But necessary to work with type aliases for when we
           ;; shorten things (e.g., always-error).
           (%known-python-type-p (type)
             (etypecase type
               (cffi::foreign-typedef (or (%known-python-type-p (cffi::name type))
                                          (%known-python-type-p (cffi::actual-type type))))
               (cffi::enhanced-foreign-type (or (%known-python-type-p (cffi::unparsed-type type))
                                                (%known-python-type-p (cffi::actual-type type))))
               (cffi::named-foreign-type nil)
               (cffi::foreign-built-in-type nil)
               (cl:list (%known-python-type-p (car type)))
               (symbol (gethash type *type-map*))))
           ;; convert (can-error pyob) and aliases into (can-error :pointer)
           (%translate-type-for-ptr (type)
             (cond
               ((typep type 'can-error) ; delayed type action
                `(can-error ,(%translate-type-for-ptr (cffi::actual-type type))))
               (t
                (etypecase type
                  (cffi::foreign-typedef (%translate-type-for-ptr (cffi::actual-type type)))
                  (cffi::enhanced-foreign-type
                   (let ((utype (cffi::unparsed-type type)))
                     (or (%translate-type-for-ptr utype)
                         (mapcar (lambda (x) (or (%translate-type-for-ptr x) x)) utype))))
                  (cl:list (when (%known-python-type-p (car type)) :pointer))
                  (symbol  (when (%known-python-type-p type) :pointer)))))))
    (multiple-value-bind (name lisp-name exportp) (parse-python-name name)
      (let* ((parsed-type (parse-type return-type))
             (ptr-name (when (%known-python-type-p parsed-type)
                                 (format-symbol (symbol-package lisp-name) "~A*" lisp-name)))
             (ptr-type (when ptr-name (%translate-type-for-ptr parsed-type)))
             (lisp-args (mapcar (lambda (x) (if (consp x) (first x) x)) args))
             (documentation (assoc-value options :documentation))
             (alternate     (assoc-value options :implementation))
             (requires      (assoc-value options :requires))
             (if-not-exist  (or (assoc-value options :if-not-exist)
                                `((error "The C function ~S does not appear to exist." ,name)))))
        `(eval-when (:compile-toplevel :load-toplevel :execute)
           ,(cond
              ((foreign-symbol-pointer name)
               `(progn
                  ,(%make-defcfun name lisp-name return-type args documentation)
                  ,(when ptr-name (%make-defcfun name ptr-name ptr-type args documentation))))
              (alternate
               `(progn
                  ,(%make-altfun name lisp-name return-type args `(,@documentation ,@alternate))
                  ,(when ptr-name (%make-altfun name ptr-name ptr-type args `(,@documentation ,@alternate)))))
              (requires
               (let ((alternate `((error "The C function ~S is only available under the following conditions:~%~{* ~A~%~}"
                                         ,name ',requires))))
                 `(progn
                    (defun ,lisp-name ,lisp-args ,@documentation (declare (ignore ,@lisp-args)) ,@alternate)
                    ,(when ptr-name `(defun ,ptr-name ,lisp-args ,@documentation (declare (ignore ,@lisp-args)) ,@alternate)))))
              (if-not-exist `(progn ,@if-not-exist))
              (t nil))
           ,(when exportp `(export ',lisp-name))
           ,(when ptr-name `(export ',ptr-name))
           ',lisp-name)))))

(defmacro defpyvar (c-name &optional (lisp-name (format-symbol #.*package* "+~A+" (translate-python-name c-name))))
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (defparameter ,lisp-name (foreign-symbol-pointer ,c-name))
     #+pyffi.debug (format t "Var ~A is ~A~%" ,c-name ,lisp-name)
     (export ',lisp-name)))

;;; Interface for Defining Python Types
(defmacro defpytype (c-name &body options)
  (let* ((lisp-name (translate-python-name c-name))
         (can-error-type (symbolicate lisp-name '#:!))
         (c-var (format nil "~A_Type" c-name))
         (lisp-var (format-symbol #.*package* "+~A+" (translate-python-name c-var)))
         (lisp-type (car (assoc-value options :type)))
         (c-type-check       (translate-python-name (format nil "~A_Check" c-name)))
         (c-type-check-exact (translate-python-name (format nil "~A_CheckExact" c-name)))
         (to   (or (assoc-value options :to)   '((value type) value)))
         (from (or (assoc-value options :from) '((value type) value)))
         (errorp (car (assoc-value options :errorp))))
    (destructuring-bind ((to-val to-type) &rest to-body) to
      (destructuring-bind ((from-val from-type) &rest from-body) from
        `(eval-when (:compile-toplevel :load-toplevel :execute)
           (defparameter ,lisp-var (foreign-symbol-pointer ,c-var))
           (defun ,c-type-check       (o)
             #+pyffi.debug (format t "In ~A: ~A~%" ',c-type-check o)
             (object.type-check o ,lisp-var))
           (defun ,c-type-check-exact (o)
             #+pyffi.debug (format t "In ~A: ~A~%" ',c-type-check-exact o)
             (%object.type-check-exact o ,lisp-var))
           (define-parse-method ,lisp-name (&rest options)
             (let ((reference-type (or (find :borrowed options) (find :new options) :new))
                   (argument-type (or (find :stolen options) (find :copied options) :copied)))
               (make-instance 'foreign-python-type
                              :actual-type :pointer
                              :to   #'(lambda (,to-val ,to-type)
                                        (declare (ignorable ,to-type) (optimize (debug 3)))
                                        #+pyffi.debug (format t "In translate-to for ~A: ~A, ~A~%" ',lisp-name ,to-val ,to-type)
                                        ,@to-body)
                              :from #'(lambda (,from-val ,from-type)
                                        (declare (ignorable ,to-type) (optimize (debug 3)))
                                        #+pyffi.debug (format t "In translate-from for ~A: ~A, ~A~%" ',lisp-name ,from-val ,from-type)
                                        ,@from-body)
                              :borrowedp (ecase reference-type (:new  nil) (:borrowed t))
                              :stolenp   (ecase argument-type  (:stolen t) (:copied nil))
                              :check-ptr  #',c-type-check
                              :check-lisp #'(lambda (v)
                                              (declare (ignorable v))
                                              #+pyffi.debug (format t "In check-lisp for ~A: ~A~%" ',lisp-name v)
                                              ,(when lisp-type `(typep v ',lisp-type))))))
           (define-parse-method ,can-error-type (&optional (reference-type :new))
             (parse-type `(can-error (,',lisp-name ,reference-type))))
           (register-python-type ',lisp-name (find-type-parser ',lisp-name))
           ,@(when errorp `((register-error-checker ',lisp-name ,errorp))))))))

(defmacro defpyfun* (lisp-name list-of-pyfun-args &body options)
  "Takes multiple arguments to defpyfun, and expands into a defpyfun for the
  first C function which actually exists."
  (loop :for (c-name return-type args) :in list-of-pyfun-args
        :when (foreign-symbol-pointer c-name)
          :do (return `(defpyfun (,c-name ,lisp-name t) ,return-type ,args ,@options))
        :finally (error "Unable to find suitable C definition to create function ~A~%" lisp-name)))

;;;; Translation Helpers for Functions Which Return Error Indicators
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *error-checkers* (make-hash-table))
  (defun register-error-checker (symbol function)
    (setf (gethash symbol *error-checkers*) function))
  (defun get-error-checker (symbol)
    (gethash symbol *error-checkers*)))
(defun %error-occurred-p () (not (null-pointer-p (err.occurred*))))

;; FIXME: In some cases the return value may-or-may-not indicate an error and we
;;        have to check (err.occurred*).  Since (err.occurred*) will always be
;;        non-null after an error (with a few exceptions where the error isn't
;;        fetchable at all), it might make more sense just to always check it
;;        and not bother with the return values of functions.
(define-foreign-type can-error ()
  ((error-value-p :initarg :error-value-p :accessor error-value-p)
   (fetchablep :initarg :fetchablep :reader error-is-fetchable-p)))
(define-parse-method can-error (actual-type &key success failure (fetchablep t))
  (let ((checkerr (get-error-checker (first (ensure-list actual-type)))))
    (make-instance 'can-error
                   :actual-type actual-type
                   :fetchablep fetchablep
                   :error-value-p (cond
                                    (success (complement (rcurry #'member (ensure-list success) :test #'equal)))
                                    (failure (rcurry #'member (ensure-list failure) :test #'equal))
                                    (checkerr checkerr)
                                    (t (constantly nil))))))

(define-parse-method soft-error (actual-type &key success failure)
  (let* ((type (parse-type `(can-error ,actual-type :success ,success :failure ,failure :fetchablep t)))
         (ev (error-value-p type)))
    (setf (error-value-p type)
          (lambda (v) (and (funcall ev v) (%error-occurred-p))))
    type))

(defmethod print-object ((o can-error) s)
  (print-unreadable-object (o s :type t)
    (format s "~A :fetchablep ~A" (cffi::actual-type o) (error-is-fetchable-p o))))

;; We don't define #'translate-to-foreign because can-error is all about return
;; types.  This may change with call-by-reference support--though even there
;; we'd want a method on #'expand-to-foreign-dyn so we can check for errors
;; after calling into C.

(defmethod translate-from-foreign (value (type can-error))
  (if (funcall (error-value-p type) value)
      (if #+(or) (error-is-fetchable-p type) (null-pointer-p (err.occurred*))
          (error "Unfetchable error in Python.")
          (raise-python-exception))
      (translate-from-foreign value (cffi::actual-type type))))

#+(or) ;; no translate-to-foreign
(defmethod free-translated-object (value (type can-error) param)
  (free-translated-object value (cffi::actual-type type) param))

;;; Some Helper Types
(dolist (x '(:int :long :long-long size-t ssize-t))
  (register-error-checker x (curry #'= -1)))
(dolist (x '(:uint :ulong :unsigned-long-long))
  (register-error-checker x (lambda (v) (declare (ignore v)) (%error-occurred-p))))
(register-error-checker :double (curry #'= -1.0))
(defctype 0-on-success          (can-error :int :success 0))
(defctype 0-on-success/no-fetch (can-error :int :success 0 :fetchablep nil)) ; May not be necessary
(defctype boolean! (can-error :boolean :success (0 1)))
(defctype ssize-t! (can-error ssize-t))
(register-error-checker :pointer #'null-pointer-p)
(register-error-checker :string  #'null-pointer-p)

;;;; Translation Helpers for Python Types
(define-foreign-type foreign-python-type ()
  ((borrowedp :initarg :borrowedp :reader borrowed-reference-p)
   (stolenp   :initarg :stolenp   :reader stolen-reference-p)
   (translate-to   :initarg :to)
   (translate-from :initarg :from)
   (foreign-is-type :initarg :check-ptr)
   (lisp-is-type    :initarg :check-lisp)))

(defmethod print-object ((o foreign-python-type) s)
  (print-unreadable-object (o s :type t)
    (format s "~A ~A ~A ~A"
            (ignore-errors (cffi::unparsed-type o))
            (if (borrowed-reference-p o) :borrowed :new)
            (if (stolen-reference-p o) :stolen :copied)
            (cffi::actual-type o))))

(defmethod translate-to-foreign (value (type foreign-python-type))
  (cond
    ((pointerp value) (values value nil)) ; assume already foreign
    (t (values (funcall (slot-value type 'translate-to) value type)
               (not (stolen-reference-p type))))))

(defmethod free-translated-object (value (type foreign-python-type) decrefp)
  (declare (ignore type))
  #+pyffi.debug (format t "In free-translated-object: ~A, ~A, ~A~%" value decrefp (%object.refcnt value))
  (when decrefp (.dec-ref value)))

(defmethod translate-from-foreign (value (type foreign-python-type))
  (unwind-protect
       (funcall (slot-value type 'translate-from) value type)
    (progn
      #+pyffi.debug (format t "In translate-from-foreign: ~A, ~A~%" value (%object.refcnt value))
      (unless (borrowed-reference-p type)
        (.dec-ref value)))))

;;;; Translation for Octet Arrays
(define-foreign-type octet-array () ())

(define-parse-method octet-array ()
  (make-instance 'octet-array :actual-type :pointer))

(defmethod translate-to-foreign (value (type octet-array))
  (cond
    ((pointerp value) (values value nil))
    (t (values
        (let ((ptr (foreign-alloc :uchar :count (length value))))
          (dotimes (i (length value) ptr)
            (setf (mem-aref ptr :uchar i) (aref value i))))
        (length value)))))

(defmethod free-translated-object (value (type octet-array) length)
  (when length (foreign-free value)))

(defmethod translate-from-foreign (value (type octet-array))
  (declare (special *byte-array.size*))
  (let ((array (make-array *byte-array.size* :element-type '(unsigned-byte 8))))
    (dotimes (i *byte-array.size* array)
      (setf (aref array i) (mem-aref value :uchar i)))))

;;; FFI -> Lisp Error Translation
(define-condition python-error ()
  ((code :initarg :exc :reader exception))
  (:report (lambda (c s)
             (format s "Python error: ~a" (exception c)))))

(defun raise-python-exception ()
  (let* ((exc (err.occurred*))
         (desc (object.str exc)))
    (unwind-protect
         (error 'python-error :exc desc)
      (err.clear))))

;; TODO: Create a defpyexception/defpyerr macro which allows us to convert
;;       between Python exceptions and Lisp conditions.
