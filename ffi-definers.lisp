(in-package #:python.cffi)

;;;; FFI -> Lisp Name Translation
(defun translate-camel-case (name)
  (ensure-symbol
   (with-output-to-string (s)
     (loop :for a :across (subseq name 0)
           :for b :across (subseq name 1)
           :do (cond
                 ((and (not (upper-case-p a))
                       (upper-case-p b))
                  (format s "~C~C" (char-upcase a) #\-))
                 (t (princ (char-upcase a) s))))
     (princ (char-upcase (char name (1- (length name)))) s))
   #.*package*))
(defun translate-python-name (c-name)
  "Translates a Python name such as PyName into a lisp name, by removing the Py
prefix, and converting CamelCase to hyphen-separated.  The first underscore is
converted into a period, and remaining underscores to dashes.  That is,
\"PyInt_New\" gets converted into 'INT.NEW, and \"Py_SomeFunction\" gets turned
into '.SOME-FUNCTION.

Produces an error if the name that would be produced conflicts with a CL symbol
and needs to be shadowed."
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
  "Returns (values python-name lisp-name exportp).  Given a string, assumes the
name should always be exported.  Given a list, each item in the list corresponds
directly to one of the returned values."
  (etypecase names
    (cl:list   (values (first names) (second names) (third names)))
    (cl:string (values names (translate-python-name names) t))))

(defvar *python-docs-page* nil)
(defmacro in-python-docs (page)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf *python-docs-page* ,page)))

(defvar *type-map* (cl:list) "An association list of lisp names for foreign Python types (symbols) to type parsers.")
(defun register-python-type (lisp-name type-parser)
  (if-let ((assoc (assoc lisp-name *type-map*)))
    (setf (cdr assoc) type-parser)
    (push (cons lisp-name type-parser) *type-map*)))

(defun %filter-declarations (decls &key for not-for)
  (build-declarations 'declare (filter-declaration-env (parse-declarations decls)
                                                       :affecting for
                                                       :not-affecting not-for)))

(defun make-defpyfun-helper-forms (wrapped-args wrapped-forms declarations)
  "When called, produces a function which is intended to take the place of
#'CFFI::DEFCFUN-HELPER-FORMS within a CFFI:DEFCFUN expansion.  This is used when
creating an alternate definition for a C function which either doesn't exist in
the currently-included Python library (e.g., because it belongs to a newer
version of Python, or because it's really a preprocessor macro)."
  (lambda (name lisp-name rettype args types options)
    (declare (ignore name lisp-name rettype types options))
    (values '()
            `(let ,(mapcar #'cl:list wrapped-args args)
               ,@(%filter-declarations declarations :for wrapped-args)
               ,@wrapped-forms))))

;;;; Interface for Python API Definitions
(defmacro defpyfun (name return-type args &body options)
  "This is, in essence, a wrapper around CFFI::DEFCFUN.

NAME must be suitable for passing to #'PARSE-PYTHON-NAME.  See the docstring of
that function for more details on the meaning of NAME.

If RETURN-TYPE is a known Python type (that is, it was defined using DEFPYTYPE
or registered via #'REGISTER-PYTHON-TYPE), then this also produces a second
function by the same name but with an asterisk appended (think LET vs. LET*)
which does /not/ do type translation but instead simply returns a pointer to the
PyObject.  (Any applicable error checking is still performed, however.)  The
non-translating versions are particularly useful in the cases where you either
A) specifically want a pointer (e.g., because you're calling PyDict_New and want
a dictionary to populate rather than an empty hashtable) or B) are getting a
large structure back and would prefer not to translate the entire thing for the
one or two values you're actually interested in.

OPTIONS is a list of any, none, or all, of the following forms:
 (:implementation &body) :: &body is an implicit progn which should be used in
   place of a foreign-funcall when the foreign function does not exist.  It
   receives arguments as specified by the arglist.  It is important to note that
   the provided implementation deals with #'TRANSLATE-TO-FOREIGNed types, and
   must produce a value suitable for #'TRANSLATE-FROM-FOREIGN.  To put another
   way, :implementation defines a Lisp version of the C code it replaces.  This
   is useful for usefully defining functions which are really C macros, as well
   as functions which don't exist in later/earlier versions of Python.
 (:requires forms) :: forms should explain under what conditions this function
   is available.  It is used mainly for documentary purposes.
 (:documentation docstring) :: The docstring which should be used for this
   function, if any.
 (:page uri) :: A string representing the page on docs.python.org which contains
   Python's documentation for this function.  Defaults to the value set
   by (in-python-docs page).
 (:if-not-exist &body) :: &body is code which should be executed if the
   specified C function does not exist, an alternate :implementation was not
   specified, and no requirements are known (:requires).  It defaults to
   producing an error.  This is probably most useful as a debugging tool to
   catch functions which are specified in Python's documentation but not
   exported by the Python library (generally, meaning the \"function\" is really
   a preprocessor macro).
"
  (labels ((%make-defcfun (c-name lisp-name return-type args alt-body)
             (let ((docstring (and (stringp (first alt-body)) (first alt-body))))
               `(defcfun (,c-name ,lisp-name) ,return-type
                  ,@(when docstring `(,docstring))
                  ,@args)))
           (%make-cwrapper (internal-name lisp-name args)
             (with-unique-names ((*values-accumulator* accum) retval)
               `(defun ,lisp-name ,(mapcar #'first (remove-if (rcurry #'typep 'return) args :key #'fourth))
                  (let ((,*values-accumulator* (cl:list))
                        (,retval '#:you-should-never-see-this-value))
                    ,(loop :for (value var actual-type parsed-type) :in (cons '(nil nil nil nil) (reverse args))
                           :for body = `(setf ,retval (,internal-name ,@(mapcar #'%choose-symbol args)))
                           :then (expand-to-foreign-dyn value var (cl:list body) parsed-type)
                           :finally (cl:return body))
                    (push ,retval ,*values-accumulator*)
                    (values-list ,*values-accumulator*)))))
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
             "Returns true if TYPE has been registered as a Python type, or is an alias or wrapper thereof, nil otherwise."
             (etypecase type
               (output-arg (%known-python-type-p (real-type type)))
               (cffi::foreign-typedef (or (%known-python-type-p (cffi::name type))
                                          (%known-python-type-p (cffi::actual-type type))))
               (cffi::enhanced-foreign-type (or (%known-python-type-p (cffi::unparsed-type type))
                                                (%known-python-type-p (cffi::actual-type type))))
               (cffi::named-foreign-type nil)
               (cffi::foreign-built-in-type nil)
               (cl:list (%known-python-type-p (car type)))
               (symbol (assoc-value *type-map* type))))
           ;; convert (can-error pyob) and aliases into (can-error :pointer)
           (%translate-type-for-ptr (type)
             "Converts a Python type, or an alias or wrapper thereof, into an equivalent, but non-translating, :pointer type."
             (cond
               ((typep type 'can-error) ; delayed type action
                `(can-error ,(%translate-type-for-ptr (cffi::actual-type type))))
               (t
                (etypecase type
                  (output-arg `(,(first (cffi::unparsed-type type)) ,(%translate-type-for-ptr (real-type type))))
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
             (url  (or (car (assoc-value options :page)) *python-docs-page*))
             (documentation (assoc-value options :documentation))
             (alternate     (assoc-value options :implementation))
             (requires      (assoc-value options :requires))
             (if-not-exist  (or (assoc-value options :if-not-exist)
                                `((error "The C function ~S does not appear to exist." ,name))))
             (normalized-args (mapcar #'normalize-arg args))
             (use-wrapper (some (lambda (a) (and (consp a) (typep (fourth a) 'output-arg))) normalized-args)))
        (when url (setf documentation (cl:list (format nil "~@[~A~%~%~]see http://docs.python.org~A#~A" (car documentation) url name))))
        `(eval-when (:compile-toplevel :load-toplevel :execute)
           ,(cond
              ((and (foreign-symbol-pointer name)
                    use-wrapper)
               (let ((internal-name (symbolicate "%" lisp-name))
                     (args normalized-args))
                 `(progn
                    ,(%make-defcfun name internal-name return-type
                                    (mapcar (lambda (arg) (cl:list (first arg) (third arg))) args)
                                    documentation)
                    ,(%make-cwrapper internal-name lisp-name args)
                    ,(%make-cwrapper internal-name (symbolicate lisp-name "*")
                                     (mapcar (lambda (arg) `(,@(butlast arg)
                                                             ,(if (%known-python-type-p (fourth arg))
                                                                  (parse-type (%translate-type-for-ptr (fourth arg)))
                                                                  (fourth arg))))
                                             args)))))
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

(defmacro defpyvar (c-name &optional lisp-name (cffi-type :pointer))
  "Produces a Lisp variable by the name of LISP-NAME whose value is a pointer to
the foreign variable C-NAME.  Differs from DEFCVAR in that, if C-NAME begins
with an ampersand (#\\&) it does not try to dereference the pointer, and so
works for the inline objects Python uses.  That is, it is the C equivalent of
\"lisp_var = &foreign_var\".  Otherwise, just expands into a DEFCVAR.

Because this is intended for use in our internal machinations, this returns
pointers by default.  However, you can override that by specifying CFFI-TYPE."
  (let* ((deref (char= #\& (char c-name 0)))
         (c-name (subseq c-name (if deref 1 0)))
         (lisp-name (or lisp-name (symbolicate "+" (translate-python-name c-name) "+"))))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       ,(if deref
            `(define-symbol-macro ,lisp-name (foreign-symbol-pointer ,c-name :library 'python-library))
            `(defcvar (,c-name ,lisp-name :read-only t) ,cffi-type))
       (export ',lisp-name))))

;;; Interface for Defining Python Types
(defmacro defpytype (c-name-and-opts &body options)
  "Defines a Python Object type, such as PyObject.  This entails creating a
variable pointing to the \"PyFoo_Type\" type object, the \"PyFoo_Check\" and
\"PyFoo_CheckExact\" functions, as well as appropriate CFFI types for the
object (FOO, FOO!, and FOO?) for use as the types in DEFPYFUN forms.

OPTIONS may consist of any of the following forms:
 (:type lisp-type) :: LISP-TYPE is a Lisp type-specifier, which should cause
   (typep v lisp-type) to return true in the event the given lisp value V should
   be converted into the python type being defined.  If unspecified, automagical
   conversions into PyObjects will not occur (though conversions to the
   more-specific type currently being specified still will).
 (:to (value type) &body) :: Defines a function which translates a Lisp object
   into a Python object.  If unspecified, defaults to performing no translation.
 (:from (value type) &body) :: Defines a function which translates a Python
   object into a Lisp object.  If unspecified, defaults to performing no
   translation (returning a :pointer).
 (:superclass super) :: In the future, this is expected to be the means of
   specifying that this particular Python type has a Python superclass of SUPER,
   to handle the FIXME mentioned in the definition of PyObject.  It currently
   does nothing.

The :to and :from functions are used both when translating the given type
specifically (that is, when the specific type is mentioned in a DEFPYFUN form)
as well as when translating a non-specific PyObject (assuming :type is also
specified).
"
  (let* ((c-name-and-opts (ensure-list c-name-and-opts))
         (c-name (first c-name-and-opts))
         (c-var-name (or (second c-name-and-opts) c-name))
         (lisp-name (translate-python-name c-name))
         (can-error-type (symbolicate lisp-name '#:!))
         (soft-error-type (symbolicate lisp-name '#:?))
         (c-var (format nil "&~A_Type" c-var-name))
         (lisp-var (format-symbol #.*package* "+~A+" (translate-python-name (format nil "~A_Type" c-name))))
         (lisp-type (car (assoc-value options :type)))
         (c-type-check       (translate-python-name (format nil "~A_Check" c-name)))
         (c-type-check-exact (translate-python-name (format nil "~A_CheckExact" c-name)))
         (foreign-type-class (symbolicate '#:foreign-python- lisp-name '#:-type))
         (to   (or (assoc-value options :to)   '((value type) value)))
         (from (or (assoc-value options :from) '((value type) (unless (borrowed-reference-p type) (.inc-ref value)) value))))
    (destructuring-bind ((to-val to-type) &rest to-body) to
      (destructuring-bind ((from-val from-type) &rest from-body) from
        `(eval-when (:compile-toplevel :load-toplevel :execute)
           (define-foreign-type ,foreign-type-class (foreign-python-type) ())
           (defpyvar ,c-var ,lisp-var)
           (defun ,c-type-check       (o)
             (object.type-check o ,lisp-var))
           (defun ,c-type-check-exact (o)
             (%object.type-check-exact o ,lisp-var))
           (define-parse-method ,lisp-name (&rest options)
             (let ((reference-type (or (find :borrowed options) (find :new options) :new))
                   (argument-type (or (find :stolen options) (find :copied options) :copied)))
               (make-instance ',foreign-type-class
                              :actual-type :pointer
                              :borrowedp (ecase reference-type (:new  nil) (:borrowed t))
                              :stolenp   (ecase argument-type  (:stolen t) (:copied nil)))))
           (define-parse-method ,can-error-type (&rest options)
             (parse-type `(can-error (,',lisp-name ,@options))))
           (define-parse-method ,soft-error-type (&rest options)
             (parse-type `(soft-error (,',lisp-name ,@options))))
           (defmethod translate-to-foreign (,to-val (,to-type ,foreign-type-class))
             (let ((reference-type (if (borrowed-reference-p ,to-type) :borrowed :new))
                   (argument-type  (if (stolen-reference-p   ,to-type) :stolen   :copied)))
               (declare (ignorable reference-type argument-type))
               ,@to-body))
           (defmethod translate-from-foreign (,from-val (,from-type ,foreign-type-class))
             (let ((reference-type (if (borrowed-reference-p ,from-type) :borrowed :new))
                   (argument-type  (if (stolen-reference-p   ,from-type) :stolen   :copied)))
               (declare (ignorable reference-type argument-type))
               ,@from-body))
           (defmethod foreign-is-convertable-to-type-p (value (type ,foreign-type-class))
             (declare (ignore type))
             ,(when (assoc-value options :from) `(,c-type-check value)))
           (defmethod lisp-is-convertable-to-foreign-p (value (type ,foreign-type-class))
             (declare (ignorable value) (ignore type))
             ,(when lisp-type `(typep value ',lisp-type)))
           (register-python-type ',lisp-name (find-type-parser ',lisp-name)))))))

(defmacro defpyfun* (lisp-name list-of-pyfun-args &body options)
  "Takes multiple arguments to defpyfun, and expands into a defpyfun for the
first C function which actually exists.  This exists mainly in support of the
PyUnicode functions, which are exported as UCS4 or UCS2 variants depending on
platform and compiler options."
  (loop :for (c-name return-type args) :in list-of-pyfun-args
        :when (foreign-symbol-pointer c-name)
          :do (cl:return `(defpyfun (,c-name ,lisp-name t) ,return-type ,args ,@options))
        :finally (error "Unable to find suitable C definition to create function ~A~%" lisp-name)))

;;;; Translation Helpers for Functions Which Return Error Indicators
(defun %error-occurred-p () (not (null-pointer-p (err.occurred*))))

(define-foreign-type can-error ()
  ((error-values   :initarg :error-values   :accessor error-values   :initform nil)
   (success-values :initarg :success-values :accessor success-values :initform nil)
   (fetchablep :initarg :fetchablep :reader error-is-fetchable-p)))
(define-parse-method can-error (actual-type &key success failure (fetchablep t))
  (make-instance 'can-error
                 :actual-type actual-type
                 :fetchablep fetchablep
                 :success-values (ensure-list success)
                 :error-values (ensure-list failure)))

(define-parse-method soft-error (actual-type &key success failure)
  (parse-type `(can-error ,actual-type :success ,success :failure ,failure :fetchablep t)))

(defmethod print-object ((o can-error) s)
  (print-unreadable-object (o s :type t)
    (format s "~A :fetchablep ~A" (cffi::actual-type o) (error-is-fetchable-p o))))

;; We don't define #'translate-to-foreign because can-error is all about return
;; types.  This may change with call-by-reference support--though even there
;; we'd want a method on #'expand-to-foreign-dyn so we can check for errors
;; after calling into C.

;; We don't define #'translate-from-foreign because #'expand-from-foreign takes
;; precedence, and they'd essentially be duplicates of each other.
(defmethod expand-from-foreign (value (type can-error))
  (once-only ((value value))
    (let ((expand-actual (expand-from-foreign value (cffi::actual-type type)))
          (unfetchable-error `(error 'unfetchable-python-error :value ,value :type ,type)))
      (cond
        ((error-is-fetchable-p type)
         `(if (%error-occurred-p)
              (raise-python-exception)
              ,expand-actual))
        ((success-values type)
         `(if (member ,value ',(success-values type) :test #'equal)
              ,expand-actual
              ,unfetchable-error))
        ((error-values type)
         `(if (member ,value ',(error-values type) :test #'equal)
              ,unfetchable-error
              ,expand-actual))
        (t expand-actual)))))

#+(or) ;; no translate-to-foreign
(defmethod free-translated-object (value (type can-error) param)
  (free-translated-object value (cffi::actual-type type) param))

;;; Some Helper Types
(defctype 0-on-success          (can-error :int :success 0))
(defctype 0-on-success/no-fetch (can-error :int :success 0 :fetchablep nil))
(defctype 0-on-failure          (can-error :int :failure 0))
(defctype boolean! (can-error :boolean :success (0 1)))
(defctype ssize-t! (can-error ssize-t))

;;;; Translation Helpers for Python Types
(define-foreign-type foreign-python-type ()
  ((borrowedp :initarg :borrowedp :reader borrowed-reference-p)
   (stolenp   :initarg :stolenp   :reader stolen-reference-p)))

(defmethod print-object ((o foreign-python-type) s)
  (print-unreadable-object (o s :type t)
    (format s "~A ~A ~A ~A"
            (ignore-errors (cffi::unparsed-type o))
            (if (borrowed-reference-p o) :borrowed :new)
            (if (stolen-reference-p o) :stolen :copied)
            (cffi::actual-type o))))

(defmethod borrowed-reference-p ((object null)) nil)

(defgeneric foreign-is-convertable-to-type-p (value type)
  (:documentation "Returns true if the foreign-type of VALUE has a known conversion under TYPE.")
  (:method (value (type foreign-python-type))
    (declare (ignore value type))
    nil))
(defgeneric lisp-is-convertable-to-foreign-p (value type)
  (:documentation "Returns true if the lisp value VALUE has a known conversion to the foreign-type represented by TYPE.")
  (:method (value (type foreign-python-type))
    (declare (ignore value type))
    nil))

(defmethod translate-to-foreign :around (value (type foreign-python-type))
  (cond
    ;; Raw pointers are already foreign.  Those intentionally dealing with raw
    ;; pointers get to also deal with the joys of reference counting and stolen
    ;; references.  (NOTE: this will cause trouble in :pass-through mode.)
    ((pointerp value) (values value nil))
    ;; fenced-p and wrapped-p are like raw pointers in that their reference
    ;; counts should not be decremented.  Note, however, that because they're
    ;; managed we need to increment their reference counts when the reference is
    ;; stolen to avoid a double-decrement.
    ((fenced-p value)
     (when (stolen-reference-p type) (.inc-ref (fenced-value value)))
     (values (call-next-method) nil))
    ((wrapped-p value)
     (when (stolen-reference-p type) (.inc-ref (wrapped-value value)))
     (values (call-next-method) nil))
    (t (values (call-next-method)
               (not (stolen-reference-p type))))))

(defmethod free-translated-object (value (type foreign-python-type) decrefp)
  (declare (ignore type))
  (when decrefp (.dec-ref value)))

(defmethod translate-from-foreign :around (value (type foreign-python-type))
  (unwind-protect
       (call-next-method)
    (unless (borrowed-reference-p type)
      (.dec-ref value))))

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

;;;; Translation of Unknown Python Types

(defvar *unknown-translation-policy* :discard
  "In the event of a Python type for which no translation is known, we offer several options for how to translate it which a user may switch between at any given time.  This variable controls which method is active, and must be one of '(:discard :barrier :finalize :pass-through).

You should probably not set this variable directly; see instead the WITH-UNKNOWN-TRANSLATION-POLICY macro.")

(defun translate-unknown-pointer (pointer &optional type)
  (ecase *unknown-translation-policy*
    ((:pass-through :pass-thru) pointer)
    (:discard (unless (borrowed-reference-p type) (.dec-ref pointer))
       nil)
    (:barrier  (fence-pointer    pointer type))
    (:finalize (finalize-pointer pointer type))))

(defmacro with-unknown-translation-policy ((policy) &body body)
  "Sets the policy for translation of Python types for which no translation is
known.  Available policies are as follows:
 * :PASS-THROUGH :: Returns the pointer and forces dealing with reference
                    counts (or acceptance of refcnt leaks).
 * :DISCARD      :: Throws away the pointer and substitutes it with NIL.
 * :BARRIER      :: Sets up a barrier, the dynamic-extent of which pointers are
                    not allowed to escape.  Wraps in a FENCED struct.
 * :FINALIZE     :: Enables GC-based disposal.  Wraps in a WRAPPER struct.

Translation policies have dynamic-extent, and may be intermixed freely."
  `(let ((*unknown-translation-policy* ,policy))
     ,@(ecase policy
         ((:pass-through :pass-thru) body)
         (:discard  body)
         (:barrier  (%with-barrier body))
         (:finalize body))))

(defun resignal (condition)
  "Resignals a condition using the appropriate signal function for the condition
type.  Unfortunately, this may differ from the signal function originally used,
but at least we tried."
  (etypecase condition
    (error     (error  condition))
    (warning   (warn   condition))
    (condition (signal condition))))

(defun translate-condition-slots (c)
  (unwind-protect
       (resignal c) ; let handlers have at it before we potentially blow away slots
    (dolist (slot '(exception-type exception-value exception-trace))
      (when (and (slot-boundp c slot)
                 (pointerp (slot-value c slot)))
        (setf (slot-value c slot)
              (translate-unknown-pointer (slot-value c slot)))))))

;;;; refcnt Barrier

(defvar *barrier-finalizers*)

(defstruct (fenced (:constructor fence-value (value)))
  "A wrapper around POINTERs to ensure they are properly invalidated upon exit from a barrier."
  (value (null-pointer) :read-only nil))

;; Ensure nobody else has to care about the translations
(defmethod translate-to-foreign ((value fenced) type)
  (translate-to-foreign (fenced-value value) type))
(defmethod translate-from-foreign ((value fenced) type)
  (translate-from-foreign (fenced-value value) type))
(defmethod free-translated-object ((value fenced) type param)
  (free-translated-object (fenced-value value) type param))

(defun fence-pointer (pointer type)
  (let ((ref (fence-value pointer)))
    (push (lambda ()
            (unless (borrowed-reference-p type)
              (.dec-ref (fenced-value ref)))
            (setf (fenced-value ref) nil))
          *barrier-finalizers*)
    ref))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %with-barrier (body-forms)
    `((let ((*barrier-finalizers* (cl:list)))
        (unwind-protect
             (progn ,@body-forms)
          (mapc #'funcall *barrier-finalizers*))))))

;; Temporary bit for compatibility with prior code
(defmacro with-refcnt-barrier (&body body)
  `(with-unknown-translation-policy (:barrier)
     ,@body))

;;;; Finalizers

(defstruct (wrapped (:constructor wrap-value (value)))
  "A wrapper around POINTERs to ensure they are properly GCed."
  (value (null-pointer) :read-only t))

(defmethod translate-to-foreign ((value wrapped) type)
  (translate-to-foreign (wrapped-value value) type))
(defmethod translate-from-foreign ((value wrapped) type)
  (translate-from-foreign (wrapped-value value) type))
(defmethod free-translated-object ((value wrapped) type param)
  (free-translated-object (wrapped-value value) type param))

(defun finalize-pointer (pointer &optional type)
  (cond
    ((null-pointer-p pointer) pointer)
    (t (.inc-ref pointer)
       (tg:finalize (wrap-value pointer)
                    (lambda ()
                      (.dec-ref pointer)
                      (unless (borrowed-reference-p type)
                        (.dec-ref pointer)))))))

;;; FFI -> Lisp Error Translation
(define-condition python-condition ()
  ((exception-type :initarg :type)
   (exception-value :initarg :value)
   (exception-trace :initarg :trace))
  (:documentation "The base condition type for all conditions dealing with the Python interpreter."))
(define-condition python-warning (python-condition warning) ()
  (:documentation "The base condition type for all Python-issued warnings."))
(define-condition python-error (python-condition error) ()
  (:documentation "The base condition type for all Python-issued errors."))

(define-condition unfetched-python-error (python-error)
  ((returned-value :initarg :value)
   (cffi-type :initarg :type))
  (:documentation "The error used when a Python error occurred, but could not be fetched.  This condition occurring, rather than UNFETCHABLE-PYTHON-ERROR, is almost certainly a bug."))
(define-condition unfetchable-python-error (unfetched-python-error) ()
  (:documentation "The error used when a Python error is unfetchable (e.g., for PyRun_SimpleString)."))
(define-condition early-python-error (python-error) ()
  (:documentation "The error used when a Python error is fetchable, but a more specific error cannot be determined.  This might occur during loading, when the initial Python condition relationships are being built up."))

;;; Python Exception -> Lisp Condition Mappings
(defparameter *exception-map* (make-hash-table :test 'equal)
  "A mapping from Python exceptions to Lisp conditions.")
(defun register-exception-map (python-exception signal-fn initializer-fn)
  "Registers two functions to a Python exception:
* SIGNAL-FN signals a Lisp condition for PYTHON-EXCEPTION.
* INITIALIZER-FN initializes the slots of a condition for later signalling by
  SIGNAL-FN.  The initializer function is responsible for calling the
  initializer functions of any Python superclasses."
  (setf (gethash python-exception *exception-map*) (cons signal-fn initializer-fn)))
(defun get-exception-signaller (type)
  (flet ((%getsig (name) (car (gethash name *exception-map*))))
    (or (%getsig (%object.get-attr-string type "__name__"))
        (dolist (s (%get-python-inheritance-tree type))
          (cond
            ((string= s "object") nil)
            ((%getsig s) (cl:return (%getsig s)))))
        (lambda (e v b) (make-condition 'early-python-error :type e :value v :trace b)))))
(defun get-exception-initializer (python-exception)
  (cdr (gethash python-exception *exception-map*)))

(defun raise-python-exception ()
  (let ((exception (err.occurred*)))
    (when (null-pointer-p exception) (error 'unfetched-python-error))
    (multiple-value-bind (type value traceback)
        (err.fetch-normalized*) ; fetch automatically clears the error
      (handler-bind ((python-condition #'translate-condition-slots))
        (funcall (get-exception-signaller type) type value traceback)))))

(defun err.fetch-normalized* ()
  (multiple-value-bind (& type value traceback)
      (err.fetch*)
    (declare (ignore &))
    (multiple-value-bind (& type value traceback)
        (err.normalize-exception* type value traceback)
      (declare (ignore &))
      (values type value traceback))))

(defun %foreign-symbol-value (symbol type)
  (mem-ref (cffi::fs-pointer-or-lose symbol :default) type))

(defun %object.get-attr-string (o string)
  (when (object.has-attr-string o string)
    (object.get-attr-string o string)))

(defun %get-python-superclasses (python-class)
  (with-unknown-translation-policy (:barrier)
    (map 'cl:list
         (lambda (o) (%object.get-attr-string o "__name__"))
         (%object.get-attr-string python-class "__bases__"))))

(defun %get-python-inheritance-tree (type)
  "Returns a list of class names in the order they should be consulted according to Python's class heirarchy rules."
  (with-unknown-translation-policy (:barrier)
    (flet ((name (type) (%object.get-attr-string type "__name__")))
      (let ((tree (%type.mro type)))
        ;; first item in tree is given type, last is object
        (mapcar #'name (butlast (rest tree)))))))

(defun %ensure-function (form)
  (cond
    ((functionp (first form))
     (first form))
    ((member (caar form) '(lambda cl:function))
     (first form))
    (t
     `(lambda ,(first form) ,@(rest form)))))

(defmacro defpyexception (python-name (&rest lisp-superclasses) (&rest slots) &body options)
  (let* ((lisp-condition-name (translate-camel-case python-name))
         (c-name (format nil "PyExc_~A" python-name))
         (c-ptr (%foreign-symbol-value c-name :pointer))
         (lisp-var-name (symbolicate "+" (translate-python-name c-name) "+"))
         (python-superclasses (remove "object" (%get-python-superclasses c-ptr) :test #'string=))
         (slot-specs (mapcar (lambda (slot-spec)
                               (destructuring-bind (names &rest keys) slot-spec
                                 (multiple-value-bind (lisp-slot python-attribute)
                                     (cffi::parse-name-and-options names)
                                   (declare (ignore python-attribute))
                                   `(,lisp-slot ,@keys))))
                             slots))
         (attribute-specs (mapcar (lambda (slot-spec)
                                    (destructuring-bind (names &rest keys) slot-spec
                                      (multiple-value-bind (lisp-slot python-attribute)
                                          (cffi::parse-name-and-options names)
                                        (declare (ignore lisp-slot))
                                        `(,python-attribute ,(cadr (member :initarg keys))))))
                                  slots))
         (docstring (or (first (assoc-value options :documentation))
                        (%object.get-attr-string c-ptr "__doc__")))
         (initializer-fn (or (assoc-value options :initializer)
                             `((c e v b)
                               (declare (ignorable e b))
                               ,@(mapcar (lambda (super) `(funcall (get-exception-initializer ,super) c e v b))
                                         python-superclasses)
                               ,@(mapcar (lambda (slot attr) `(setf (slot-value c ',(first slot)) (%object.get-attr-string v ,(first attr))))
                                         slot-specs
                                         attribute-specs))))
         (signal-fn (or (assoc-value options :converter)
                        `((e v b)
                          (let ((condition (make-condition ',lisp-condition-name
                                                           :type e :value v :trace b)))
                            (funcall (get-exception-initializer ,python-name) condition e v b)
                            (error condition))))))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       ,(unless (boundp lisp-var-name) `(defpyvar ,c-name)) ; Unless is TEMPORARY.
       (define-condition ,lisp-condition-name
           (,@(mapcar #'translate-camel-case python-superclasses)
            ,@(or lisp-superclasses '(python-condition)))
         ,slot-specs
         (:documentation ,docstring))
       (register-exception-map ,python-name
                               ,(%ensure-function signal-fn)
                               ,(%ensure-function initializer-fn))
       (export ',lisp-condition-name)
       ',lisp-condition-name)))

;;;; C structs
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *c-structs* (make-hash-table))
  (defparameter *delayed-cstruct-code* nil))

(defun untranslated-fields (fields)
  (loop :for (name untranslated-type translated-type) :in fields
        :collect `(,(if translated-type (symbolicate name "*") name)
                   ,untranslated-type)))

(defun translated-fields (fields)
  (loop :for (name untranslated-type translated-type) :in fields
        :when translated-type
          :collect `(,name ,(if (assoc-value *type-map* translated-type)
                              `(,translated-type :borrowed)
                              translated-type))))

(defmacro defcstruct* (name-and-options (&optional parent) &body fields)
  "Define the layout of a foreign structure."
  (cffi::discard-docstring fields)
  (destructuring-bind (name . options)
      (ensure-list name-and-options)
    (let* ((untranslated-fields (untranslated-fields fields))
           (translated-fields (translated-fields fields))
           (struct-fields (append (gethash parent *c-structs*) untranslated-fields))
           (accessor-fields (append untranslated-fields translated-fields))
           (conc-name (symbolicate name ".")))
      (setf (gethash name *c-structs*) struct-fields)
      `(progn
         (eval-when (:compile-toplevel :load-toplevel :execute)
           (cffi::notice-foreign-struct-definition ',name ',options ',struct-fields)
           ,@(loop :for (field type) :in accessor-fields
                   :for accessor = (symbolicate conc-name field)
                   :collect `(declaim (inline ,accessor (setf ,accessor))))
           ,@(cffi::generate-struct-accessors name conc-name
                                              (mapcar #'car untranslated-fields)))
         ;; CFFI expects types to exist during compilation.  Because they might
         ;; be forward-referenced, we push the translating slots and accessors
         ;; into FINALIZE-CSTRUCTS, which should be called after all the types
         ;; are defined.
         (eval-when (:compile-toplevel :load-toplevel)
           (appendf *delayed-cstruct-code*
                    '((let ((ptype (parse-type ',name)))
                        (loop :for (field type) :in (translated-fields ',fields)
                              :for offset = (foreign-slot-offset ',name (symbolicate field "*"))
                              :do (setf (gethash field (cffi::slots ptype))
                                        (cffi::make-struct-slot field offset type 1))))
                      ,@(cffi::generate-struct-accessors name conc-name (mapcar #'car translated-fields)))))
         ',name))))

(defmacro finalize-cstructs ()
  (prog1 `(eval-when (:compile-toplevel :load-toplevel :execute)
            ,@(or *delayed-cstruct-code*
                  `((cerror "No delayed code.  Something has gone wrong?"))))
    (setf *delayed-cstruct-code* nil)))
