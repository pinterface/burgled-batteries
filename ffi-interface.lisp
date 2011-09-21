(in-package #:python.cffi)

(declaim (optimize (debug 3)))

;;;; FFI Library
;; Much of what we do below requires it be loaded during macroexpansion time.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-foreign-library python-library
    (:darwin (:framework "Python"))
    (:unix (:or "libpython2.7.so.1.0" "libpython2.6.so.1.0" "libpython2.5.so.1.0" "libpython2.4.so.1.0" "libpython2.3.so.1.0"))
    (:windows (:or "python27.dll" "python26.dll" "python25.dll" "python24.dll" "python23.dll"))
    (t (:default "libpython")))
  (use-foreign-library python-library))

;;;; FFI -> Lisp Name Translation
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun translate-python-name (c-name)
    (unless (string= "Py" c-name :end2 2)
      (error "Not a Python name."))
    (let ((sym (ensure-symbol
                (with-output-to-string (s)
                  (loop :for a :across (subseq c-name 2)
                        :for b :across (subseq c-name 3)
                        :do (cond
                              ((char= a #\_) (princ #\. s))
                              ((and (not (upper-case-p a))
                                    (upper-case-p b))
                               (format s "~C~C" (char-upcase a) #\-))
                              (t (princ (char-upcase a) s))))
                  (princ (char-upcase (char c-name (1- (length c-name)))) s))
                #.*package*)))
      (if (eql (find-package '#:cl) (symbol-package sym))
          (error "The python name ~S produces a symbol ~A in the CL package!"
                 c-name sym)
          sym))))

;;;; Basic Handling of the PyObject struct
(defcstruct %object
  ;; #ifdef Py_TRACE_REFS
  #+pyffi.trace-refs (-ob-next object)
  #+pyffi.trace-refs (-ob-prev object)
  ;; #endif
  (refcnt ssize-t)
  (type :pointer))

(defun %object.refcnt (o) (and (pointerp o) (not (null-pointer-p o)) (foreign-slot-value o '%object 'refcnt)))
(defun %object.type (o) (foreign-slot-value o '%object 'type))
(defun %object.type-check-exact (o type) (pointer-eq (%object.type o) type))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *type-map* (make-hash-table))
  (defun register-python-type (python-type lisp-type)
    (setf (gethash python-type *type-map*) lisp-type)))

;;;; Interface for Python API Definitions
(defmacro defpyfun (name return-type args &body alternate)
  (labels ((%make-defcfun (c-name lisp-name return-type args alt-body)
             (let ((docstring (and (stringp (first alt-body)) (first alt-body))))
               `(defcfun (,c-name ,lisp-name) ,return-type
                  ,@(when docstring `(,docstring))
                  ,@args)))
           (%make-altfun (lisp-name c-args body)
             (let ((l-args (mapcar (lambda (x) (if (consp x) (first x) x)) c-args)))
               (multiple-value-bind (body decls) (parse-body body)
                 `(defun ,lisp-name ,l-args
                    ;; ignore args for :requires-<foo> things
                    ,@(when (and (length= 1 body) (keywordp (first body)))
                        `((declare (ignorable ,@l-args))))
                    ,@decls
                    #+pyffi.debug (format t "In altfn ~A: ~A~%" ',lisp-name ,(caar args))
                    ,@body))))
           (%known-python-type-p (return-type)
             (etypecase return-type
               (cl:list (if (eql (first return-type) 'can-error)
                            (%known-python-type-p (second return-type))
                            (gethash (car return-type) *type-map*)))
               (symbol (gethash return-type *type-map*)))))
    (let* ((translating-lisp-name (translate-python-name name))
           (ptring-lisp-name (when (%known-python-type-p return-type)
                               (format-symbol (symbol-package translating-lisp-name) "~A*" translating-lisp-name))))
      `(eval-when (:compile-toplevel :load-toplevel :execute)
         ,(cond
           ((foreign-symbol-pointer name)
            `(progn
               ,(%make-defcfun name translating-lisp-name return-type args alternate)
               ,(when ptring-lisp-name (%make-defcfun name ptring-lisp-name :pointer args alternate))))
           (alternate
            `(progn
               ,(%make-altfun translating-lisp-name args alternate)
               ,(when ptring-lisp-name (%make-altfun ptring-lisp-name args alternate))))
           (t (error "The C function ~S does not appear to exist." name)))
         (export ',translating-lisp-name)
         ,(when ptring-lisp-name `(export ',ptring-lisp-name))
         ',translating-lisp-name))))

(defmacro defpyvar (c-name &optional (lisp-name (format-symbol #.*package* "+~A+" (translate-python-name c-name))))
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (defparameter ,lisp-name (foreign-symbol-pointer ,c-name))
     #+pyffi.debug (format t "Var ~A is ~A~%" ,c-name ,lisp-name)
     (export ',lisp-name)))

;;; Interface for Defining Python Types
(defmacro defpytype (c-name &body options)
  (let* ((lisp-name (translate-python-name c-name))
         (c-var (format nil "~A_Type" c-name))
         (lisp-var (format-symbol #.*package* "+~A+" (translate-python-name c-var)))
         (lisp-type (car (assoc-value options :type)))
         (c-type-check       (translate-python-name (format nil "~A_Check" c-name)))
         (c-type-check-exact (translate-python-name (format nil "~A_CheckExact" c-name)))
         (to   (assoc-value options :to))
         (from (assoc-value options :from))
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
           (define-parse-method ,lisp-name (&optional (reference-type :new))
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
                            :borrowedp (ecase reference-type
                                         (:new nil)
                                         (:borrowed t))
                            :check-ptr #',c-type-check
                            :check-lisp #'(lambda (v)
                                            (declare (ignorable v))
                                            #+pyffi.debug (format t "In check-lisp for ~A: ~A~%" ',lisp-name v)
                                            ,(when lisp-type `(typep v ',lisp-type)))))
           (register-python-type ',lisp-name (find-type-parser ',lisp-name))
           ,@(when errorp `((register-error-checker ',lisp-name ,errorp))))))))

;;;; Functions Related to Embedding CPython
(defpyfun "Py_Initialize"   :void ()
  "Initialize the Python interpreter.  This, or .INITIALIZE-EX, must be called before Python may be used.")
(defpyfun "Py_InitializeEx" :void ((initsigs :int))
  "Like .INITIALIZE if INITSIGS is 1.  If INITSIGS is 0, skips registration of signal handlers.")
(defpyfun "Py_IsInitialized" :boolean ()
  "Returns true if the Python interpreter has been initialize, false otherwise.")

(defpyfun "Py_Finalize" :void ())

(defpyfun "Py_SetProgramName" :void ((name :string)))
(defpyfun "Py_GetProgramName" :string ())
(defpyfun "Py_GetPrefix" :string ())
(defpyfun "Py_GetExecPrefix" :string ())
(defpyfun "Py_GetProgramFullPath" :string ())
(defpyfun "Py_GetPath" :string ()) ;; FIXME?: Split on #\: or #\; and return a list?
(defpyfun "Py_GetVersion" :string ())
(defpyfun "Py_GetPlatform" :string ())
(defpyfun "Py_GetCopyright" :string ())
(defpyfun "Py_GetCompiler" :string ())
(defpyfun "Py_GetBuildInfo" :string ())
#+(or) ;; CFFI doesn't really support the <char **> type.
(defpyfun "PySys_SetArgvEx" :void ((argc :int) (argv (:array :string)) (updatepath :int))
  (sys.set-argv argc argv))
#+(or) ;; CFFI doesn't really support the <char **> type.
(defpyfun "PySys_SetArgv" :void ((argc :int) (argv (:array :string))))
(defpyfun "Py_SetPythonHome" :void ((home :string)))
(defpyfun "Py_GetPythonHome" :string ())

;;;; Translation Helpers for Functions Which Return Error Indicators
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *error-checkers* (make-hash-table))
  (defun register-error-checker (symbol function)
    (setf (gethash symbol *error-checkers*) function))
  (defun get-error-checker (symbol)
    (gethash symbol *error-checkers*)))

(define-foreign-type can-error ()
  ((error-value-p :initarg :error-value-p :reader error-value-p)
   (fetchablep :initarg :fetchablep :reader error-is-fetchable-p)))
(define-parse-method can-error (actual-type &key success failure (fetchablep t))
  (let ((checkerr (get-error-checker (first (ensure-list actual-type)))))
    (make-instance 'can-error
                   :actual-type actual-type
                   :fetchablep fetchablep
                   :error-value-p (cond
                                    (success (complement (curry #'equal success)))
                                    (failure (curry #'equal failure))
                                    (checkerr checkerr)
                                    (t (constantly nil))))))

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
(defctype 0-on-success          (can-error :int :success 0))
(defctype 0-on-success/no-fetch (can-error :int :success 0 :fetchablep nil)) ;; May not be necessary
(register-error-checker :pointer #'null-pointer-p)

;;;; Translation Helpers for Python Types
(define-foreign-type foreign-python-type ()
  ((borrowedp :initarg :borrowedp :reader borrowed-reference-p)
   (translate-to   :initarg :to)
   (translate-from :initarg :from)
   (foreign-is-type :initarg :check-ptr)
   (lisp-is-type    :initarg :check-lisp)))

(defmethod print-object ((o foreign-python-type) s)
  (print-unreadable-object (o s :type t)
    (format s "~A ~A ~A"
            (ignore-errors (cffi::unparsed-type o))
            (if (borrowed-reference-p o) :borrowed :new)
            (cffi::actual-type o))))

(defmethod translate-to-foreign (value (type foreign-python-type))
  (cond
    ((pointerp value) (values value nil)) ; assume already foreign
    (t (values (funcall (slot-value type 'translate-to) value type) t))))

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

;;;; Definitions Relating to Python Types

;;; PyObject is the root of all other types
(defpytype "PyObject"
  (:errorp #'null-pointer-p)
  (:to (value type)
    (loop :for v :being :the :hash-values :of *type-map*
          :for foreign-type := (funcall v reference-type)
          :when (funcall (slot-value foreign-type 'lisp-is-type) value)
            :do (return (translate-to-foreign value foreign-type))
          :finally (return value)))
  (:from (value type)
    ;(if (null-pointer-p value) (raise-python-exception))
    (unless (null-pointer-p value)
      ;; NECESSARY!  If we return the pointer, the later .DEC-REF in tff will
      ;; be too early; if we convert to a subtype of PyObject, that conversion
      ;; will also trigger a .DEC-REF, taking us to zero before we're ready.
      (unless (borrowed-reference-p type) (.inc-ref value))
      (loop :for v :being :the :hash-values :of *type-map*
            :for foreign-type := (funcall v reference-type)
            :when (funcall (slot-value foreign-type 'foreign-is-type) value)
              :do (return (translate-from-foreign value foreign-type))
            :finally (return value)))))

(defpytype "PyType"
  (:to   (value type) value)
  (:from (value type) value))

;; Docs say use Py_None, but _Py_NoneStruct is what's exported
(defpyvar "_Py_NoneStruct" +None+)
(defun %none.check (o) (pointer-eq +None+ o))

;;; Numeric Types
(defpytype "PyInt"
  (:type integer)
  (:to   (value type) (int.from-long* value))
  (:from (value type) (int.as-long value)))

(defpyvar "_Py_ZeroStruct" +False+)
(defpyvar "_Py_TrueStruct" +True+)
(defpytype "PyBool"
  (:type boolean)
  (:to (value type) (if value +True+ +False+))
  (:from (value type)
    (cond
      ((pointer-eq +True+ value) t)
      ((pointer-eq +False+ value) nil)
      (t (error "Not a boolean!")))))

#+TODO-implement-functions
(defpytype "PyLong"
  (:to (value type) (long.from-long* value))
  (:from (value type) (long.as-long value)))

(defpytype "PyFloat"
  (:type cl:float)
  (:to   (value type) (float.from-double* value))
  (:from (value type) (float.as-double   value)))

;; FIXME: Actually using %complex will require FSBV integration (coming soon to a CFFI near you!)
(defcstruct %complex (real :double) (imag :double))
#+TODO-implement-functions
(defpytype "PyComplex"
  (:type cl:complex)
  (:to (value type)
    (complex.from-doubles* (realpart value) (imagpart value)))
  (:from (value type)
    (cl:complex (complex.real-as-double value) (complex.imag-as-double value))))

;;; Sequence Types
#+TODO-implement-functions
(defpytype "PyByteArray"
  (:type (array (unsigned-byte 8)))
  (:to (value type) (byte-array.from-string-and-size* value (length value)))
  (:from (value type) (byte-array.as-string value)))

(defpytype "PyString"
  (:type cl:string)
  (:to   (value type) (string.from-string* value))
  (:from (value type) (string.as-string    value)))

#+(or) ;; FIXME: we'll need the bytes.whatever functions
(defpytype "PyBytes"
  (:to   (value type) (bytes.from-string* value))
  (:from (value type) (bytes.as-string    value)))

;; Shorthand types for unicode strings
(defctype utf8-string (:string :encoding :utf-8))
(defctype ucs4-string (:string :encoding #+little-endian :ucs-4/le #+big-endian :ucs-4/be))
(defctype ucs2-string (:string :encoding #+little-endian :ucs-2/le #+big-endian :ucs-2/be))

;; FIXME: unicode.<whatever> has to alias to unicode-ucs[24].<whatever>, because
;;        the -ucs versions are what actually get exported.
(defpytype "PyUnicode"
  (:type cl:string)
  (:to   (value type) (unicode.from-unicode* value (length value)))
  (:from (value type) (unicode.as-unicode value)))

#+(or) ;; Skipping old-style buffers
(defpytype "PyBuffer"
  (:to (value type) …)
  (:from (value type) …))

(defpytype "PyTuple"
  ;; CAUTION: tuple of characters may get confused with strings
  (:type (and vector (not cl:string)))
  (:to (value type)
    #+(or) ;; loop doesn't save any lines here, does it?
    (loop :with tup = (tuple.new* (length value))
          :for i :from 0 :to (length value)
          :do (tup.set-item tup i (svref value i))
          :finally (return tup))
    (let* ((len (length value))
           (tup (tuple.new* len)))
      (dotimes (i len tup)
        (tuple.set-item tup i (svref value i)))))
  (:from (value type)
    (let* ((len (tuple.size value))
           (tup (make-array len)))
      (dotimes (i len tup)
        (setf (svref tup i) (tuple.get-item value i))))))

(defpytype "PyList"
  (:type cl:list)
  (:to (value type)
    (loop :with list = (list.new* (length value))
          :for item :in value :and i :from 0
          :do (list.set-item list i item))
    #+(or) ;; slow (because of nth)
    (let* ((len (length value))
           (lst (list.new len)))
      (dotimes (i len lst)
        (list.set-item lst i (nth i value)))))
  (:from (value type)
    (loop :for i :from 0 :to (1- (list.size value))
          :collect (list.get-item value i))))

;;; Mapping Types
(defpytype "PyDict"
  (:type hash-table)
  (:to (value type)
    (let ((dict (dict.new*)))
      (maphash (lambda (k v) (dict.set-item dict k v)) value)
      dict))
  (:from (value type)
    ;; Probably premature optimization, but the below avoids the translation
    ;; machinery for the bits we'd just GC anyway
    (let* ((dict (make-hash-table :test #'equal))
           (items (dict.items* value)))
      (unwind-protect
           (dotimes (i (list.size items) dict)
             (let* ((item (list.get-item* items i))
                    (key (tuple.get-item item 0))
                    (value (tuple.get-item item 1)))
               (when (string= key "bozo_exception") (format t "BOZO!  ~A~%" (object.str value)))
               (setf (gethash key dict) value)))
        (.dec-ref items)))))

;;; TODO: Other Objects (Class, Instance, Function, Method, File, Module, Iterator, Descriptor, ...)


;;;; API Functions Relating to Threads, Interpreter State, and Debugging

;;; Threads
(defpyfun "PyEval_InitThreads" :void () :requires-python-threads)
(defpyfun "PyEval_ThreadsInitialized" :boolean ())
(defctype thread-state :pointer)
(defpyfun "PyEval_SaveThread" thread-state ())
(defpyfun "PyEval_RestoreThread" :void ((tstate thread-state)))
(defpyfun "PyThreadState_Get" thread-state ())
(defpyfun "PyThreadState_Swap" thread-state ((tstate thread-state)))
(defpyfun "PyEval_ReInitThreads" :void ())
(defcenum (gilstate.state :int) :locked :unlocked)
(defpyfun "PyGILState_Ensure" gilstate.state ())
(defpyfun "PyGILState_Release" :void ((state gilstate.state)))
(defpyfun "PyGILState_GetThisThreadState" thread-state ())

(defmacro with-thread-state (&body body)
  "The Lispy equivalent of surrounding code with Py_BEGIN_ALLOW_THREADS ... Py_END_ALLOW_THREADS."
  (with-unique-names (thread-state)
    `(let ((,thread-state (eval.save-thread)))
       (unwind-protect (progn ,@body)
         (eval.restore-thread ,thread-state)))))

(defctype interpreter-state :pointer)
(defpyfun "PyInterpreterState_New" interpreter-state () :requires-python-threads)
(defpyfun "PyInterpreterState_Clear" :void ((interp interpreter-state)) :requires-python-threads)
(defpyfun "PyInterpreterState_Delete" :void ((interp interpreter-state)) :requires-python-threads)
(defpyfun "PyThreadState_New" thread-state ((interp interpreter-state)) :requires-python-threads)
(defpyfun "PyThreadState_Clear" :void ((tstate thread-state)) :requires-python-threads)
(defpyfun "PyThreadState_Delete" :void ((tstate thread-state)) :requires-python-threads)
(defpyfun "PyThreadState_GetDict" (dict :borrowed) () :requires-python-threads)
(defpyfun "PyThreadState_SetAsyncExc" :int ((id :long) (exc object)) :requires-python-threads)
(defpyfun "PyEval_AcquireThread" :void ((tstate thread-state)) :requires-python-threads)
(defpyfun "PyEval_ReleaseThread" :void ((tstate thread-state)) :requires-python-threads)
(defpyfun "PyEval_AcquireLock" :void () :requires-python-threads)
(defpyfun "PyEval_ReleaseLock" :void () :requires-python-threads)

;;; Sub-Interpreters
(defpyfun "Py_NewInterpreter" thread-state ())
(defpyfun "Py_EndInterpreter" :void ((tstate thread-state)))

;;; Async Notifications
;; TODO: Add define-notification-callback macro like define-trace-function below?
(defpyfun "Py_AddPendingCall" (can-error :int :success 0) ((callback :pointer) (arg :pointer))
  (error "Sorry, .ADD-PENDING-CALL requires Python 2.7 or newer."))

;;; Profiling and Tracing
(defctype frame-object :pointer)
(defctype .tracefunc :pointer)
;; FIXME: should we call this define-trace-callback instead?  define-trace-function-callback?
(defmacro define-trace-function (name (object frame what arg) &body body)
  `(defcallback ,name :int ((,object object) (,frame frame-object) (,what :int) (,arg object))
     ,@body))

(defpyfun "PyEval_SetProfile" :void ((func .tracefunc) (obj object)))
(defpyfun "PyEval_SetTrace" :void ((func .tracefunc) (obj object)))
(defpyfun "PyEval_GetCallStats" object ((self object)) :requires-python-call-profile)
(defconstant +pcall-all+             0) ; Are these really necessary, or should
(defconstant +pcall-function+        1) ; we instead split out the tuple into
(defconstant +pcall-fast-function+   2) ; something like an alist or a
(defconstant +pcall-faster-function+ 3) ; call-stats struct?
(defconstant +pcall-method+          4)
(defconstant +pcall-bound-method+    5)
(defconstant +pcall-cfunction+       6)
(defconstant +pcall-type+            7)
(defconstant +pcall-generator+       8)
(defconstant +pcall-other+           9)
(defconstant +pcall-pop+            10)

;;; Advanced Debugger Support
(defpyfun "PyInterpreterState_Head" interpreter-state ())
(defpyfun "PyInterpreterState_Next" interpreter-state ((interp interpreter-state)))
(defpyfun "PyInterpreterState_ThreadHead" thread-state ((interp interpreter-state)))
(defpyfun "PyThreadState_Next" thread-state ((tstate thread-state)))

;;;; High-level Interpreter Functions
(defcstruct compiler-flags (flags :int))
#+requires-CHAR*-ARRAY-support (defpyfun "Py_Main" :int ((argc :int) (argv (:array :string))))
#+requires-FILE*-support (defpyfun "PyRun_AnyFile" :int ((fp :file) (filename :string)))
#+requires-FILE*-support (defpyfun "PyRun_AnyFileFlags" :int ((fp :file) (filename :string) (flags compiler-flags)))
#+requires-FILE*-support (defpyfun "PyRun_AnyFileEx" :int ((fp :file) (filename :string) (closeit :int)))
#+requires-FILE*-support (defpyfun "PyRun_AnyFileExFlags" :int ((fp :file) (filename :string) (closeit :int) (flags compiler-flags)))
(defpyfun "PyRun_SimpleString" 0-on-success/no-fetch ((command :string)))
(defpyfun "PyRun_SimpleStringFlags" 0-on-success/no-fetch ((command :string) (flags compiler-flags)))
#+requires-FILE*-support (defpyfun "PyRun_SimpleFile" 0-on-success/no-fetch ((fp :file) (filename :string)))
#+requires-FILE*-support (defpyfun "PyRun_SimpleFileFlags" 0-on-success/no-fetch ((fp :file) (filename :string) (flags compiler-flags)))
#+requires-FILE*-support (defpyfun "PyRun_SimpleFileEx" 0-on-success/no-fetch ((fp :file) (filename :string) (closeit :int)))
#+requires-FILE*-support (defpyfun "PyRun_SimpleFileExFlags" 0-on-success/no-fetch ((fp :file) (filename :string) (closeit :int) (flags compiler-flags)))
;; FIXME: The Python docs leave me uncertain as to whether these are /no-fetch
#+requires-FILE*-support (defpyfun "PyRun_InteractiveOne" 0-on-success ((fp :file) (filename :string)))
#+requires-FILE*-support (defpyfun "PyRun_InteractiveOneFlags" 0-on-success ((fp :file) (filename :string) (flags compiler-flags)))
;; FIXME: Can these produce an error code, or do they always return 0?
#+requires-FILE*-support (defpyfun "PyRun_InteractiveLoop" :int ((fp :file) (filename :string))) ; canerr?
#+requires-FILE*-support (defpyfun "PyRun_InteractiveLoopFlags" :int ((fp :file) (filename :string) (flags compiler-flags))) ; canerr?

(defpyfun "PyParser_SimpleParseString" :pointer ((str :string) (start :int)))
(defpyfun "PyParser_SimpleParseStringFlags" :pointer ((str :string) (start :int) (flags :int)))
(defpyfun "PyParser_SimpleParseStringFlagsFilename" :pointer ((str :string) (filename :string) (start :int) (flags :int)))
#+requires-FILE*-support (defpyfun "PyParser_SimpleParseFile" :pointer ((fp :file) (filename :string) (start :int)))
#+requires-FILE*-support (defpyfun "PyParser_SimpleParseFileFlags" :pointer ((fp :file) (filename :string) (start :int) (flags :int)))

(defpyfun "PyRun_String" (can-error object) ((str :string) (start :int) (globals dict) (locals dict)))
(defpyfun "PyRun_StringFlags" (can-error object) ((str :string) (start :int) (globals dict) (locals dict) (flags compiler-flags)))
#+requires-FILE*-support (defpyfun "PyRun_File" (can-error object) ((fp :file) (filename :string) (start :int) (globals object) (locals object)))
#+requires-FILE*-support (defpyfun "PyRun_FileEx" (can-error object) ((fp :file) (filename :string) (start :int) (globals object) (locals object) (closeit :int)))
#+requires-FILE*-support (defpyfun "PyRun_FileFlags" (can-error object) ((fp :file) (filename :string) (start :int) (globals object) (locals object) (flags compiler-flags)))
#+requires-FILE*-support (defpyfun "PyRun_FileExFlags" (can-error object) ((fp :file) (filename :string) (start :int) (globals object) (locals object) (closeit :int) (flags compiler-flags)))

(defctype code-object :pointer)
(defpyfun "Py_CompileString" (can-error code-object) ((str :string) (filename :string) (start :int)))
(defpyfun "Py_CompileStringFlags" (can-error code-object) ((str :string) (filename :string) (start :int) (flags compiler-flags)))
(defpyfun "PyEval_EvalCode" (can-error object) ((co code-object) (globals object) (locals object)))
#+requires-POINTER-ARRAY-support (defpyfun "PyEval_EvalCodeEx" (can-error object) ((co code-object) (globals object) (locals object) (args (:array object)) (argcount :int) (kws (:array object)) (kwcount :int) (defs (:array object)) (defcount :int) (closure object)))
(defpyfun "PyEval_EvalFrame" (can-error object) ((f frame-object)))
(defpyfun "PyEval_EvalFrameEx" (can-error object) ((f frame-object) (throwflag :int)))
(defpyfun "PyEval_MergeCompilerFlags" :boolean ((cf compiler-flags)))

;;;; Memory Management
;; These take pointers, rather than objects, because converting an object to
;; pass into these functions would be nonsensical.
(defpyfun "Py_IncRef" :void ((o :pointer)))
(defpyfun "Py_DecRef" :void ((o :pointer)))

;;;; Error Handling

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

;;; Python Error API (most of these which return a value are canerr, I think)
(defpyfun "PyErr_PrintEx" :void ((set-sys-last-vars :int)))
(defpyfun "PyErr_Print" :void ())
(defpyfun "PyErr_Occurred" (object :borrowed) ())
(defpyfun "PyErr_ExceptionMatches" :int ((exc object)))
(defpyfun "PyErr_GivenExceptionMatches" :int ((given object) (exc object)))
#+requires-call-by-reference-support (defpyfun "PyErr_NormalizeException" :void ((exc (:ref object)) (val (:ref object)) (tb (:ref object))))
(defpyfun "PyErr_Clear" :void ())
#+requires-call-by-reference-support (defpyfun "PyErr_Fetch" :void ((ptype (:ref object)) (pvalue (:ref object)) (ptraceback (:ref object))))
(defpyfun "PyErr_Restore" :void ((type object) (value object) (traceback object)))
(defpyfun "PyErr_SetString" :void ((type object) (message :string)))
(defpyfun "PyErr_SetObject" :void ((type object) (value object)))
(defpyfun "PyErr_Format" (can-error object) ((exception object) (format :string) &rest)) ; more like always-errors! :)
(defpyfun "PyErr_SetNone" :void ((type object)))
(defpyfun "PyErr_BadArgument" :int ())
(defpyfun "PyErr_NoMemory" object ())
(defpyfun "PyErr_SetFromErrno" object ((type object)))
(defpyfun "PyErr_SetFromErrnoWithFilename" object ((type object) (filename :string)))
(defpyfun "PyErr_SetFromWindowsErr" object ((ierr :int)) :requires-windows)
(defpyfun "PyErr_SetExcFromWindowsErr" object ((type object) (ierr :int)) :requires-windows)
(defpyfun "PyErr_SetFromWindowsErrWithFilename" object ((ierr :int) (filename :string)) :requires-windows)
(defpyfun "PyErr_SetExcFromWindowsErrWithFilename" object ((type object) (ierr :int) (filename :string)) :requires-windows)
(defpyfun "PyErr_BadInternalCall" :void ())
(defpyfun "PyErr_WarnEx" :int ((category object) (message :string) (stacklevel :int)))
(defpyfun "PyErr_Warn" :int ((category object) (message :string)))
(defpyfun "PyErr_WarnExplicit" :int ((category object) (message :string) (filename :string) (lineno :int) (module :string) (registry object)))
(defcvar ("Py_Py3kWarningFlag" *err.warn-py3k*) :boolean)
(defpyvar "PyExc_DeprecationWarning")
(defpyfun "PyErr_WarnPy3k" :int ((message :string) (stacklevel :int))
  (if *err.warn-py3k* (err.warn-ex +exc.deprecation-warning+ message stacklevel) 0))
(defpyfun "PyErr_CheckSignals" :int ())
(defpyfun "PyErr_SetInterrupt" :void ())
(defpyfun "PySignal_SetWakeupFd" :int ((fd :int)) :requires-python-2.6)
(defpyfun "PyErr_NewException" object ((name :string) (base object) (dict object)))
(defpyfun "PyErr_NewExceptionWithDoc" object ((name :string) (doc :string) (base object) (dict object))
  (declare (ignore doc))
  (err.new-exception name base dict))
(defpyfun "PyErr_WriteUnraisable" :void ((obj object)))

;;; Unicode Exceptions
(defpyfun "PyUnicodeDecodeError_Create" object ((encoding :string) (object :string) (length ssize-t) (start ssize-t) (end ssize-t) (reason :string)))
(defpyfun "PyUnicodeEncodeError_Create" object ((encoding :string) (object unicode) (length ssize-t) (start ssize-t) (end ssize-t) (reason :string)))
(defpyfun "PyUnicodeTranslateError_Create" object ((object unicode) (length ssize-t) (start ssize-t) (end ssize-t) (reason :string)))
(defpyfun "PyUnicodeDecodeError_GetEncoding" object ((exc object)))
(defpyfun "PyUnicodeEncodeError_GetEncoding" object ((exc object)))
(defpyfun "PyUnicodeDecodeError_GetObject" object ((exc object)))
(defpyfun "PyUnicodeEncodeError_GetObject" object ((exc object)))
(defpyfun "PyUnicodeTranslateError_GetObject" object ((exc object)))
;; below are canerr
#+(or) ;; how to handle pointer to size?
(defpyfun "PyUnicodeDecodeError_GetStart" :int ((exc object) (start (:pointer ssize-t))))
#+(or) ;; how to handle pointer to size?
(defpyfun "PyUnicodeEncodeError_GetStart" :int ((exc object) (start (:pointer ssize-t))))
#+(or) ;; how to handle pointer to size?
(defpyfun "PyUnicodeTranslateError_GetStart" :int ((exc object) (start (:pointer ssize-t))))
(defpyfun "PyUnicodeDecodeError_SetStart" :int ((exc object) (start ssize-t)))
(defpyfun "PyUnicodeEncodeError_SetStart" :int ((exc object) (start ssize-t)))
(defpyfun "PyUnicodeTranslateError_SetStart" :int ((exc object) (start ssize-t)))
#+(or) ;; how to handle pointer to size?
(defpyfun "PyUnicodeDecodeError_GetEnd" :int ((exc object) (end (:pointer ssize-t))))
#+(or) ;; how to handle pointer to size?
(defpyfun "PyUnicodeEncodeError_GetEnd" :int ((exc object) (end (:pointer ssize-t))))
#+(or) ;; how to handle pointer to size?
(defpyfun "PyUnicodeTranslateError_GetEnd" :int ((exc object) (end (:pointer ssize-t))))
(defpyfun "PyUnicodeDecodeError_SetEnd" :int ((exc object) (end ssize-t)))
(defpyfun "PyUnicodeEncodeError_SetEnd" :int ((exc object) (end ssize-t)))
(defpyfun "PyUnicodeTranslateError_SetEnd" :int ((exc object) (end ssize-t)))
;; end of canerr
(defpyfun "PyUnicodeDecodeError_GetReason" object ((exc object)))
(defpyfun "PyUnicodeEncodeError_GetReason" object ((exc object)))
(defpyfun "PyUnicodeTranslateError_GetReason" object ((exc object)))
;; more canerr
(defpyfun "PyUnicodeDecodeError_SetReason" :int ((exc object) (reason :string)))
(defpyfun "PyUnicodeEncodeError_SetReason" :int ((exc object) (reason :string)))
(defpyfun "PyUnicodeTranslateError_SetReason" :int ((exc object) (reason :string)))
#+TODO.really-a-#define (defpyfun "Py_EnterRecursiveCall" :int ((where :string)))
#+TODO.really-a-#define (defpyfun "Py_LeaveRecursiveCall" :void ())

;;; Exception Variables
;; TODO: Create a defpyexception macro which allows us to convert between
;;       Python exceptions and Lisp conditions.
(defpyvar "PyExc_BaseException")
(defpyvar "PyExc_Exception")
(defpyvar "PyExc_StandardError")
(defpyvar "PyExc_ArithmeticError")
(defpyvar "PyExc_LookupError")
(defpyvar "PyExc_AssertionError")
(defpyvar "PyExc_AttributeError")
(defpyvar "PyExc_EOFError")
(defpyvar "PyExc_EnvironmentError")
(defpyvar "PyExc_FloatingPointError")
(defpyvar "PyExc_IOError")
(defpyvar "PyExc_ImportError")
(defpyvar "PyExc_IndexError")
(defpyvar "PyExc_KeyError")
(defpyvar "PyExc_KeyboardInterrupt")
(defpyvar "PyExc_MemoryError")
(defpyvar "PyExc_NameError")
(defpyvar "PyExc_NotImplementedError")
(defpyvar "PyExc_OSError")
(defpyvar "PyExc_OverflowError")
(defpyvar "PyExc_ReferenceError")
(defpyvar "PyExc_RuntimeError")
(defpyvar "PyExc_SyntaxError")
(defpyvar "PyExc_SystemError")
(defpyvar "PyExc_SystemExit")
(defpyvar "PyExc_TypeError")
(defpyvar "PyExc_ValueError")
#+windows (defpyvar "PyExc_WindowsError")
(defpyvar "PyExc_ZeroDivisionError")

;;;; TODO: Operating System Utilities, System Functions, Process Control
;;;; TODO: Check Importing Modules; ensure all functions accounted for
;;;; TODO: Data Marshalling Support
;;;; TODO: Passing Arguments and Building Values
;;;; TODO: String Conversion and Formatting
;;;; TODO: Reflection
;;;; TODO: Codec Registry and Support Functions, Codec Lookup API, Registry API for Unicode Errors

;;;; Abstract Objects
#+requires-FILE*-support
(defpyfun "PyObject_Print" :int ((o object) (fp :file) (flags :int)))
(defpyfun "PyObject_HasAttr" :int ((o object) (attr-name object)))
(defpyfun "PyObject_HasAttrString" :int ((o object) (attr-name :string)))
(defpyfun "PyObject_GetAttr" object ((o object) (attr-name object)))
(defpyfun "PyObject_GetAttrString" object ((o object) (attr-name :string)))
(defpyfun "PyObject_GenericGetAttr" object ((o object) (name object)))
(defpyfun "PyObject_SetAttr" :int ((o object) (attr-name object) (v object)))
(defpyfun "PyObject_SetAttrString" :int ((o object) (attr-name :string) (v object)))
(defpyfun "PyObject_GenericSetAttr" :int ((o object) (name object) (value object)))
(defpyfun "PyObject_DelAttr" :int ((o object) (attr-name object))
  (object.set-attr o attr-name (null-pointer)))
(defpyfun "PyObject_DelAttrString" :int ((o object) (attr-name :string))
  (object.set-attr-string o attr-name (null-pointer)))
(defpyfun "PyObject_RichCompare" object ((o1 object) (o2 object) (opid :int)))
(defpyfun "PyObject_RichCompareBool" :int ((o1 object) (o2 object) (opid :int)))
#+requires-call-by-reference-support (defpyfun "PyObject_Cmp" :int ((o1 object) (o2 object) (result (:ref :int))))
(defpyfun "PyObject_Compare" :int ((o1 object) (o2 object)))
(defpyfun "PyObject_Repr" object ((o object)))
(defpyfun "PyObject_Str" object ((o object)))
#+(or) ; FIXME: This wouldn't account for the difference between .str and .str*
       ;        Maybe fix by giving the expansion a want-ptr variable to check?
(defpyfun "PyObject_Bytes" object ((o object))
  (object.str o))
(defpyfun "PyObject_Unicode" object ((o object)))
(defpyfun "PyObject_IsInstance" :int ((inst object) (cls object)))
(defpyfun "PyObject_IsSubclass" :int ((derived object) (cls object)))
(defpyfun "PyCallable_Check" :int ((o object)))
(defpyfun "PyObject_Call" object ((callable-object object) (args object) (kw object)))
(defpyfun "PyObject_CallObject" object ((callable-object object) (args object)))
(defpyfun "PyObject_CallFunction" object ((callable object) (format :string) &rest))
(defpyfun "PyObject_CallMethod" object ((o object) (method :string) (format :string) &rest))
#+(or) ;; last arg must be NULL
(defpyfun "PyObject_CallFunctionObjArgs" object ((callable object) &rest #+(or) <NULL>))
#+(or) ;; last arg must be NULL
(defpyfun "PyObject_CallMethodObjArgs" object ((o object) (name object) &rest #+(or) <NULL>))
(defpyfun "PyObject_Hash" :long ((o object)))
(defpyfun "PyObject_HashNotImplemented" :long ((o object)))
(defpyfun "PyObject_IsTrue" :int ((o object)))
(defpyfun "PyObject_Not" :int ((o object)))
(defpyfun "PyObject_Type" object ((o object)))
(defpyfun "PyObject_TypeCheck" :boolean ((o object) (type type-object))
  (or (%object.type-check-exact o type) (type.is-subtype (%object.type o) type)))
(defpyfun "PyObject_Length" ssize-t ((o object)))
(defpyfun "PyObject_Size" ssize-t ((o object)))
(defpyfun "PyObject_GetItem" object ((o object) (key object)))
(defpyfun "PyObject_SetItem" :int ((o object) (key object) (v object)))
(defpyfun "PyObject_DelItem" :int ((o object) (key object)))
(defpyfun "PyObject_AsFileDescriptor" :int ((o object)))
(defpyfun "PyObject_Dir" object ((o object)))
(defpyfun "PyObject_GetIter" object ((o object)))

;;;; TODO: Concrete Objects
;;;; TODO: Memory Management
;;;; TODO: Object Implementation Support (Allocating, Structures, Types, GC)

;; FIXME: reorder to better match order of Python's docs
;; FIXME: some of these :POINTERs should probably be OBJECTs (or DICTs, etc.)
;; FIXME: we also need a way to say "returns NULL on error" or "-1 on error"
(defpyfun "PyDict_New" dict ())
(defpyfun "PyDict_Keys" list ((d :pointer)))
(defpyfun "PyDict_Size" ssize-t ((d :pointer)))
(defpyfun "PyDict_GetItem" (object :borrowed) ((d :pointer) (key object)))
(defpyfun "PyDict_GetItemString" (object :borrowed) ((d :pointer) (key string)))
(defpyfun "PyDict_Items" list ((d :pointer)))
(defpyfun "PyDict_SetItem" :int ((d :pointer) (key object) (val object))) ; canerr
(defpyfun "PyFloat_AsDouble" :double ((s float)))
(defpyfun "PyFloat_FromDouble" float ((s :double)))
(defpyfun "PyImport_GetModuleDict" dict ())
(defpyfun "PyImport_Import" (can-error object) ((name string)))
(defpyfun "PyImport_ImportModule" (can-error :pointer) ((name :string)))
(defpyfun "PyImport_ImportModuleEx" (can-error :pointer) ((name :string) (globals :pointer) (locals :pointer) (fromlist :pointer))
  (import.import-module-level name globals locals fromlist -1))
(defpyfun "PyImport_ImportModuleLevel" (can-error :pointer) ((name :string) (globals :pointer) (locals :pointer) (fromlist :pointer) (level :int)))
(defpyfun "PyImport_AddModule" :pointer ((name :string)))
(defpyfun "PyType_IsSubtype" :boolean ((a :pointer) (b :pointer)))
(defpyfun "PyInt_AsLong" :long ((o :pointer)))
(defpyfun "PyInt_FromLong" int ((i :long)))
(defpyfun "PyList_New" list ((size :int)))
(defpyfun "PyList_Size" :int ((lst :pointer)))
(defpyfun "PyList_GetItem" (object :borrowed) ((lst :pointer) (index :int)))
(defpyfun "PyList_SetItem" :int ((lst :pointer) (index :int) (o object)))
(defpyfun "PyModule_GetDict" dict ((m :pointer)))
(defpyfun "PyString_AsString" :string ((s :pointer)))
(defpyfun "PyString_FromString" string ((s :string)))
#+(or) ; We probably want to return an octet array here, not a string
(defpyfun "PyBytes_AsString" :string ((s :pointer)) (string.as-string s))
#+(or) ; We probably want to pass in an octet array here, not a string
(defpyfun "PyBytes_FromString" :pointer ((s :string)) (string.from-string s))
(defpyfun "PyTuple_New" tuple ((size :int)))
(defpyfun "PyTuple_Size" :int ((lst :pointer)))
(defpyfun "PyTuple_GetItem" (object :borrowed) ((lst :pointer) (index :int)))
(defpyfun "PyTuple_SetItem" :int ((lst :pointer) (index :int) (o object)))
; may not exist (opposite ucs2)
(defpyfun "PyUnicodeUCS4_AsUnicode" ucs4-string ((s :pointer)) :not-if-ucs2)
(defpyfun "PyUnicodeUCS4_FromUnicode" unicode ((s ucs4-string) (size :int)) :not-if-ucs2)
(defpyfun "PyUnicodeUCS4_GetSize" :int ((u :pointer)) :not-if-ucs2)
; may not exist (opposite ucs4)
(defpyfun "PyUnicodeUCS2_AsUnicode" ucs2-string ((s :pointer)) :not-if-ucs4)
(defpyfun "PyUnicodeUCS2_FromUnicode" unicode ((s ucs2-string) (size :int)) :not-if-ucs4)
(defpyfun "PyUnicodeUCS2_GetSize" :int ((u :pointer)) :not-if-ucs4)
(defun unicode.as-unicode (s)
  (cond
    ((foreign-symbol-pointer "PyUnicodeUCS4_AsUnicode")
     (unicode-ucs4.as-unicode s))
    ((foreign-symbol-pointer "PyUnicodeUCS2_AsUnicode")
     (unicode-ucs2.as-unicode s))
    (t (error "No unicode functions!"))))
(defun unicode.from-unicode* (s size)
  (cond
    ((foreign-symbol-pointer "PyUnicodeUCS4_FromUnicode")
     (unicode-ucs4.from-unicode* s size))
    ((foreign-symbol-pointer "PyUnicodeUCS2_FromUnicode")
     (unicode-ucs2.from-unicode* s size))
    (t (error "No unicode functions!"))))

#+(or) ;; WARNING: don't trace if lots of data.  (feedparser.parse(my-lj) produces ~195k lines)
(trace translate-to-foreign translate-from-foreign free-translated-object
       %object.refcnt
       string.from-string* string.from-string
       unicode.from-unicode* unicode.from-unicode unicode-ucs4.from-unicode* unicode-ucs4.from-unicode
       .inc-ref .dec-ref
       object.type-check object.check type.check int.check string.check
       dict.new* dict.new dict.set-item dict.items dict.items* dict.get-item*
       tuple.get-item
       list.size)
