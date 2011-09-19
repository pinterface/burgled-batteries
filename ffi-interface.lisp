(in-package #:python.cffi)

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

(defun raise-python-exception ()
  (let* ((exc (err.occurred))
         (desc (object.str exc)))
    (unwind-protect
         (error 'python-error :exc desc)
      (err.clear))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun translate-python-name (c-name)
    (unless (string= "Py" c-name :end2 2)
      (error "Not a Python name."))
    (ensure-symbol
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

(defcstruct %object
  ;; If Py_TRACE_REFS
  ;;("_ob_next" object)
  ;;("_ob_prev" object)
  (refcnt ssize-t)
  (type :pointer))

(defun %object.type (o) (foreign-slot-value o '%object 'type))
(defun %object.type-check-exact (o type) (pointer-eq (%object.type o) type))

(define-foreign-type foreign-python-type ()
  ((borrowedp :initarg :borrowedp :reader borrowed-reference-p)
   (translate-to   :initarg :to)
   (translate-from :initarg :from)
   (foreign-is-type :initarg :check-ptr)
   (lisp-is-type    :initarg :check-lisp)))

(defmethod print-object ((o foreign-python-type) s)
  (print-unreadable-object (o s :type t)
    (format s "~A ~A" (ignore-errors (cffi::unparsed-type o)) (if (borrowed-reference-p o) :borrowed :new))))

(defmethod translate-to-foreign (value (type foreign-python-type))
  (cond
    ((cffi-sys:pointerp value) (values value nil)) ; assume already foreign
    (t (values (funcall (slot-value type 'translate-to) value type) t))))

(defmethod translate-from-foreign (value (type foreign-python-type))
  (unwind-protect
       (funcall (slot-value type 'translate-from) value type)
    (unless (borrowed-reference-p type)
      (.dec-ref value))))

(defmethod free-translated-object (value (type foreign-python-type) decrefp)
  (declare (ignore type))
  #+pyffi.debug (format t "In free-translated-object: ~A, ~A, ~A~%" value decrefp (foreign-slot-value value '%object 'refcnt))
  (when decrefp (.dec-ref value))
  nil)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *type-map* (make-hash-table)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun register-python-type (python-type lisp-type)
    (setf (gethash python-type *type-map*) lisp-type)))

(defmacro defpytype (c-name &body options)
  (let* ((lisp-name (translate-python-name c-name))
         (c-var (format nil "~A_Type" c-name))
         (lisp-var (format-symbol #.*package* "+~A+" (translate-python-name c-var)))
         (lisp-type (car (assoc-value options :type)))
         (c-type-check       (translate-python-name (format nil "~A_Check" c-name)))
         (c-type-check-exact (translate-python-name (format nil "~A_CheckExact" c-name)))
         (to   (assoc-value options :to))
         (from (assoc-value options :from)))
    (destructuring-bind ((to-val to-type) &rest to-body) to
      (destructuring-bind ((from-val from-type) &rest from-body) from
        `(eval-when (:compile-toplevel :load-toplevel :execute)
           (defparameter ,lisp-var (foreign-symbol-pointer ,c-var))
           (defun ,c-type-check       (o) (object.type-check o ,lisp-var))
           (defun ,c-type-check-exact (o) (%object.type-check-exact o ,lisp-var))
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
                                            ,(when lisp-type `(typep v ',lisp-type)))))
           (register-python-type ',lisp-name (find-type-parser ',lisp-name)))))))

(defpytype "PyType"
  (:to   (value type) value)
  (:from (value type) value))

;; Docs say use Py_None, but _Py_NoneStruct is what's exported
(defparameter +None+ (foreign-symbol-pointer "_Py_NoneStruct"))
(defun %none.check (o) (pointer-eq +None+ o))

(defpytype "PyInt"
  (:type integer)
  (:to   (value type) (int.from-long value))
  (:from (value type) (int.as-long value)))

(defpytype "PyTuple"
  ;; CAUTION: tuple of characters will get confused with strings
  (:type (and vector (not cl:string)))
  (:to (value type)
    (let* ((len (length value))
           (tup (tuple.new len)))
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
    (let* ((len (length value))
           (lst (list.new len)))
      (dotimes (i len lst)
        (list.set-item lst i (nth i value)))))
  (:from (value type)
    (let* ((len (list.size value))
           lst)
      (dotimes (i len) (push (list.get-item value i) lst))
      (reverse lst))))

(defpytype "PyFloat"
  (:type (or single-float double-float))
  (:to   (value type) (float.from-double value))
  (:from (value type) (float.as-double   value)))

(defpytype "PyString"
  (:type cl:string)
  (:to   (value type) (string.from-string value))
  (:from (value type) (string.as-string   value)))

#+(or) ;; FIXME: we'll need the bytes.whatever functions
(defpytype "PyBytes"
  (:to   (value type) (bytes.from-string value))
  (:from (value type) (bytes.as-string   value)))

(defpytype "PyUnicode"
  (:type cl:string)
  (:to   (value type) #+(or) (string.from-string value))
  (:from (value type) #+(or) (string.as-string   value)
         (with-output-to-string (s)
           (dotimes (i (unicode-ucs4.get-size value))
             (princ (code-char (mem-ref (unicode-ucs4.as-unicode value) :uint32 (* 4 i))) s)))
         #+(or) (cffi:convert-from-foreign (unicode-ucs4.as-unicode value) '(:octet))))

(defpytype "PyDict"
  (:type hash-table)
  (:to (value type)
    (let ((dct (dict.new)))
      (maphash
       (lambda (k v) (dict.set-item dct k v))
       value)
      dct))
  (:from (value type)
    (let ((dct (make-hash-table :test #'equal))
          (its (dict.items value)))
      (dolist (i its dct) (setf (gethash (svref i 0) dct) (svref i 1))))))

(defpytype "PyObject"
  (:to (value type)
    (loop :for v :being :the :hash-values :of *type-map*
          :for foreign-type := (funcall v reference-type)
          :when (funcall (slot-value foreign-type 'lisp-is-type) value)
            :do (return (translate-to-foreign value foreign-type))
          :finally (return value)))
  (:from (value type)
    (if (null-pointer-p value) (raise-python-exception))
    (loop :for v :being :the :hash-values :of *type-map*
          :for foreign-type := (funcall v reference-type)
          :when (funcall (slot-value foreign-type 'foreign-is-type) value)
            :do (format t "will translate ~A using ~A~%" value foreign-type)
                (return (translate-from-foreign value foreign-type))
          :finally (return value))))



(defmacro defpyfun (name return-type args &body alternate)
  (let* ((translating-lisp-name (translate-python-name name))
         (ptring-lisp-name (when (gethash return-type *type-map*)
                             (format-symbol (symbol-package translating-lisp-name) "~A*" translating-lisp-name))))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       ,(cond
         ((foreign-symbol-pointer name)
          `(progn
             (defcfun (,name ,translating-lisp-name) ,return-type ,@args)
             ,(when ptring-lisp-name
                `(defcfun (,name ,ptring-lisp-name) :pointer ,@args))))
         (alternate
          `(progn
             (defun ,translating-lisp-name ,(mapcar #'first args)
               ,@alternate)
             ,(when ptring-lisp-name
                `(defun ,ptring-lisp-name ,(mapcar #'first args)
                   ,@alternate)))))
       (export ',translating-lisp-name)
       ,(when ptring-lisp-name `(export ',ptring-lisp-name))
       ',translating-lisp-name)))

;; FIXME: reorder to better match order of Python's docs
;; FIXME: some of these :POINTERs should probably be OBJECTs (or DICTs, etc.)
;; FIXME: we also need a way to say "returns NULL on error" or "-1 on error"
(defpyfun "Py_Initialize" :void ())
(defpyfun "Py_Finalize"   :void ())
(defpyfun "Py_IncRef" :void ((o :pointer)))
(defpyfun "Py_DecRef" :void ((o :pointer)))
(defpyfun "PyCallable_Check" :boolean ((o :pointer)))
(defpyfun "PyDict_New" :pointer ())
(defpyfun "PyDict_Keys" list ((d :pointer)))
(defpyfun "PyDict_Size" ssize-t ((d :pointer)))
(defpyfun "PyDict_GetItem" (object :borrowed) ((d :pointer) (key object)))
(defpyfun "PyDict_GetItemString" (object :borrowed) ((d :pointer) (key string)))
(defpyfun "PyDict_Items" list ((d :pointer)))
(defpyfun "PyDict_SetItem" :int ((d :pointer) (key object) (val object))) ; canerr
(defpyfun "PyErr_Clear" :void ())
(defpyfun "PyErr_Occurred" (object :borrowed) ())
(defpyfun "PyErr_Print" :void ())
(defpyfun "PyFloat_AsDouble" :double ((s :pointer)))
(defpyfun "PyFloat_FromDouble" :pointer ((s :double)))
(defpyfun "PyImport_GetModuleDict" dict ())
(defpyfun "PyImport_Import" :pointer ((name string)))
(defpyfun "PyImport_ImportModule" :pointer ((name :string)))
(defpyfun "PyImport_ImportModuleEx" :pointer ((name :string) (globals :pointer) (locals :pointer) (fromlist :pointer))
  (import.import-module-level name globals locals fromlist -1))
(defpyfun "PyImport_ImportModuleLevel" :pointer ((name :string) (globals :pointer) (locals :pointer) (fromlist :pointer) (level :int)))
(defpyfun "PyImport_AddModule" :pointer ((name :string)))
(defpyfun "PyType_IsSubtype" :boolean ((a :pointer) (b :pointer)))
(defpyfun "PyInt_AsLong" :long ((o :pointer)))
(defpyfun "PyInt_FromLong" :pointer ((i :long)))
(defpyfun "PyList_New" :pointer ((size :int)))
(defpyfun "PyList_Size" :int ((lst :pointer)))
(defpyfun "PyList_GetItem" (object :borrowed) ((lst :pointer) (index :int)))
(defpyfun "PyList_SetItem" :int ((lst :pointer) (index :int) (o object)))
(defpyfun "PyModule_GetDict" dict ((m :pointer)))
(defcfun ("PyModule_GetDict" module.getdict/as-ptr) :pointer (m :pointer))
(export 'module.getdict/as-ptr)
(defpyfun "PyObject_CallObject" object ((o :pointer) (args tuple)))
(defpyfun "PyObject_GetAttrString" object ((o :pointer) (attr :string)))
(defpyfun "PyObject_SetAttrString" :int ((o :pointer) (s :string) (a object))) ;;canerr
(defpyfun "PyObject_Str" string ((o :pointer)))
(defpyfun "PyObject_Type" :pointer ((o :pointer)))
(defpyfun "PyObject_TypeCheck" :boolean ((o :pointer) (type :pointer))
  (or (%object.type-check-exact o type) (type.is-subtype (%object.type o) type)))
(defpyfun "PyObject_Not" :int ((o :pointer)))
(defpyfun "PyRun_String" object ((str (:string :encoding :utf-8)) (start :int) (globals :pointer) (locals :pointer)))
(defpyfun "PyString_AsString" :string ((s :pointer)))
(defpyfun "PyString_FromString" :pointer ((s :string)))
(defpyfun "PyBytes_AsString" :string ((s :pointer)))
(defpyfun "PyBytes_FromString" :pointer ((s :string)))
(defpyfun "PyTuple_New" :pointer ((size :int)))
(defpyfun "PyTuple_Size" :int ((lst :pointer)))
(defpyfun "PyTuple_GetItem" (object :borrowed) ((lst :pointer) (index :int)))
(defpyfun "PyTuple_SetItem" :int ((lst :pointer) (index :int) (o object)))
; may not exist (opposite ucs2)
(defpyfun "PyUnicodeUCS4_AsUnicode" :pointer #+(or) (:string :encoding :ucs-4) ((s :pointer)))
(defpyfun "PyUnicodeUCS4_FromUnicode" :pointer ((s :pointer) (size :int)))
(defpyfun "PyUnicodeUCS4_GetSize" :int ((u :pointer)))
; may not exist (opposite ucs4)
(defpyfun "PyUnicodeUCS2_AsUnicode" (:string :encoding :ucs-2) ((s :pointer)))
(defpyfun "PyUnicodeUCS2_FromUnicode" :pointer ((s :pointer) (size :int)))
(defpyfun "PyUnicodeUCS2_GetSize" :int ((u :pointer)))
