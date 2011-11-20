(in-package #:python.cffi)

(cc-flags #.(format nil "-I~A" *cpython-include-dir*))

(include "Python.h")
(include "structmember.h") ; needed for member type flags

;; Sizes
(ctype ssize-t "Py_ssize_t")
(ctype size-t  "size_t")

;; Evaluation Context Flags
(constantenum parser-context
  ((:expression "Py_eval_input"))
  ((:statement  "Py_single_input"))
  ((:file       "Py_file_input")))

;; Profiling and Tracing
(constantenum trace-what
  ((:call        "PyTrace_CALL"))
  ((:exception   "PyTrace_EXCEPTION"))
  ((:line        "PyTrace_LINE"))
  ((:return      "PyTrace_RETURN"))
  ((:c-call      "PyTrace_C_CALL"))
  ((:c-exception "PyTrace_C_EXCEPTION"))
  ((:c-return    "PyTrace_C_RETURN")))

;; Comparison Operator Flags
(constantenum comparison-operator
  ((:<  "Py_LT"))
  ((:<= "Py_LE"))
  ((:=  "Py_EQ"))
  ((:/= "Py_NE"))
  ((:>  "Py_GT"))
  ((:>= "Py_GE")))

;; Type Flags
(bitfield (type-flags :base-type :long)
  (:have-get-char-buffer     "Py_TPFLAGS_HAVE_GETCHARBUFFER")
  (:have-sequence-in         "Py_TPFLAGS_HAVE_SEQUENCE_IN")
  (:gc                       "Py_TPFLAGS_GC")
  (:have-in-place-ops        "Py_TPFLAGS_HAVE_INPLACEOPS")
  (:check-types              "Py_TPFLAGS_CHECKTYPES")
  (:have-rich-compare        "Py_TPFLAGS_HAVE_RICHCOMPARE")
  (:have-weak-refs           "Py_TPFLAGS_HAVE_WEAKREFS")
  (:have-iter                "Py_TPFLAGS_HAVE_ITER")
  (:have-class               "Py_TPFLAGS_HAVE_CLASS")
  (:heap-type                "Py_TPFLAGS_HEAPTYPE")
  (:base-type                "Py_TPFLAGS_BASETYPE")
  (:ready                    "Py_TPFLAGS_READY")
  (:readying                 "Py_TPFLAGS_READYING")
  (:have-gc                  "Py_TPFLAGS_HAVE_GC")
  (:have-stackless-extension "Py_TPFLAGS_HAVE_STACKLESS_EXTENSION")
  (:have-index               "Py_TPFLAGS_HAVE_INDEX")
  (:have-version-tag         "Py_TPFLAGS_HAVE_VERSION_TAG")
  (:valid-version-tag        "Py_TPFLAGS_VALID_VERSION_TAG")
  (:is-abstract              "Py_TPFLAGS_IS_ABSTRACT")
  (:have-new-buffer          "Py_TPFLAGS_HAVE_NEWBUFFER")
  (:int-subclass             "Py_TPFLAGS_INT_SUBCLASS")
  (:long-subclass            "Py_TPFLAGS_LONG_SUBCLASS")
  (:list-subclass            "Py_TPFLAGS_LIST_SUBCLASS")
  (:tuple-subclass           "Py_TPFLAGS_TUPLE_SUBCLASS")
  (:string-subclass          "Py_TPFLAGS_STRING_SUBCLASS")
  (:unicode-subclass         "Py_TPFLAGS_UNICODE_SUBCLASS")
  (:dict-subclass            "Py_TPFLAGS_DICT_SUBCLASS")
  (:base-exception-subclass  "Py_TPFLAGS_BASE_EXC_SUBCLASS")
  (:type-subclass            "Py_TPFLAGS_TYPE_SUBCLASS")
  (:default-external         "Py_TPFLAGS_DEFAULT_EXTERNAL")
  (:default-core             "Py_TPFLAGS_DEFAULT_CORE")
  (:default                  "Py_TPFLAGS_DEFAULT"))

;; Method Call Flags
(bitfield method-convention-flags
  ;; calling convention
  (:positional-arguments "METH_VARARGS")
  (:keyword-arguments    "METH_KEYWORDS")
  (:mixed-arguments      "METH_KEYWORDS | METH_VARARGS")
  (:no-arguments         "METH_NOARGS")
  (:object-method        "METH_O")
  ;; binding convention
  (:class-binding  "METH_CLASS")
  (:static-binding "METH_STATIC")
  ;; replace existing definition
  (:coexist          "METH_COEXIST")
  (:replace-existing "METH_COEXIST"))

;; Member Type Flags
(constantenum member-type
  ((:short              "T_SHORT"))
  ((:int                "T_INT"))
  ((:long               "T_LONG"))
  ((:float              "T_FLOAT"))
  ((:double             "T_DOUBLE"))
  ((:string             "T_STRING"))
  ((:object             "T_OBJECT"))
  ((:object-ex          "T_OBJECT_EX"))
  ((:char               "T_CHAR"))
  ((:byte               "T_BYTE"))
  ((:unsigned-byte      "T_UBYTE"))
  ((:unsigned-int       "T_UINT"))
  ((:unsigned-short     "T_USHORT"))
  ((:unsigned-long      "T_ULONG"))
  ((:boolean            "T_BOOL"))
  ((:long-long          "T_LONGLONG")  :optional t)
  ((:unsigned-long-long "T_ULONGLONG") :optional t)
  ((:ssize-t            "T_PYSSIZET")))

;; API Version
(constant (+api-version+ "PYTHON_API_VERSION") :type integer)
