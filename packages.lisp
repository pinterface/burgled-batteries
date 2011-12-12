(defpackage #:python.cffi
  (:nicknames #:cpython)
  (:use #:cl #:cffi #:alexandria #:tcr.parse-declarations-1.0)
  (:import-from #:cffi #:parse-type #:find-type-parser)
  (:shadow #:type #:list #:float #:string #:complex #:function #:method #:type-error #:return #:arithmetic-error)
  (:export #:.initialize
           #:.finalize
           #:import.add-module
           #:with-unknown-translation-policy
           #:with-refcnt-barrier
           #:.dec-ref
           #:.inc-ref
           #:+None+
           #:+True+
           #:+False+
           #:trace-refs
           .
           ;; SUPER HACKY to aid reloading
           #.(let ((sym nil))
               (when (find-package '#:python.cffi)
                 (do-external-symbols (v '#:python.cffi sym)
                   (push (symbol-name v) sym)))))
  (:documentation "
PYTHON.CFFI is the package for defining the CFFI interface to Python's external
C functions.  It provides several macros to aid in this effort, namely DEFPYFUN,
DEFPYVAR, and DEFPYTYPE.  See the documentation for those macros for more
information.

Name translation from Python's C names into Lisp is, in general, relatively
straightforward.  First, the \"Py\" prefix is stripped, and the very first
underscore is turned into a period.  Then, CamelCaseIdentifiers are converted to
the more-common-to-Lisp dash-separated-identifier, and any remaining underscores
are /also/ turned into dashes.  In some cases (PyList, PyFunction, etc.) this
results in symbol-names equal to those of CL symbols, and you should accordingly
be careful within this package.

Many Python functions have yet to be defined.  See TODOs, and Python docs for
information on what remains.

Error Checking from C functions is handled within CFFI's type translation
system.  See the CAN-ERROR foreign-type (and both the CAN-ERROR and SOFT-ERROR
parse-methods) for the gory details, but to summarize: some C functions return a
value indicating that an error occurred.  This can be specified using
CAN-ERROR (indicating that the error value is strict and definitely means an
error occurred) and SOFT-ERROR (indicating that the error value may also just be
a regular return value and #'%ERROR-OCCURRED-P must be consulted to verify the
occurrance of an error).  Types with an exclamation mark (#\\!) appended are
shorthand for (CAN-ERROR <type>); types with a question mark (#\\?) appended are
shorthand for (SOFT-ERROR <type>).

Lisp<->Python type translation is handled through the FOREIGN-PYTHON-TYPE
foreign-type.  \"new\" vs. \"borrowed\" and \"stolen\" vs. \"copied\" references
can be accounted for by specifying :new, :borrowed, :stolen, or :copied as
arguments to the appropriate type specifier.  :new and :copied are the defaults.

Python's PyString/PyBytes type is best avoided due to the problematic nature in
converting to and from the type.  See the WARNING above (defpytype \"PyString\"
...) for details.
"))

(defpackage #:burgled-batteries
  (:use #:cl #:python.cffi #:alexandria)
  (:shadowing-import-from #:python.cffi #:arithmetic-error #:type-error)
  (:shadow #:import
           #:apply)
  (:export #:startup-python
           #:shutdown-python
           #:import
           #:run
           #:run*
           #:apply
           #:defpyfun
           #:defpyslot
           #:defpymethod))
