(defpackage #:python.cffi
  (:use #:cl #:cffi #:alexandria)
  (:import-from #:cffi #:parse-type #:find-type-parser)
  (:shadow #:type #:list #:float #:string)
  (:export #:.initialize
           #:.finalize
           #:import.add-module
           #:.dec-ref
           #:.inc-ref
           #:+eval-input+
           #:+file-input+
           #:+single-input+
           .
           ;; SUPER HACKY to aid reloading
           #.(let ((sym nil))
               (do-external-symbols (v '#:python.cffi sym)
                 (push (symbol-name v) sym)))))

(defpackage #:pyffi
  (:use #:cl #:python.cffi)
  (:shadow #:import
           #:eval
           #:apply)
  (:export #:startup-python
           #:shutdown-python
           #:import
           #:eval
           #:apply
           #:defpyfun
           #:defpyslot
           #:defpymethod))
