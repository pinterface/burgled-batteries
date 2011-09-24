(defpackage #:python.cffi
  (:use #:cl #:cffi #:alexandria #:tcr.parse-declarations-1.0)
  (:import-from #:cffi #:parse-type #:find-type-parser)
  (:shadow #:type #:list #:float #:string #:complex)
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
               (when (find-package '#:python.cffi)
                 (do-external-symbols (v '#:python.cffi sym)
                   (push (symbol-name v) sym))))))

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
