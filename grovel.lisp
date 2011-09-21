(in-package #:python.cffi)

(include "Python.h")

(ctype ssize-t "Py_ssize_t")

(constant (+eval-input+   "Py_eval_input"  ) :type integer)
(constant (+file-input+   "Py_file_input"  ) :type integer)
(constant (+single-input+ "Py_single_input") :type integer)

;; Profiling and Tracing
(constant (+trace-call+        "PyTrace_CALL")        :type integer)
(constant (+trace-exception+   "PyTrace_EXCEPTION")   :type integer)
(constant (+trace-line+        "PyTrace_LINE")        :type integer)
(constant (+trace-return+      "PyTrace_RETURN")      :type integer)
(constant (+trace-c-call+      "PyTrace_C_CALL")      :type integer)
(constant (+trace-c-exception+ "PyTrace_C_EXCEPTION") :type integer)
(constant (+trace-c-return+    "PyTrace_C_RETURN")    :type integer)
