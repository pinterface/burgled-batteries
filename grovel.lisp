(in-package #:python.cffi)

(include "Python.h")

(ctype ssize-t "Py_ssize_t")

(constant (+eval-input+   "Py_eval_input"  ) :type integer)
(constant (+file-input+   "Py_file_input"  ) :type integer)
(constant (+single-input+ "Py_single_input") :type integer)
