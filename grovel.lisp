(in-package #:pyffi)

(include "Python.h")

(ctype :Py-ssize-t "Py_ssize_t")

(constant (+py-eval-input+   "Py_eval_input"  ) :type integer)
(constant (+py-file-input+   "Py_file_input"  ) :type integer)
(constant (+py-single-input+ "Py_single_input") :type integer)
