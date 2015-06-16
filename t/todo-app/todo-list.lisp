(in-package :todo-list)

(python:defpymodule todo-list)

(python:in-pymodule todo-list)

(defvar *todo-list* nil)

(python:defpycallback (get-todo-list :return-type python.cffi::tuple)
    ()
  *todo-list*)

(python:defpycallback (add-item :return-type python.cffi::tuple)
    ((item python.cffi::string))
  (push item *todo-list*)
  *todo-list*)

(python:defpycallback (remove-item :return-type python.cffi::tuple)
    ((item python.cffi::string))
  (setf *todo-list* (delete item *todo-list* :test #'string=))
  *todo-list*)

(defun tk ()
  (python:startup-python)
  (python:run* (asdf:system-relative-pathname :burgled-batteries 
					      "t/todo-app/tk.py")))
