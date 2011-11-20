(in-package #:cpython)

(defun read-path ()
  (format *query-io* "Enter directory containing Python's C header files: ")
  (cl:list (read-line *query-io*)))

(defun query-error (format-string &rest args)
  (restart-case (apply #'error format-string args)
    (use-value (v)
      :report "Specify directory containing CPython's header files"
      :interactive read-path
      v)))

(defun query-user-for-include-dir ()
  (loop :for path := (query-error "Unable to determine Python include directory.")
               :then (query-error "Path ~s does not appear to exist." path)
        :when (cl-fad:directory-exists-p path) :return it))

(defparameter *cpython-include-dir*
  (or (loop :for minor :from 7 :downto 4
            :when (or (cl-fad:directory-exists-p (format nil "/usr/include/python2.~d" minor))
                      (cl-fad:directory-exists-p (format nil "/usr/local/include/python2.~d" minor)))
              :return it)
      ;; This allows us to avoid querying the user during a recompile, while
      ;; still allowing for a change in Python version
      (when (boundp '*cpython-include-dir*)
        (cl-fad:directory-exists-p *cpython-include-dir*))
      (query-user-for-include-dir)))
