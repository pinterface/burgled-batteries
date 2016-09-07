(in-package :burgled-batteries)

(defstruct python-module
  name python-name methods documentation)

(defvar *python-modules* (make-hash-table :test #'equalp))
(defvar *python-module* nil)

(defun replace-all (string part replacement &key (test #'char=))
  "Returns a new string in which all the occurences of the part 
is replaced with replacement."
  (with-output-to-string (out)
    (loop with part-length = (length part)
          for old-pos = 0 then (+ pos part-length)
          for pos = (search part string
                            :start2 old-pos
                            :test test)
          do (write-string string out
                           :start old-pos
                           :end (or pos (length string)))
          when pos do (write-string replacement out)
          while pos)))

(defmethod python-string ((string symbol))
  (python-string (string-downcase (string string))))

(defmethod python-string ((string string))
  (replace-all string "-" "_"))

(defun get-pymodule (name &key (error-p t))
  (or 
   (gethash name *python-modules*)
   (and error-p
	(error "Python module not defined: ~A" name))))

(defmacro defpymodule (name &key python-name documentation)
  `(%defpymodule ',name 
		 :python-name ,python-name 
		 :documentation ,documentation))

(defun %defpymodule (name &key python-name documentation)
  (let ((python-module (make-python-module :name name 
					   :python-name (or python-name
							    (python-string name))
					   :documentation documentation)))
    (setf (gethash name *python-modules*)
	  python-module)
    (setf *python-module* python-module)))

(defmacro in-pymodule (name)
  `(%in-pymodule ',name))

(defun %in-pymodule (name)
  (setf *python-module* (get-pymodule name)))

(defmacro defpycallback (name-and-options (&rest args) &body body)
  (multiple-value-bind (lisp-name python-name return-type python-module)
      (if (listp name-and-options)
	  (let ((options (cdr name-and-options)))
	    (values (first name-and-options)
		    (or (getf options :python-name)
			(python-string (first name-and-options)))
		    (or (getf options :return-type)
			:pointer)
		    (let ((python-module (getf options :module)))
		      (or (and python-module `(get-pymodule ',python-module))
			  '*python-module*))))
	  (values name-and-options
		  (python-string name-and-options)
		  :pointer
		  '*python-module*))
    `(progn
       (python.cffi::defpycallback ,lisp-name ,return-type ,args ,@body)
       (when ,python-module
	 (pushnew 
	  (cons ,python-name
		',lisp-name)
	  (python-module-methods ,python-module)
	  :test #'equalp)))))

(defun initialize-module (module)
  (python.cffi::build-module 
   (python-module-python-name module)
   (python-module-methods module)))

(defun initialize-modules ()
  (loop 
     :for module :being :the :hash-values :of *python-modules*
     :do (initialize-module module)))
