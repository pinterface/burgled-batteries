(in-package #:python-cffi.test)

(lift:addtest (burgled-batteries)
  barrier-test
  (python.cffi:with-refcnt-barrier
    (loop :for (object code) :in `((,python.cffi:+None+  "None")
				   (,python.cffi:+True+  "True")
				   (,python.cffi:+False+ "False")
				   ;; CPython implements small numbers as shared references.
				   (,(burgled-batteries:run* "1") "1"))
       :do (symbol-macrolet ((current-refcnt (python.cffi::%object.refcnt object)))
	     (let ((orig-refcnt current-refcnt))
	       (flet ((ensure-unchanged-refcnt (python-code)
			(burgled-batteries:run python-code)
			(lift:ensure (= orig-refcnt current-refcnt)
				     :report
				     "Reference count for ~S was ~A by ~S"
				     :arguments
				     (code
				      (if (> orig-refcnt current-refcnt) "decreased" "increased")
				      python-code))))
		 (ensure-unchanged-refcnt code)
		 (ensure-unchanged-refcnt (format nil "[~A, ~A, ~A]" code code code))
		 (ensure-unchanged-refcnt (format nil "(~A, ~A, ~A)" code code code))
		 (ensure-unchanged-refcnt (format nil "dict(a=~A, b=~A, c=~A)" code code code))))))))

;; unknown translations
(lift:addtest (burgled-batteries)
  barrier-test-2
  (python.cffi:with-refcnt-barrier
    (burgled-batteries:run "import datetime")
    (burgled-batteries:run "tmp = datetime.date.today()")
    (let* ((ptr (cpython::dict.get-item* burgled-batteries::main-module-dict* "tmp"))
	   (orig-refcnt (cpython::%object.refcnt ptr)))
      (loop :for (object code) :in `((,ptr "tmp"))
	 :do (symbol-macrolet ((current-refcnt (python.cffi::%object.refcnt object)))
	       (let ((orig-refcnt current-refcnt))
		 (flet ((ensure-unchanged-refcnt (python-code)
			  (cpython:with-refcnt-barrier
			    (burgled-batteries:run python-code))
			  (lift:ensure (= orig-refcnt current-refcnt)
				       :report
				       "Reference count for ~S was ~A by ~S"
				       :arguments (code
						   (if (> orig-refcnt current-refcnt) "decreased" "increased")
						   python-code))))
                           
		   (ensure-unchanged-refcnt code)
		   (ensure-unchanged-refcnt (format nil "[~A, ~A, ~A]" code code code))
		   (ensure-unchanged-refcnt (format nil "(~A, ~A, ~A)" code code code))
		   (ensure-unchanged-refcnt (format nil "dict(a=~A, b=~A, c=~A)" code code code))))))
      (lift:ensure (= orig-refcnt (cpython::%object.refcnt ptr))
		   :report
		   "Reference count was changed ~D overall."
		   :arguments
		   ((- orig-refcnt (cpython::%object.refcnt ptr)))))))

;; Inspired by (read: almost entirely copied from) #'VOODOO in trivial-garbage's
;; tests.
(defun voodoo (expr)
  (funcall (compile nil `(lambda () (eval (read-from-string (format nil "~S" ',expr))))))
  (values))

(lift:addtest (burgled-batteries)
  finalization-test
  (cpython:with-unknown-translation-policy (:finalize)
    (burgled-batteries:import "time")
    (burgled-batteries:run "v = time.gmtime()")
    (let* ((code "v")
	   (wrapped (burgled-batteries:run code))
	   (object (python.cffi::wrapped-value wrapped)))
      (unwind-protect
	   (symbol-macrolet ((current-refcnt (python.cffi::%object.refcnt object)))
	     (let ((orig-refcnt current-refcnt))
	       (flet ((ensure-unchanged-refcnt (python-code)
			(tg:gc :full t)
			(voodoo `(burgled-batteries:run ,python-code))
			(voodoo `(tg:gc :full t))
			(lift:ensure (= orig-refcnt current-refcnt)
				     :report
				     "Reference count (~A, ~A) for ~S was ~A by ~A from ~S"
				     :arguments
				     (orig-refcnt current-refcnt
						  code
						  (if (> orig-refcnt current-refcnt) "decreased" "increased")
						  (abs (- orig-refcnt current-refcnt))
						  python-code))))
		 (ensure-unchanged-refcnt code)
		 (ensure-unchanged-refcnt (format nil "[~A, ~A, ~A]" code code code))
		 (ensure-unchanged-refcnt (format nil "(~A, ~A, ~A)" code code code))
		 (ensure-unchanged-refcnt (format nil "dict(a=~A, b=~A, c=~A)" code code code)))))
	(burgled-batteries:run "v = None")
	(tg:gc :full t))
      ;; Mostly this is just here to ensure wrapped doesn't get GCed during the tests
      (cpython::%object.refcnt (cpython::wrapped-value wrapped)))))

;; unknown translations with stolen references
(lift:addtest (burgled-batteries)
  refcnt-test
  (cpython:with-refcnt-barrier
    (burgled-batteries:run "import datetime")
    (burgled-batteries:run "tmp = datetime.date.today()")
    (let* ((ptr (cpython::dict.get-item* burgled-batteries::main-module-dict* "tmp"))
	   (orig-refcnt (cpython::%object.refcnt ptr)))
      (loop :for (object code) :in `((,ptr "tmp"))
	 :do (symbol-macrolet ((current-refcnt (python.cffi::%object.refcnt object)))
	       (let* ((orig-refcnt current-refcnt)
		      (tuple* (burgled-batteries:run* "(1, 2, 3, 4)")))
		 (flet ((ensure-unchanged-refcnt ()
			  (lift:ensure (= orig-refcnt current-refcnt)
				       :report
				       "Reference count for ~S was ~A"
				       :arguments (
						   code
						   (if (> orig-refcnt current-refcnt) "decreased" "increased")
						   ))))
		   (ensure-unchanged-refcnt)
		   (cpython:with-unknown-translation-policy (:barrier)
		     (cpython:tuple.set-item tuple* 0 (burgled-batteries:run "tmp")))
		   (voodoo
		    `(cpython:with-unknown-translation-policy (:finalize)
		       (cpython:tuple.set-item ,tuple* 1 (burgled-batteries:run "tmp"))))
		   (voodoo `(tg:gc :full t))
		   (cpython:.dec-ref tuple*)
		   (ensure-unchanged-refcnt)))))
      (lift:ensure (= orig-refcnt (cpython::%object.refcnt ptr))
		   :report
		   "Reference count was changed ~D overall."
		   :arguments
		   ((- orig-refcnt (cpython::%object.refcnt ptr)))))))
