(in-package #:python-cffi.test)

;; TODO: Use an actual test library.

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
                         (assert (= orig-refcnt current-refcnt) ()
                                 "Reference count for ~S was ~A by ~S"
                                 code
                                 (if (> orig-refcnt current-refcnt) "decreased" "increased")
                                 python-code)
                         (format t ".")))
                  (ensure-unchanged-refcnt code)
                  (ensure-unchanged-refcnt (format nil "[~A, ~A, ~A]" code code code))
                  (ensure-unchanged-refcnt (format nil "(~A, ~A, ~A)" code code code))
                  (ensure-unchanged-refcnt (format nil "dict(a=~A, b=~A, c=~A)" code code code)))))))

(python.cffi::with-pointer-finalizers
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
                      (join-thread (make-thread (lambda () (burgled-batteries:run python-code) (values))))
                      (tg:gc :full t)
                      (assert (= orig-refcnt current-refcnt) ()
                              "Reference count for ~S was ~A by ~A from ~S"
                              code
                              (if (> orig-refcnt current-refcnt) "decreased" "increased")
                              (abs (- orig-refcnt current-refcnt))
                              python-code)
                      (format t ".")))
               (ensure-unchanged-refcnt code)
               (ensure-unchanged-refcnt (format nil "[~A, ~A, ~A]" code code code))
               (ensure-unchanged-refcnt (format nil "(~A, ~A, ~A)" code code code))
               (ensure-unchanged-refcnt (format nil "dict(a=~A, b=~A, c=~A)" code code code)))))
      (burgled-batteries:run "v = None")
      (tg:gc :full t))))
