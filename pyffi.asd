;;; -*- Lisp -*-
(defpackage :pyffi-system (:use #:asdf #:cl))
(in-package :pyffi-system)

(cl:eval-when (:load-toplevel :execute)
  (asdf:load-system "cffi-grovel"))

(defsystem :pyffi
    :depends-on (:cffi)
    :name "pyffi"
    :author "Dmitri Hrapof <hrapof@common-lisp.ru>"
    :maintainer "Dmitri Hrapof <hrapof@common-lisp.ru>"
    :licence "LGPL"
    :description "Python interface"
    :components
    (;; FIXME: auto-detect location of Python include directory
     (cffi-grovel:grovel-file "grovel" :cc-flags ("-I/usr/include/python2.6"))
     (:file "pyffi")))
