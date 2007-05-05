;;; -*- Lisp -*-
(defpackage :pyffi-system (:use #:asdf #:cl))
(in-package :pyffi-system)

(defsystem :pyffi
    :depends-on (:cffi)
    :name "pyffi"
    :author "Dmitri Hrapof <hrapof@common-lisp.ru>"
    :maintainer "Dmitri Hrapof <hrapof@common-lisp.ru>"
    :licence "LGPL"
    :description "Python interface"
    :components
    ((:file "pyffi")))