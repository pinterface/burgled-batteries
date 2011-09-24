;;; -*- Lisp -*-
(cl:eval-when (:load-toplevel :execute)
  (asdf:load-system "cffi-grovel"))

(defpackage #:pyffi-system (:use #:asdf #:cl #:cffi-grovel))
(in-package #:pyffi-system)

(defsystem :pyffi
    :depends-on (#:cffi #:alexandria #:parse-declarations-1.0)
    :name "pyffi"
    :author "Dmitri Hrapof <hrapof@common-lisp.ru>"
    :maintainer "pinterface <pix@kepibu.org>"
    :licence "LGPL + MIT"
    :description "Python interface"
    :serial t
    :components
    ((:file "packages")
     ;; FIXME: auto-detect location of Python include directory
     (grovel-file "grovel" :cc-flags ("-I/usr/include/python2.6"))
     (:file "ffi-interface")
     (:file "api")))
