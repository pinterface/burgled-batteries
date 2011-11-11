;;; burgled-batteries.asd --- ASDF system definition for burgled-batteries
;;;
;;; License ::
;;; * LGPL: Portions copyright Dmitri Hrapof <hrapof@common-lisp.ru>
;;;   burgled-batteries began life as patches to pyffi.  The parts of pyffi
;;;   which remain (mostly in api.lisp) are copyright Dmitri Hrapof and licensed
;;;   under the LGPL.
;;; * MIT: Anything by pinterface <pix@kepibu.org>
;;;   Anything I've written which isn't a trivial modification of something from
;;;   pyffi is licensed under the MIT license.

#-burgled-batteries.guess-not-grovel
(cl:eval-when (:load-toplevel :execute)
  (asdf:load-system "cffi-grovel"))

(defpackage #:burgled-batteries-system (:use #:asdf #:cl #-burgled-batteries.guess-not-grovel #:cffi-grovel))
(in-package #:burgled-batteries-system)

(defsystem :burgled-batteries
    :depends-on (#:cffi #:alexandria #:parse-declarations-1.0 #:trivial-garbage)
    :name "burgled-batteries"
    :author (:original "Dmitri Hrapof <hrapof@common-lisp.ru>"
             :current "pinterface <pix@kepibu.org>")
    :licence "LGPL + MIT"
    :description "Lisp-Python interface"
    :long-description "
This system provides support for embedding Python into Common Lisp via CFFI.

It uses cffi-grovel to determine sizes and values of some assorted Python types
and constants.  However, if you'd rather avoid this, or grovelling is not
possible for you, a best-guess effort can also be made.  You can note your
preference for guessing by evaluating the following form before telling ASDF to
load this system:
  (push :burgled-batteries.guess-not-grovel *features*)

If you /would/ like to use the groveller, you may need to edit the .ASD file to
configure the directory where Python header files are located.  (My apologies: I
hope for this to be both more easily configurable, and less necessary, in the
future.)
"
    :serial t
    :components
    ((:file "packages")
     #-burgled-batteries.guess-not-grovel (:file "grovel-bitfields")
     ;; FIXME: auto-detect location of Python include directory (or at least make it configurable)
     #-burgled-batteries.guess-not-grovel (grovel-file "grovel" :cc-flags ("-I/usr/include/python2.6"))
     #+burgled-batteries.guess-not-grovel (:file "grovel-guess")
     (:file "cffi-output-args")
     (:file "ffi-definers")
     (:file "ffi-interface")
     (:file "ffi-conditions")
     (:file "ffi-callbacks")
     (:file "api")))
