;;;; Adds the ability to grovel bitfields to cffi-grovel.  CFFI is welcome to
;;;; use this code.  It's just copy-pasta from cenum anyway.
(in-package #:cffi-grovel)

(define-grovel-syntax bitfield (name &body masks)
  (destructuring-bind (name &key base-type)
      (ensure-list name)
    (c-section-header out "bitfield" name)
    (c-export out name)
    (c-format out "(cffi:defbitfield (")
    (c-print-symbol out name t)
    (when base-type
      (c-printf out " ")
      (c-print-symbol out base-type t))
    (c-format out ")")
    (dolist (mask masks)
      (destructuring-bind (lisp-name c-name)
          mask
        (check-type lisp-name keyword)
        (check-type c-name string)
        (c-format out "~%  (")
        (c-print-symbol out lisp-name)
        (c-format out " ")
        (c-printf out "#x%08x" c-name)
        (c-format out ")")))
    (c-format out ")~%")))
