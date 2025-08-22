(in-package :common-lisp-user)

(defpackage :excl
  (:use :common-lisp)
  (:export 
   #:run-shell-command
   #:stream-terpri
   #:stream-force-output
   )
  )

(push :excl *features*)
