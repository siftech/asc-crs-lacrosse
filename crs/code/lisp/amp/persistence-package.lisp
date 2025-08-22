(in-package :common-lisp-user)

(defpackage :persistence
  (:use :common-lisp)
  (:export "SAVE"
           "OBJECT-STRING"
	   "RESTORE"
           "RESTORE-FROM-STRING")
  )
