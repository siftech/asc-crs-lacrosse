;;; -------------------------------------------------------------------------
;;; COPYRIGHT START
;;; COPYRIGHT END
;;; -------------------------------------------------------------------------

;;; A super class for tasks created for the Lacrosse agent.

(cl:in-package :fuzzbomb)

(defclass lacrosse-task (docker-task-new)
  ()
  (:default-initargs
	;; in the contest way, much easier to have nf-ccl img run everything
   :container-start-method :in-this-container
  ))

(defmethod initialize-instance :after ((task lacrosse-task) &key)
  (dbug :top "initialize-instance :after ((task lacrosse-task): ~s" task)
  (init-lacrosse-task-in-out-pathnames task)
  )

;; Broken out for splicing into other tasks that may be used in
;; lacrosse context, eg afl-task.
(defun init-lacrosse-task-in-out-pathnames (task)
  (setf (output-pathname task) (parse-namestring (output-dir (target task))))
  (ensure-directories-exist (output-pathname task))
  (setf (input-pathname task) (parse-namestring (input-dir (target task))))
  (ensure-directories-exist (input-pathname task))
  (dbug :target "set (output-pathname task) to: ~s" (output-pathname task))
  (dbug :target "set (input-pathname task) to: ~s" (input-pathname task)))
