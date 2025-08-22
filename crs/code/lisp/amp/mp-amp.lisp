;;; -------------------------------------------------------------------------
;;; mp-amp.lisp
;;; - multiprocessing ver of AMP inner loop, allows CSM to run in parallel
;;; yet remain interruptible by incoming state msg from RTS (or potentially
;;; other interrupts/notifications).
;;; - $Revision: 1.8 $
;;; -------------------------------------------------------------------------
;;; COPYRIGHT START
;;; COPYRIGHT END
;;; -------------------------------------------------------------------------

(cl:in-package :fuzzbomb)

;;(use-package :multiprocessing)
#+allegro
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :process))

;;; -------------------------------------------------------------------------
(defvar *stars-printed* 0)
(defvar *max-stars-to-wait* (* 20 60))	;; this is approx multiples of *heartbeat-period*, at .75 approx 1 per sec, ...so, currently approx 20 min

(defun mp-run-amp-inner-loop ()
  (declare (special *stars-printed* *self*))
  (let ((sent-idle-msg t))     ; don't send idle-msg until *after* initial tasking
    (loop until *halt*
       do
         (multiple-value-bind (task score) (mp-select-task)
           (cond ((and task (> score 0))
                  (dbug :top "Task queue: ~a" (tasks *self*))
                  (dbug :top "Selected ~A from ~A tasks, score = ~A" task (length (tasks *self*)) score)
		  (setf *stars-printed* 0)
                  (setf (tasks *self*) (delete task (tasks *self*)))
                  (dont-error (execute-task task))
                  (setf sent-idle-msg nil)
                  (dont-error (process-all-msgs))
                  )
                 ((and (not *delib-task*)
                       *send-idle-msg*
                       (not sent-idle-msg))
                  (dbug :top "Sending :idle msg to optimus.")
                  (send-message-to-optimi :type :idle)
                  (setf sent-idle-msg t))
                 (t
                  (if (mp:wait-for-input-available *sockets* :timeout *heartbeat-period*)
                      (dont-error (progn (setf *stars-printed* 0) (process-all-msgs)))
                      (progn
                        (cond ((integerp *dot-heartbeat*)
                               (print-heartbeat)
                               (incf *dot-heartbeat*)
                               (when (zerop (rem *dot-heartbeat* 30)) (format t "~%")(force-output))
                               )
			      (*dot-heartbeat*
                               (print-heartbeat)))
                        (when (and *delib-task* *delib-process-suspended*)
                          (dbug :top "Pushing delib-task ~A back on tasks list" *delib-task*)
                          (pushnew *delib-task* (tasks *self*))	;; see whether should resume delib task
                          ;; we do that by putting it on the task list so that if other tasks appear, they all get sorted properly.
                          )
			))))))))

;;; -------------------------------------------------------------------------

(defun print-heartbeat ()
  (declare (special *self*))
  ;;(dont-error (check-test-queue))
  ;; (defensive-rewrite-update-reload)
  (cond ((has-pending-work *self*)
      	 (format t "*")
         ;;(dont-error (check-for-old-test-batches))
	 (incf *stars-printed*)			;; if we have pending stuff, wait a bit
   	 (when (> *stars-printed* *max-stars-to-wait*)
		(setf *stars-printed* 0)
		(dont-error (update-pending-work-when-idle)))
	 )
	(T
      	 (format t ".")
   	 (dont-error (update-pending-work-when-idle))	;; if no pending, update right away
	)
  ))


;;; -------------------------------------------------------------------------

(defun has-pending-work (amp)
  (declare (ignore amp))
  (declare (special *all-revs*))
  (any #'pending-test-cases *all-revs*)
)

;  (declare (special *is-this-best-task*))
;  (and (svc *self*)
;       (or (pending-test-cases (original-rev (svc *self*)))
;	   (any #'pending-test-cases (find-all-instances 'svc-rev))))
;  (or (and (pending-tests *is-this-best-task*) (any #'second (pending-tests *is-this-best-task*)))

;;(trace has-pending-work)

;;; -------------------------------------------------------------------------
;;; If there is already/still a current task, select it.
;;; If there are no tasks, nil.
;;; Otherwise, pick the one with the highest incremental utility.

(defun mp-select-task ()
  (declare (special *self*))
  (cond ((current-task *self*))
        ((null (tasks *self*)) nil)
        (T
         (dbug :delib-trace "All tasks are ~A" (tasks *self*))
         (let ((tasks (cond (*delib-task*
                             (remove-if #'deliberation-task-p (tasks *self*)))
                            (t
                             (tasks *self*)))))
           (dbug :delib-trace "Tasks under consideration are ~A" tasks)
           (dbug :delib-trace "Scores are ~A" (mapcar #'scoring-fn tasks))
           (rank-and-choose #'scoring-fn #'max tasks)))))

;;; -------------------------------------------------------------------------
