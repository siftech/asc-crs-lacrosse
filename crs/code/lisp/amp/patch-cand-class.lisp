;;; -------------------------------------------------------------------------
;;; patch-cand-class.lisp
;;;
;;; -------------------------------------------------------------------------
;;; COPYRIGHT START
;;; COPYRIGHT END
;;; -------------------------------------------------------------------------

(cl:in-package :fuzzbomb)

(defclass patch-cand (named-object)
  (
   (id :initarg :id :initform -1 :accessor id
       :documentation "Unique lax id."
       :type int)
   (patch-file :initarg :patch-file :initform nil :accessor patch-file
               :documentation "String containing path to patch file."
               :type (or null string))
   (vuln-cand :initarg :vuln-cand :initform nil :accessor vuln-cand
              :documentation "Ptr to parent vuln-cand."
              :type (or null vuln-cand))
   (target :initarg :target :initform nil :accessor target
           :documentation "Ptr to the target and all of its state."
           :type (or null lacrosse-cp-target))
   (shared-data-dir :initarg :shared-data-dir :initform nil :accessor shared-data-dir
                    :documentation "String containing path to shared-data-dir."
                    ;;:type (or null string)
                    )
   
   ;; submission status - this could be a mixin, but not now
   (submitted-p :initform nil :accessor submitted-p
                :documentation "Set to t after submission.")
   (rejected-p :initform nil :accessor rejected-p
               :documentation "Set to t when rejected.")
   (accepted-p :initform nil :accessor accepted-p
               :documentation "Set to t when accepted.")
   
   (gp-uuid :initarg gp-uuid :initform nil :accessor gp-uuid
            :documentation "String containing the gp-uuid from capi."
            ;;:type (or null string)
            )
   ))

(defmethod initialize-instance :after ((patch-cand patch-cand) &key)
  ;; Unless id is supplied to make-instance, use named-object name.
  ;; It's intended that only optimus assign the id.
  (when (and (numberp (id patch-cand))
             (= -1 (id patch-cand)))
    (setf (id patch-cand) (name patch-cand)))
  (let ((shared-data-dir (format nil "~a/patch-cands/~6,'0d/" (shared-data-dir (vuln-cand patch-cand)) (id patch-cand))))
    (setf (shared-data-dir patch-cand) shared-data-dir)
    (ensure-directories-exist shared-data-dir))
  )

(defmethod lp-string ((patch-cand patch-cand))
  (format nil "<PATCH-CAND: ~a  vcid: ~a: file: ~a>" (id patch-cand) (id (vuln-cand patch-cand)) (patch-file patch-cand)))

(defmethod ready-to-submit-p ((patch-cand patch-cand))
  "Does this patch-cand meet the min reqts for submission?"
  (dbug :top "testing ready-to-submit-p: ~s" patch-cand)
  ;;(describe patch-cand)
  (and
   (cpv-uuid (vuln-cand patch-cand))
   (patch-file patch-cand)
   (not (submitted-patch (vuln-cand patch-cand)))
   (not (rejected-p patch-cand))
   (not (equiv-to-rejected-p patch-cand))
   ))

(defmethod equiv-to-rejected-p ((pc patch-cand))
  (member pc (remove-if-not #'rejected-p (all-patch-cands (asc-target)))
	  :test #'same-patch-p))

(defmethod same-patch-p ((pc-1 patch-cand) (pc-2 patch-cand))
  (same-file-contents-p (patch-file pc-1) (patch-file pc-2)))

(defun consider-submit-patch (target)
  (dbug :top "consider-submit-patch")
  ;;(describe target)
  ;;(describe (first (vuln-cands target)))
  (let ((patch-to-submit (find-if #'ready-to-submit-p (all-patch-cands target))))
    (dbug :top "patch-to-submit: ~s" patch-to-submit)
    (when patch-to-submit
      (setf (submitted-patch (vuln-cand patch-to-submit)) patch-to-submit)
      (let ((cmd-str (format nil "~a/submit-patch.sh ~a ~a ~a ~a ~a"
			     *lax-tools*
                             (cpv-uuid (vuln-cand patch-to-submit))
                             (patch-file patch-to-submit)
                             (id (vuln-cand patch-to-submit))
                             (id patch-to-submit)
			     (shared-data-dir patch-to-submit)
                             )))
        (dbug :top "submitting gp: ~s" patch-to-submit)
        (dbug :top "  cmd: ~s" cmd-str)
        ;;(uiop:run-program cmd-str :output t :ignore-error-status t)
        ;; async; :output t doesn't work on ccl, but *standard-output* does.
        (uiop:launch-program cmd-str :output *standard-output* :ignore-error-status t)
        ))))
