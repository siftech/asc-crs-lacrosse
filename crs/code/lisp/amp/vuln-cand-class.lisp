;;; -------------------------------------------------------------------------
;;; vuln-cand-class.lisp
;;; - target objects that AMP works on...
;;; -------------------------------------------------------------------------
;;; COPYRIGHT START
;;; COPYRIGHT END
;;; -------------------------------------------------------------------------

(cl:in-package :fuzzbomb)

;;; FIXME   create id
;;; FIXME copy blob to shared dir

(defclass vuln-cand (named-object)
  (
   (id :initarg :id :initform -1 :accessor id
         :documentation "Unique lax id."
         :type int)
   (blob :initarg :blob :initform nil :accessor blob
         :documentation "Path to a blob containing a crashing input."
         :type (or null string))
   (bic :initarg :bic :initform nil :accessor bic
        :documentation "String containing the hash of the BIC."
        :type (or null string))
   (cp-src
    ;;:type string
    :documentation "Path component distinguishing this repo."
    :initform nil
    :accessor cp-src)
   (files-in-commit :initarg :files-in-commit :initform nil :accessor files-in-commit
        :documentation "list of strings naming the files-in-commit of the BIC.")
   (sanitizer-id :initarg :sanitizer-id :initform nil :accessor sanitizer-id
                 :documentation "String containing the sanitizer-id."
;;; DJM Fri 28 Jun 2024 11:29:55 PM CDT
;;; oh jeez, stop with the Fing type decls, this is lisp, lemme do what I want.
;;; In particular, the Fing IDs look like keywords, so just let em. sheesh...I aint got time to bleed

        ;;       :type (or null string)
        )
   (harness-id :initarg :harness-id :initform nil :accessor harness-id
               :documentation "String containing the harness-id."
;;; DJM Fri 28 Jun 2024 11:31:12 PM CDT
;;; Again with the type decls..what, I gotta smack you?
;;             :type (or null string)
        )
   (harness-binary :initarg :harness-binary :initform nil :accessor harness-binary
               :documentation "String containing the harness-binary."
                   :type (or null string))

   ;; submission status - this could be a mixin, but not now
   (submitted-p :initform nil :accessor submitted-p
                :documentation "Set to t after submission.")
   (rejected-p :initform nil :accessor rejected-p
               :documentation "Set to t when rejected.")
   (accepted-p :initform nil :accessor accepted-p
               :documentation "Set to t when accepted.")
   
   (cpv-uuid :initarg cpv-uuid :initform nil :accessor cpv-uuid
             :documentation "String containing the cpv-uuid from capi.  Non-nil indicates acceptance."
             :type (or null string))
   (patch-cands :initarg :patch-cands :initform nil :accessor patch-cands
                :documentation "List of patch-cand objects."
                :type list)

   (submitted-patch :initarg :submitted-patch :initform nil :accessor submitted-patch
                    :documentation "The patch-cand object chosen for submission."
                    ;;:type patch-cand
                    )
   ;; NOT gp-ack ... look at the gp-response in the submitted-patch
   (target :initarg :target :initform nil :accessor target
           :documentation "Ptr to the target and all of its state."
           :type (or null lacrosse-cp-target))
   (shared-data-dir :initarg :shared-data-dir :initform nil :accessor shared-data-dir
               :documentation "String containing path to shared-data-dir."
               ;;:type (or null string)
               )
   ))

(defmethod blob-p ((vuln-cand vuln-cand))
  (not (not (blob vuln-cand))))

(defmethod bic-p ((vuln-cand vuln-cand))
  (not (not (bic vuln-cand))))

(defmethod initialize-instance :after ((vuln-cand vuln-cand) &key)
  ;; Unless id is supplied to make-instance, use named-object name.
  ;; It's intended that only optimus assign the id.
  (when (and (numberp (id vuln-cand))
             (= -1 (id vuln-cand)))
    (setf (id vuln-cand) (name vuln-cand)))
  (let ((shared-data-dir (format nil "~a/vuln-cands/~6,'0d/" (shared-path (target vuln-cand)) (id vuln-cand))))
    (setf (shared-data-dir vuln-cand) shared-data-dir)
    (ensure-directories-exist shared-data-dir)))

(defmethod print-object ((vuln-cand vuln-cand) str)
  (print-unreadable-object (vuln-cand str :type t)
    (format str "[~a] id: ~a  blob: ~a  sanitizer-id: ~a  bic: ~a"
            (name vuln-cand) (id vuln-cand) (blob vuln-cand) (sanitizer-id vuln-cand) (bic vuln-cand))))

(defmethod lp-string ((vuln-cand vuln-cand))
  (format nil "<VULN-CAND: ~a  blob: ~a  sanitizer-id: ~a  bic: ~a>"
          (id vuln-cand) (blob-p vuln-cand) (not (not (sanitizer-id vuln-cand))) (bic-p vuln-cand)))

;;(defmethod shared-blob-dir ((vuln-cand vuln-cand))
;;  (format nil "~a/" *experiment-dir*

(defmethod ready-to-submit-p ((vuln-cand vuln-cand))
  ;;(describe vuln-cand)
  (let ((submitted-bics (submitted-bics (asc-target))))
    (and (bic vuln-cand)
         (sanitizer-id vuln-cand)
         (harness-id vuln-cand)
         (blob vuln-cand)
         (not (submitted-p vuln-cand))
         (not (member (bic vuln-cand) submitted-bics :test #'string-equal))
	 (not (rejected-p vuln-cand))
	 (not (equiv-to-rejected-p vuln-cand))
         )))

(defmethod equiv-to-rejected-p ((vc vuln-cand))
  "Check bic and blob against previously rejected cands.
   Pretty crude, but will keep us from really dumb submits."
  (member vc (remove-if-not #'rejected-p (vuln-cands (asc-target)))
	  :test #'same-bic-and-blob-p))

(defmethod same-bic-and-blob-p ((vc-1 vuln-cand) (vc-2 vuln-cand))
  (and (string-equal (bic vc-1) (bic vc-2))
       (same-file-contents-p (blob vc-1) (blob vc-2))))

(defun consider-submit-vc (target)
  (dbug :top "consider-submit-vc")
  (dbug :top "previously submitted bics: ~s" (submitted-bics target))
  (let ((vc-to-submit (find-if #'ready-to-submit-p (vuln-cands target))))
    (when vc-to-submit
      (let ((cmd-str (format nil "~a/submit-pov.sh \"~a\" ~a ~a ~a ~a ~a ~a" *lax-tools*
                             (cp-prop target :cp_name)
                             (bic vc-to-submit)
                             (sanitizer-id vc-to-submit)
                             (harness-id vc-to-submit)
                             (blob vc-to-submit)
                             (id vc-to-submit)
			     (shared-data-dir vc-to-submit))))
        (dbug :top "submitting vc: ~s" vc-to-submit)
        (dbug :top "  cmd: ~s" cmd-str)
        ;;(uiop:run-program cmd-str :output t :ignore-error-status t)
        ;; async; :output t doesn't work on ccl, but *standard-output* does.
        (uiop:launch-program cmd-str :output *standard-output* :ignore-error-status t)
        (setf (submitted-p vc-to-submit) t)
        ))))

(defmethod find-pc-by-id (id (vuln-cand vuln-cand))
  (find id (patch-cands vuln-cand) :key #'id :test #'=))

(defun submitted-bics (target)
  (iterate (for vc in (vuln-cands target))
    (when (and (bic vc)
               (submitted-p vc)
               (not (rejected-p vc)))
      (adjoining (bic vc) test #'string-equal))))
