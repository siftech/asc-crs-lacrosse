;;; -------------------------------------------------------------------------
;;; amp-class.lisp
;;; - class defn for AMP agent
;;; - $Revision: 1.19 $
;;; -------------------------------------------------------------------------
;;; COPYRIGHT START
;;; COPYRIGHT END
;;; -------------------------------------------------------------------------

(cl:in-package :fuzzbomb)

;;;(declaim (optimize (speed 3) (safety 1)))

(defclass amp ()
  ((name :initform nil :initarg :name :accessor name)
   (shortname :initform nil :initarg :shortname :accessor shortname)
   (init-comm-p :initform nil :accessor init-comm-p)
   (host :initform (or (getenv "DOCKER_HOSTNAME") (getenv "CIRCA_HOST") (hostname)) :accessor host)
   (port :initform nil :initarg :port :accessor port)
   ;; the well-known socket I have opened to accept connections.
   ;; - master AMP accepts from other AMPs
   ;; - all AMPs accept from their respective RTSs.
   (amp-sock :initform nil :accessor amp-sock)
   ;; assoc list that stores the sockets to connect other AMP agents.
   ;; - assoc list ((agentobj socket)...)
   (peer-sockets :initform nil :accessor peer-sockets)
   ;; non-nil if I am optimus prime
   (master-p :initform nil :initarg :master-p :accessor master-p) ; Make 'em say Uhh!
   ;; role is :optimus or :fuzzbomb. Maybe add :master-fuzzbomb if needed.
   (role :initform nil :initarg :role :accessor role)
   ;; if you are master-fb, this is zero.  if you are a slave, it is 1+
	;; this is used in a truly evil way to punch deep through a bunch of call stacks that don't understand slaves or files of diff
	;; targets, so that waaaay deep in the fuzzball wrapper it can just say "oh, i'm a master or slave and I have an index?  Ok, I'll run on
	;; those targets instead of the default full file of all the targets. The master uses the zero index.
   (slave-index :initform nil :initarg :slave-index :accessor slave-index)
   (master :initform nil :initarg :master :accessor master)
   (master-host :initform nil :initarg :master-host :accessor master-host)
   ;; Optimus' lists of unassigned Fuzzbombs and DVMs.
   (unassigned-fuzzbombs :initform nil :initarg :unassigned-fuzzbombs :accessor unassigned-fuzzbombs)
   (unassigned-dvms :initform nil :initarg :unassigned-dvms :accessor unassigned-dvms)
   (cbss :initform nil :initarg :cbss :accessor cbss)
   (targets :initform nil :initarg :targets :accessor targets)
   ;; List of target trees for external targets.
   (target-trees :initform nil :initarg :targets :accessor target-trees)
   ;; assoc of master-fb to its current target.
   (master-fuzzbombs :initform nil :initarg :master-fuzzbombs :accessor master-fuzzbombs)
   ;; assoc of master fuzzbombs to list of all targets
   (master-fuzzbomb-tasks :initform nil :initarg :master-fuzzbomb-tasks :accessor master-fuzzbomb-tasks)

   ;; list of (target task) entries that have been assigned to this FB AMP by OPT
   ;; - pushed on when assigned.  Used for possible reassignment when an AMP dies.
   (lacrosse-tasks :initform nil :initarg :lacrosse-tasks :accessor lacrosse-tasks)
   
   ;; Maybe split Fuzzbomb and Optimus into amp subclasses? They can
   ;; still share main loop, but won't have each others' slots.
   ;; I am the master fuzzbomb for this CBS.
   (svc :initform nil :initarg :svc :accessor svc)
   (repair-matrix :initform nil :initarg :repair-matrix :accessor repair-matrix)
   (dvms :initform nil :initarg :dvms :accessor dvms)	;; this holds shortnames not amp objs; they are in *dvms*
   (pending-test-cases :initform nil :initarg :pending-test-cases :accessor pending-test-cases)	;; for DVMs, a list of (svc-rev . test-case-list)
   (slave-fuzzbombs :initform nil :initarg :slave-fuzzbombs :accessor slave-fuzzbombs)
   ;; Alist of tasks subscribed to test results for different svc-revs
   (svc-rev-subscriptions :initform nil :initarg svc-rev-subscriptions :accessor svc-rev-subscriptions)
   ;; Tracking repair tasks, to avoid redundant work.
   (repair-tasks :initform nil :initarg :repair-tasks :accessor repair-tasks)
   ;; default amp uses matchmaker, instances or specializations may not
   (matchmaker-p :initform t :initarg :matchmaker-p :accessor matchmaker-p)
   (mission :initform nil :accessor mission)

   ;; List of local agent's awarded/accepted contracts
   ;; In the case of master, all contracts.
   (contracts :initform nil :accessor contracts)
   (tasks :initform nil :accessor tasks)
   (current-task :initform nil :accessor current-task)
   (cur-quantum :initform nil :accessor cur-quantum)

   (bridge-socket :initform nil :accessor bridge-socket :documentation "Socket for connection to bridge.")
   (bridge-port :initarg :bridge-port :initform 2500 :accessor bridge-port :documentation "The port we expect the bridge to be listening on -- will be diff for diff agents")

   (threat-skills :initform nil :initarg :threat-skills :accessor threat-skills
                  :documentation "List of threat-skill objects.")
   (goal-skills :initform nil :initarg :goal-skills :accessor goal-skills
                :documentation "List of goal-skill objects.")

   ;;
   (actions-for-threats :initform nil :initarg :actions-for-threats :reader actions-for-threats)
   (actions-for-goals :initform nil :initarg :actions-for-goals :reader actions-for-goals)
   )
  )

;;;-------------------------------------------------------------------------
(defmethod initialize-instance :after ((a amp) &key actions-for-threats actions-for-goals)
  (when (null (port a))
    (error "Must define a unique port for ~A" a))

  (when (not (stringp (host a)))
    (error "Must define a string hostname for ~A" a))

  ;; Shouldn't pass actions-for-threats or actions-for-goals, if backward-compat flag isn't set.
  (when (and (not *use-old-phase-level-skill-code*)
             (or actions-for-threats actions-for-goals))
    (error "actions-for-threats (~s) or actions-for-goals (~s) was set, but *use-old-phase-level-skill-code* was nil." actions-for-threats actions-for-goals))

  ;; process actions-for-threats and actions-for-goals initargs, if they were used.
  (dolist (aft actions-for-threats)
    (destructuring-bind (threat torms) aft
      (let ((threat-skill (make-instance 'threat-skill
                                         :threat threat
                                         :torm torms)))
        (push threat-skill (threat-skills a)))))

  (dolist (afg actions-for-goals)
    (destructuring-bind (goal torms) afg
      (let ((goal-skill (make-instance 'goal-skill
                                       :goal goal
                                      :torm torms)))
        (push goal-skill (goal-skills a)))))

  (push a *amps*)
  )

(defmethod num-povs ((amp amp))
  (reduce #'+ (targets amp)
          :key #'num-povs))

(defun find-target-by-id (id)
  (declare (special *self*))
  (find id (targets *self*) :key #'id :test #'=))

;;;------------------------------------------------------------------------
;;; N is determined by position in the global *amps* list
(defun make-amp (shortname
                 &key master-p role actions-for-threats actions-for-goals (matchmaker-p T)
                      (amp-class 'amp))
  (setf shortname (string-left-trim ":" (string-upcase shortname)))
  (when (getenv "CIRCA_PORT")
    (setf *amp-port* (parse-integer (getenv "CIRCA_PORT"))))
  (dbug :top "AMP port set to ~A when creating amp" *amp-port*)

  (let ((amp (make-instance amp-class
               :master-p master-p
               :role role
               :actions-for-threats actions-for-threats
               :actions-for-goals actions-for-goals
               :shortname shortname
               :matchmaker-p matchmaker-p
               :name (make-full-name shortname)        ;; see base.lisp
               :port (or *amp-port* (+ *circa-baseport* (* 10 *num-amps*)))
               :bridge-port (or *bridge-port* (+ *circa-baseport* (* 10 *num-amps*) 9)))))
    (incf *num-amps*)
    amp))

(defun amp-p (x)
  (typep x 'amp))

(defmethod optimus-p ((obj amp))
  (eq (role obj) :optimus))

(defmethod fuzzbomb-p ((obj amp))
  (eq (role obj) :fuzzbomb))

(defmethod slave-fuzzbomb-p ((obj amp))
  (and (eq (role obj) :fuzzbomb) (> (slave-index obj) 0)))

(defmethod slave-p ((obj amp))
  (slave-fuzzbomb-p obj))

(defmethod master-fuzzbomb-p ((obj amp))
  (and (eq (role obj) :fuzzbomb) (= (slave-index obj) 0)))

(defmethod dvm-p ((obj amp))
  (eq (role obj) :dvm))

(defmethod optimus-prime-p ((obj amp))
  (master-p obj))

(defmethod lp ((obj amp))
  (format T "#<AMP ~A>~%" (name obj)))

(defmethod print-object ((obj amp) stream)
  (format stream "#<AMP ~A>" (name obj)))

(defmethod dbug-target-list ((amp amp))
  (dbug :top "dbug-target-list")
  (dolist (target (targets amp))
    (dbug :top "~a" target)))

(defmethod get-target-by-id ((amp amp) target-id)
  (find target-id (targets amp) :key #'id))

;;;-------------------------------------------------------------------------
;;; returns amp object.
;;; - if the name has no dashes, then we know it hasnt been made
;;;     into the full name w/ base/etc added, so try that...
;;;
;;; [jrye:20111209.1018CST] Added a find call with the NAME exactly as
;;; passed. The AMP was failing to respond to contracts because it was
;;; trying to look up AMPs from a name like MASTER-AMP-jrye.

(defmethod find-amp ((name string))
  ;;(dbug :top "name is ~A~%*amps* are ~A" name *amps*)
  ;;(dbug :top "fullname will be ~A" (make-full-name name))
  ;;(dolist (a *amps*) (dbug :top "amp ~A has name ~A" a (name a)))
  (or (find name *amps* :key #'name :test #'string=)
      (find (string-upcase name) *amps* :key #'name :test #'string=)
      (find (make-full-name name) *amps* :key #'name :test #'string=)))


(defmethod find-amp ((n null))
  nil)


(defmethod clear-skills ((amp amp))
  (setf (threat-skills amp) nil)
  (setf (goal-skills amp) nil)
  (when (mission amp)
    (clear-skills (mission amp))))

