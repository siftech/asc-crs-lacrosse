;;; -------------------------------------------------------------------------
;;; amp.lisp
;;; - main driver file for AMP
;;; - $Revision$
;;; -------------------------------------------------------------------------
;;; COPYRIGHT START
;;; COPYRIGHT END
;;; -------------------------------------------------------------------------

(cl:in-package :fuzzbomb)

;;(foobarski)   ;; uncomment this undefined fn to show that asdf will catch it as an error and puke.

;;(let (foobarski))  ;; uncomment this unused vble to show that asdf will catch it as a warning and puke.

;;(use-package :multiprocessing)
#+allegro
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :process))

;;; -------------------------------------------------------------------------
;(docfile "
;The main AMP loop (the main outer loop of all agents).
;Includes process-msg methods for handling different types of messages that arrive at AMP
;from other AMPs and the bridge.
;Legacy elements include handling contract-net-based negotiation of which agent handles a 'task',
;queueing and selection of 'tasks' for local blocking execution.
;" :module "AMP")

;;; -------------------------------------------------------------------------
(debug-list-value :top "For top level information.")
(debug-list-value :amp "For information about top-level running of Adaptive Mission Planner.")
(debug-list-value :result "For results that we may want to review in summary or test comparo.")
(debug-list-value :reload "Reloading results from a prior run.")
(debug-list-value :reload-deep "Reloading results from a prior run.")
(debug-list-value :triage "For info regarding target triage.")
(debug-list-value :delib "For information about AMP deliberation scheduling.")
(debug-list-value :delib-trace "For detailed information about AMP deliberation scheduling.")

(pushnew :top *debug-list*)
(pushnew :result *debug-list*)
(pushnew :amp *debug-list*)

(defvar *init-task-limit-per-fuzzbomb* 1
  "If non nil, assign at most *init-task-limit-per-fuzzbomb* tasks to a fuzzbomb agent at startup.")

;; Global for deciding whether or not to use the PCAPs, default to true.
(defvar *use-pcaps* t)

(defvar *event-start-time* nil)

(defvar *stop-sending-povs-to-master* nil)      ;; FIXME set up msg to set this when master has enuf PoVs

(defvar *max-fuzzball-povs* 100 "If we make more than this many povs from fuzzball, stop! At least until scoring quiesces")
(defvar *num-fuzzball-povs* 0 "counter")

(defvar *suspended-by-master* nil)

;;; -------------------------------------------------------------------------
;;; -------------------------------------------------------------------------

(defvar *fuzzball-task* nil)

;;; -------------------------------------------------------------------------
(defun amp-dbug-prefix-fn (keys)
  (declare (ignore keys))
  (update-time)
  (format nil "~A [Q ~A]:" *program-name* *cur-quantum*))

(setf *debug-prefix-function* #'amp-dbug-prefix-fn)
;;; -------------------------------------------------------------------------

(defvar *process-msgs-task* (make-instance 'process-msgs-task))
;;; -------------------------------------------------------------------------
(defvar *pereti-workflows-completed* 0)
(defvar *total-pereti-targets* 0)

;;; -------------------------------------------------------------------------
;;; this is the main outer loop of AMP... all AMP computations other than
;;; transient startup stuff must run w/in the dynamic scope of the specials
;;; (re)defined here.
;;; -------------------------------------------------------------------------
;;; AMP outline:
;;; - The AMP does two things: executes tasks and processes messages.
;;; - Tasks encapsulate non-trivial computations that it should be able
;;;     to perform deliberation management over.  Message processing should
;;;     be only trivial/negligible computations.
;;;
;;; Task processing:
;;; - The list of not-completed Tasks are stored in the (tasks *self*) slot.
;;; - At any time, the AMP can be actually executing/attending to only one
;;;     task, altho tasks may be interleaved; the status and partial progress
;;;     on a task should be stored in the task object.
;;; - For now, tasks lock the AMP "pseudo processor" from doing other tasks
;;;     while they are being attended to: the AMP chooses a single
;;;     (current-task *self*) and spends a certain limit of time on it,
;;;     either completing it or not, and hence either moving it off of
;;;     (tasks *self*) or not.  A chosen current task may not give up the
;;;     AMP pseudo processor, and may thus retain its attention for the next
;;;     deliberation increment, if it has the "no-scheduling" flag set.
;;;     - That last part is not implemented that way yet...
;;;
;;; Message processing:
;;; - when get a phase msg, this means it contains a default phase plan
;;;     description that is not up for bids, but is mandatory; put it on
;;;     the task agenda, run CSM on it eventually, generate plan, return
;;;     ack or description of failure.
;;; - when get a contract announcement, create a bid and send back bid msg.
;;; - when get an award msg, run CSM on newly-created phase description
;;;   problem configuration

;;; can we combine any of the stuff of tasks and contracts?  This would make
;;; life a little easier to keep track of... the diff status fields etc...
;;; - well, contracts can be tasks, but there can be tasks that are not
;;;     about contracts... so what if we just allow a contract to be put
;;;     onto the (tasks *self*) list if it has something to be done to it,
;;;     and then (execute-task) can be specialized on a contract to do the
;;;     right thing.  OK, good idea, that's the way it is now.

;;; The AMP always executes a task if one is available.  If select-task returns
;;; NIL, then we see if any messages are available.  If not, then we
;;; wait until a msg comes in; in the current AMP form, msgs are only way
;;; for new things to do to arrive, except for the bootstrapping/init.

;;; The sockets list that we wait on should be dynamic/global/special, so that
;;; the processing of a quit msg from RTS can remove the RTS sock
;;; from the list...  at least at testing time, this is nice so that
;;; the AMP/CSM keep doing right thing (not polling) even when RTS dies.

(defun run-amp (&key (name nil) (domain nil) (domain-dir nil))
  (dbug :top "Name (from keyword): ~A" name)
  (when (not name)
    (setf name (get-command-line-argument 1))
    (dbug :top "Name (from command line): ~A" name))

  ;; [mboldt:20140926] Removing domain file business for CGC. Instead,
  ;; use doint-generated code and experiment.lisp.
  (when (or domain domain-dir)
    (warn "Domain set, but we don't do anything with it! domain: ~A domain-dir: ~A"
          domain domain-dir))

  (unwind-protect (run-amp-1 :name name :domain domain :domain-dir domain-dir)
    ; (tell-aid-I-died)
    (deinit-comm *self* 'unwind-protect)))

;; Just like the (r) in CSM is lazy way to start run, (a) is for AMP.
(defun a () (run-amp))

(defun run-amp-1 (&key (name nil) (domain nil) (domain-dir nil))
  (declare (special *reload-file* *run-fake-bridge* *connect-to-bridge*))
  (init-globals)
  (dbug :result "Optimus is alive!")
  (dbug :timeline "BEGIN initialization.")
  (print-flags)
  (setf *cur-quantum* 0)
  (when *self*
    (deinit-comm *self* 'already-had-self))

  (setf *master-amp* (find-if #'master-p *amps*))

  (setf *self* (which-agent-am-i name))

  (when (and *run-fake-bridge* (not (optimus-prime-p *self*)))
    (dbug :top "NOTE, disabling fake-bridge b/c I'm not OPTIMUS and he's usually only one who gets those msgs")
    (setf *connect-to-bridge* nil)
    (setf *run-fake-bridge* nil))

  (musliner:while (not (init-comm-p *self*))
    (dont-error (init-comm *self*))
    (when (not (init-comm-p *self*))
      ;; Wait and try again if init-comm fails.
      (sleep 1)
      (dbug :top "Retrying init-comm.")))

  (setf *halt* nil)
  ;;(unless *reboot-in-progress* (setf *rts-sock* nil))
  (setf *aid-sock* nil)
  (setappend *sockets* (mapcar #'second (peer-sockets *self*)))

  (init-resources)

  (when (optimus-prime-p *self*) (precomputations))

  (let* (
         ;; dynamic scope of this name used by dbug
         (*program-name* (name *self*))
         )

    ;;(setf *current-rts-sched-slot* 0)
    ;;(setf *rts-current-index-number* -1)
    ;;(setf *downloaded-canned-taps* nil)
    (when *use-stop-amp* (push (make-instance 'stop-amp-task) (tasks *self*)))

    (sort-amp-contracts)
    (when *ask-to-start* (synchronize-amp-startup))

    ;; this is triv way to sync clocks... could do inside sync call
    ;; above for more accuracy.
    (set-zero-time)

    (dbug :top "Atomic batteries to power, turbines to speed... ready to move out!")
    (when (optimus-prime-p *self*)
        (dbug :top "Creating forbidden-timestamp")
        (run-shell-command "cp -f /crs_scratch/timestamp /crs_scratch/forbidden-timestamp"))
    (dbug :timeline "END initialization.")
    (mp-run-amp-inner-loop)

    ;; When mp-run-amp-inner-loop exits, it's time to clean up and exit.
  (when (dvm-p *self*)
     (dbug :top "This is the end, killing brute and run-test")
     (run-shell-command "pkill --signal 9 -f brute")
     (run-shell-command "pkill --signal 9 -f run-test"))

    (deinit-comm *self* *halt*)
    (when *reboot-in-progress*
      (dbug :top "Rebooting AMP...")
      (dolist (s *sockets*)
        (dbug :amp-deep "Remaining socket: ~A to ~A" (local-port s) (remote-port s)))
      (run-amp-1 :name name :domain domain :domain-dir domain-dir))
    ))

(defun init-globals ()
  "Initialize globals that depend on runtime environment."
  (declare (special cl-user::*amp-src-directory*))

  (unless (and (getenv "CIRCA_BASEPORT")
               (stringp (getenv "CIRCA_BASEPORT"))
               (numberp (read-from-string (getenv "CIRCA_BASEPORT"))))
    (error "You must set CIRCA_BASEPORT to a number to load/compile/run the AMP."))

  (setf *circa-baseport* (read-from-string (getenv "CIRCA_BASEPORT")))
  (setf *circa-basename* (getenv "CIRCA_BASENAME"))
  (setf *matchmaker-host* (or (getenv "CIRCA_MM_HOST") "localhost"))
  (setf *matchmaker-port* (+ *circa-baseport* 2))

  (when *use-afl-via-driller*
        (dbug :top "Mapping *use-afl-via-driller* to zero-driller-thread version of *use-driller*")
        (setf *use-driller* t)
        (setf *driller-timeout* 10800)
        (setf *driller-workers* 0)
        (setf *driller-force-interval* 10800)
        (setf *driller-more-args* "--no-dictionary --avoid-cfg")
        )

  (setf *amp-directory*
    (cond ((getenv "CIRCA_HOME")
           (merge-pathnames (make-pathname :directory '(:relative "code" "amp"))
                            (make-pathname :directory (getenv "CIRCA_HOME"))))
          (t (warn "CIRCA_HOME not set, defaulting *amp-directory* to *default-pathname-defaults*.")
             *default-pathname-defaults*)))
  (setf *neo-fuzz-home*
        (cond ((uiop:getenv "NEO_FUZZ_HOME"))
              (t (asdf:system-relative-pathname :fuzzbomb "../../"))))

  (setf *lax-home* "/lacrosse/")
  (setf *lax-tools* (format nil "~acode/tools/" *lax-home*))
  (setf *cp-root* (getenv "AIXCC_CP_ROOT"))
  (setf *crs-scratch* (getenv "AIXCC_CRS_SCRATCH_SPACE"))
  )

;;; -------------------------------------------------------------------------
(defun update-meters ()
  (when (and *run-aid* *aid-meters*)
    (dbug (:delib-trace :aid) "Update meters:")
    (dolist (phase (phases (mission *self*)))
      (dbug :delib-trace "~A survival ~A" (name phase) (* 100 (survival-probability phase)))
      ;(aid-set-meter (strcat (name phase) "-S") (* 100 (survival-probability phase)))
      (dbug :delib-trace "~A reward ~A" (name phase) (reward phase))
      ;(aid-set-meter (strcat (name phase) "-R") (reward phase))
        )
    (dbug :delib-trace "PAYOFF ~A" (expected-future-payoff nil *cur-quantum*))
    ;(aid-set-meter "PAYOFF" (expected-future-payoff nil *cur-quantum*))
))

;;; -------------------------------------------------------------------------
;;; This fn behaves different if you are master vs. other;
;;; If you are master, prompt user for a keystroke input to indicate
;;;     that the whole shebang (all AMPs) should start thinking about tasks.
;;;     - then send msg to all non-masters telling them to go, and return.
;;; If you are non-master, wait until get appropriate msg from master,
;;;     then return.

(defun synchronize-amp-startup ()
  (cond ((master-p *self*)
         (format t "---------------------------------------------~%")
         (format t "~%~%Enter anything to proceed...~%~%")
         (format t "---------------------------------------------~%")
         (read-line)
         (broadcast-message :type :sync))
        (T
         (setf *amp-paused* T)
         (musliner:while *amp-paused*
           (mp:wait-for-input-available (list (socket-to (shortname *master-amp*))))
           (process-all-msgs)))))

;;; -------------------------------------------------------------------------
;;; - contracts go through several :status modes:
;;;     :new means they are brand new, unannounced etc.
;;;     :announced means we have announced them, and have gotten all
;;;             bids (when we get last expected bid msg, put contract on tasks)
;;;     :awarded means we have gotten all bids, made decision and awarded to
;;;             a particular contractor.

;;; -------------------------------------------------------------------------
;;; we dispatch execution action based on status of contract.
;;; - note contracts that are awarded to us are executed in
;;;     process-award-msg

(defmethod execute-task ((c contract))
  (dbug :top "Executing task operation for ~A" c)
  (ecase (status c)
    ;; new contract is announced and moved off of active
    ;; list; it will be moved back on when all bids rcvd.
    (:new
     (announce-contract c)
     )
    (:announced
     (award-contract c)
     )
    ;; Contract terms not met; must be reannounced.
    (:failed
     ;; Restore fresh context
     (dbug :top "Revisiting failed contract with ~A" c)
     (setf (contractor c) nil)
     (setf (bidders c) nil)
     (setf (bids c) nil)
     (setf (status c) :new)
     ;; And announce (good as :new!)
     (announce-contract c)
     )
;;;       (:accepted
;;;         (execute-contract c))
    ))

;;; -------------------------------------------------------------------------
(defmethod announce-contract ((c contract))
  (dbug :top "Announcing ~A" c)
  (broadcast-message :type :announcement
                     :contract-name (name c)
                     :contract-type (if (eq (type-of c) 'threat-contract)
                                        :threat
                                      :goal)
                     :value (list (phasename c)
                                  (if (eq (type-of c) 'threat-contract)
                                      (threat c)
                                    (goal c))))
  (setf (status c) :announced)
  ;(aid-set-contract-flag c :ANNOUNCED)
  ;; if we are the only AMP, we've "gotten all bids" so move on...
  (when (= (length *amps*) 1)
    (dbug :delib "Awarded to self, pushing ~A back on tasks" c)
    ;;(aid-set-contract-flag c :MINEUNPLANNED)
    (push c (tasks *self*)))
  )

;;; -------------------------------------------------------------------------
;; this wasnt good to use b/c the end time for phase is just initial
;; estimate, and only RTS can tel lyou when you really transition to new phase
;; (in ACM/UAV domains, based on phase-labeled waypoints).

;;;(defun in-the-past (thing)
;;;  (< (end-time thing) *cur-quantum*))

;;; -------------------------------------------------------------------------
;; this is not used now; execution of plan-config-task
;; downloads as soon as finished.
;; - could eventually upgrade to this decision-based downloading.

;;;(defmethod execute-task ((task download-tap-plan))
;;;  (dbug :top "Executing download-tap-plan... ")
;;;  (let* ((plan (plan task))
;;;        (config (config plan))
;;;        (mission-phase (mission-phase config))
;;;        (mission (mission mission-phase))
;;;       )
;;;
;;;  (dbug :top "Executing download-tap-plan... for:")
;;;  (dbug :top " Plan ~A ..." plan)
;;;  (dbug :top " Config ~A ..." config)
;;;  (dbug :top " Mission-phase ~A ..." mission-phase)
;;;  (dbug :top " Mission ~A ..." mission)
;;;))

;;; -------------------------------------------------------------------------
;; General task catcher.

(defmethod execute-task ((task task))
  (error "Don't know how to execute generic task ~A...~%" task)
  )

;;; -------------------------------------------------------------------------
(defun process-all-msgs (&aux (heard-something T))
  (dbug :amp-deep "process-all-msgs")
  (mp:without-scheduling
    (musliner:while heard-something
      ;;(dont-error (check-test-queue))
      (setf heard-something nil)
      (dolist (s *sockets*)
        (dbug :amp-deep "Processing socket: ~A to ~A" (local-port s) (remote-port s))
        (when (listen s)
          (setf heard-something T)
          (dont-error (process-msg (get-msg s) s)))
          ;; (when (and *self* (svc *self*)) (flooded-p))       ;; for now we check after every msg is processed; could do it just for ones that increase flooding possibility
          )
      )))

;;; -------------------------------------------------------------------------
(defun process-msgs (&optional (max-msgs 100) &aux (i 0) (heard-something T))
  (dbug :amp-deep "process-msgs, max ~A" max-msgs)
  (mp:without-scheduling
    (musliner:while (and heard-something (< i max-msgs))
      (setf heard-something nil)
      (dolist (s *sockets*)
        (dbug :amp-deep "Processing socket: ~A to ~A" (local-port s) (remote-port s))
        (when (listen s)
          (setf heard-something T)
          (incf i)
          (dont-error (process-msg (get-msg s) s))))
      )))

(defun process-msg (msg sock);; &optional (from nil))
 (declare (special *fuzzball-pov-only-mode*
                   *fuzzball-task*))
  ;;(dbug :msg "Processing msg ~s from socket: ~A to ~A" msg (local-port sock) (remote-port sock))
  ;;(dbug :msg "Processing msg ~s from ~A" msg (getassoc sock (peer-sockets *self*)))
  (dbug :msg "Processing msg ~s from ~A" msg (getassoc :from msg))
  (case (getassoc :type msg)
    (:report (process-report-msg msg))
    (:announcement (process-announcement-msg msg))
    (:bid (process-bid-msg msg))
    (:award (process-award-msg msg))
    (:add-phase (process-add-phase-msg msg))
    (:modify-current-phase (process-modify-current-phase-msg msg))
    (:modify-skills (process-modify-skills-msg msg))
    ;;(:run-new-schedule (process-rts-run-new-schedule-msg msg))
    (:init (process-init-msg msg))
    ;;(:rts-init (process-rts-init-msg msg))
    ;;(:aid-init)
    (:reboot (process-reboot-msg msg))
    (:dying (process-dying-msg msg))
    (:halt
     (suspend-delib-task *delib-task*)
     (when (optimus-prime-p *self*)
        (dbug :top "Optimus prime received halt message; broadcasting to others")
        (broadcast-message :type :halt))
     (dbug :top "Halting because of received halt message")
     (setf *halt* 'halt-message))
    ;;(:rts-handoff (process-rts-handoff-msg msg))
    ;;(:rts-quitting (process-rts-quitting-msg msg))
    ;;(:aid-quitting (process-aid-quitting-msg msg))
    (:eof (process-eof-msg msg sock))
    ;;(:state (process-state-msg msg))
    (:new-master (process-new-master-msg msg))
    ;;(:notify (process-notify-msg msg))
    (:sync
     (setf *amp-paused* nil)
     (dbug :top "Got :sync startup message, beginning tasks..."))

    ;; Fuzzbomb messages.
    ;;(:kickstart (process-kickstart-msg msg))
    (:run-shell-cmd (process-run-shell-cmd-msg msg))
    (:rsync-file (process-rsync-file-msg msg))
    (:shell-cmd-results (process-shell-cmd-results-msg msg))
    (:new-target (process-new-target-msg msg))
    ;;(:new-madeira-target (process-new-madeira-target-msg msg))
    ;;(:madeira-success (process-madeira-success-msg msg))
    (:pcap-target (process-pcap-target-msg msg))
    (:capture-pcap (process-capture-pcap-msg msg))
    (:new-pereti-path (process-new-pereti-path-msg msg))
    (:new-pereti-step (process-new-pereti-step-msg msg))
    ;;(:new-test-case (process-new-test-case-msg msg))
    ;;(:new-test-case-dir (process-new-test-case-dir-msg msg))
    (:workflow-complete (process-workflow-complete-msg msg))
    ;;(:run-latent-tasks (process-run-latent-tasks msg))
    (:new-pov (process-new-pov-msg msg))
    ;;(:master-fuzzbomb (process-master-fuzzbomb-msg msg))
    ;;(:init-slave-svc (process-init-slave-svc-msg msg))
    (:fb-slave-assignment (process-fb-slave-assignment-msg msg))
    (:suspend-delib-task
        (setf *suspended-by-master* T)
        (suspend-delib-task *delib-task*))
    (:resume-delib-task
        (setf *suspended-by-master* nil)
        (when *delib-task* (execute-task *delib-task*)))
    ;;(:best-revision (process-best-revision-msg msg))
    ;;(:resend-best-revision (process-resend-best-revision-msg msg))
    ;;(:upload-best (process-upload-best-msg msg))
    ;;(:protocol (process-protocol-msg msg))
    ;;(:enable-defensive-rewrites (enable-defensive-rewrites))
    (:dbug (process-dbug-msg msg))
    (:eval (process-eval-msg msg))
    (:generic (process-eval-msg msg))   ;; for legacy compat
    (:fuzzball-pov-only-mode
        (dbug :top "Told to go to fuzzball-pov-only-mode")
        (setf *fuzzball-pov-only-mode* T)
        (when *fuzzball-task* (restart-fuzzball)))
    (:gui-update (process-gui-update-msg msg))
    (:gui-command (process-gui-command-msg msg))
    (:idle (process-idle-msg msg))

    ;; Lacrosse msgs
    (:new-challenge-project
     (process-new-challenge-project-msg msg))
    (:challenge-project-update
     (process-challenge-project-update msg))
    (:vuln-cand
     (process-vuln-cand-msg msg))
    (:bic-found
     (dbug :top "Received BIC-FOUND from ~a with bic = ~a" (getassoc :from msg) (getassoc :bic msg))
     (process-bic-found-msg msg))
    (:failed-to-bisect
     (dbug :top "Received FAILED-TO-BISECT from ~a" (getassoc :from msg))
     )
    (:patch-generation-succeeded
     (dbug :top "Received PATCH-GENERATION-SUCCEEDED from ~a" (getassoc :from msg))
     (process-patch-generation-succeeded-msg msg))
    (:patch-generation-failed
     (dbug :top "Received PATCH-GENERATION-FAILED from ~a" (getassoc :from msg))
     (process-patch-generation-failed-msg msg))
    (:gp-status
     (dbug :top "Received status on generated patch from ~a" (getassoc :from msg))
     (process-gp-status-msg msg))
    (:vd-status
     (dbug :top "Received status on discovered vulnerability from ~a" (getassoc :from msg))
     (process-vd-status-msg msg))
    (T (format t "WARNING: Unknown message type [~A]" msg))))

;;; -------------------------------------------------------------------------
;;; Fuzzbomb messages.

;;;-------------------------------------------------------------------------
(defvar *last-batch-end-time* nil)

(defvar *cached-run-shell-cmd-msgs* nil)

(defvar *running-test-runners* 0 "How many run-tests-with-message processes are ongoing")

(defvar *max-concurrent-test-runners* 100)

(defun process-cached-run-shell-cmd-msgs ()
  (when *cached-run-shell-cmd-msgs* (process-run-shell-cmd-msg (pop *cached-run-shell-cmd-msgs*)))
)

(defun process-run-shell-cmd-msg (msg)
  "Run a shell command, optionally rsync'ing files before."
  (dbug :amp-deep "Got run-shell-cmd-msg ~A" msg)

  (let* ((cmd (getassoc :cmd msg))
        (task (getassoc :task msg))
        (from (getassoc :from msg))
        ;;(sent-time (getassoc :sent-time msg))
        ;;(start-time (when (eq task :run-test-cases) (get-internal-real-time)))
        (start-time (get-internal-real-time))
        (tool-start-time nil)
        (tool-finish-time nil)
        (test-batch-id (getassoc :test-batch-id msg))
        (num-test-cases (getassoc :num-test-cases msg))
        (host (getassoc :rsync-host msg))
        (file (getassoc :rsync-file msg)))
    (when (eq task :run-test-cases)
      ;; DIRTY HACK: sub BRIDGE_PORT with amp's bridge port since fbombs don't have it.
      (setf cmd (cl-ppcre:regex-replace "BRIDGE_PORT" cmd (write-to-string (bridge-port *self*))))
      (when (>= *running-test-runners* *max-concurrent-test-runners*)
        (setappend *cached-run-shell-cmd-msgs* (list msg))
        (dbug :top "Caching run-shell-cmd-msg for future handling, there are now ~A cached" (length *cached-run-shell-cmd-msgs*))
        (return-from process-run-shell-cmd-msg nil)
        )

      (when (not *last-batch-end-time*) (setf *last-batch-end-time* start-time))
      (dbug :top "Received test-batch-id ~A with ~A test-cases at ~A; time since last batch finished ~A" test-batch-id num-test-cases start-time (- start-time *last-batch-end-time*)))
    (when (and host file)
      (rsync-file host file)
      (setf tool-start-time (get-internal-real-time))
      (dbug :top "Rsync completed after ~A" (- tool-start-time start-time)))
    (when (not tool-start-time) (setf tool-start-time (get-internal-real-time)))
    (cond ((eq task :run-test-cases) ;; this runs in background and gets results from msg.
           (run-shell-command cmd)
           (incf *running-test-runners*)
           (dbug :amp "Yowza, running ~A concurrent tests, ~A cached" *running-test-runners* (length *cached-run-shell-cmd-msgs*)))
          (t
    (multiple-value-bind (stdout stderr ec)
        (toolchain-command cmd)
      (setf tool-finish-time (get-internal-real-time))
      (dbug :top "toolchain cmd used ~A, completed after ~A" (- tool-finish-time tool-start-time) (- tool-finish-time start-time))
      (when (/= ec 0)
        (warn "Shell command ~A returned non-zero exit code ~A.~%stdout:~%~AS~%stderr~%~A" cmd ec stdout stderr))
      (case task
;        (:unstrip-libcgc
;               ;; FIXME what if ec <> 0?? just leave the other AMP hanging forever?  sheesh, send it some news!!
;         (when (= ec 0)
;           (progn
;             (setf *libcgc-func-table* stdout)
;             (broadcast-message :type :rsync-file
;                                :host (host *self*)
;                                :path (libcgc-json-file)))))
        ; (:run-test-cases
        ;  ;; FIXME WIP
        ;       ;; FIXME what if ec <> 0?? just leave the other AMP hanging forever?  sheesh, send it some news!!
        ;  (when (= ec 0)
        ;    (let ((json (decode-json-from-source stdout))
        ;          ; (json-file "")
        ;          )
        ;      (when (not json)
        ;        (dbug :amp "ERROR ~A produced invalid json from stdout of:~%~A" cmd stdout))
        ;      (dbug :amp "Sending run-test-case-results~%~A~%to ~A" json from)
        ;      (setf *last-batch-end-time* (get-internal-real-time))
        ;      (send-message :to from
        ;                          :type :run-test-cases-results
        ;                        :sent-time sent-time
        ;                        :test-batch-id test-batch-id
        ;                        :dvm-time (- *last-batch-end-time* start-time)
        ;                          :num-test-cases num-test-cases
        ;                          :json json))))
        (otherwise (send-message :to from
                                 :type :shell-cmd-results
                                 :task task
                                 :stdout stdout
                                 :stderr stderr
                                 :exit-code ec))))))))

;;;-------------------------------------------------------------------------
(defun process-shell-cmd-results-msg (msg)
  (dbug :amp-deep "Got shell-cmd-results ec: ~A~%stdout:~%~A~%stderr:~%~A"
        (or (getassoc :exit-code msg) 1)
        (getassoc :stdout msg)
        (getassoc :stderr msg)))

;;(defun process-run-tests-done-msg (msg)
;;  (let ((from (getassoc :from msg))
;;        (json-string (getassoc :json msg))
;;        (sent-time (getassoc :sent-time msg))
;;        (test-batch-id (getassoc :test-batch-id msg))
;;        (num-test-cases (getassoc :num-test-cases msg)))
;;    (dbug :amp "Test running done for batch ~A from ~A" test-batch-id from)
;;    (decf *running-test-runners*)
;;    (dbug :amp "Down to ~A tests running" *running-test-runners*)
;;    (let* ((json (decode-json-from-source json-string))
;;           (msg (list (list :to from)
;;                      (list :type :run-test-cases-results)
;;                      (list :sent-time sent-time)
;;                      (list :test-batch-id test-batch-id)
;;                      (list :dvm-time 0) ;; restore this sometime... (- *last-batch-end-time* start-time)
;;                      (list :num-test-cases num-test-cases)
;;                      (list :json json))))
;;      (when (not json)
;;        (dbug :amp "ERROR invalid json:~%~A" json-string))
;;      (dbug :amp "Sending batch ~A run-test-case-results~%~A~%to ~A" test-batch-id json from)
;;      (setf *last-batch-end-time* (get-internal-real-time))
;;      (if (equal from (shortname *self*))
;;          (process-run-test-cases-results-msg msg)
;;        (send-msg from msg))
;;      )
;;    (process-cached-run-shell-cmd-msgs))
;;  )

;;;-------------------------------------------------------------------------
(defmethod notify-of-svc-rev-results ((subscriber t) results)
  (declare (ignore results))
  nil)

(defun process-run-test-cases-results-msg (msg)
  (dbug :amp-deep "Got run-test-cases-results msg: ~%~A" msg)
;;  (let* ((json (getassoc :json msg))
;;         (results (json-to-lisp json)))
;;    (cond (json (handle-test-results results msg))
;;        (T (dbug :top "Warning: nil json results for test batch ~A" (getassoc :test-batch-id msg))
;;              ;; FIXME WIP redo / reassign
;;        )))
)

(defun get-root-test-case (test-case)
  (if (parent test-case)
      (get-root-test-case (parent test-case))
      test-case))

(defun get-descendant-test-cases (test-case)
  (remove-duplicates
   (append
    (children test-case)
    (apply #'append
           (mapcar #'get-descendant-test-cases (children test-case))))))

;;(defun dbug-core (result svc-rev)
;;  (when result
;;    (dbug :repair-deep " TC: ~S" (test-case result))
;;    (when (stats result)
;;      (let* ((fault-index (position (find-if #'faulted (stats result)) (stats result)))
;;           (fault-bin (nth fault-index (bins svc-rev))))
;;      (dbug :repair-deep " Regs: ~S" (mapcar #'registers (stats result)))
;;      (dbug :repair-deep " Core @ insns: ~A" (mapcar #'(lambda (a) (insn-by-addr a fault-bin))
;;                                                     (valid-core-ips-for-test-results (list result) fault-bin)))
;;      ))))

;;;-------------------------------------------------------------------------



;;;-------------------------------------------------------------------------
(defun process-rsync-file-msg (msg)
  "Get a file from the host and path in MSG."
  (let ((host (getassoc :host msg))
        (path (getassoc :path msg)))
    (ensure-directories-exist path)
    (rsync-file host path)
  ))

;;; -------------------------------------------------------------------------
(defun brute-amp-shortname ()
  ;; This is pretty hacky...
  (format nil "DVM~A" (position *self* *amps*)))

#|
(defun send-protocol-to-brute ()
  (declare (special *self* *have-protocol*))
  (dbug :top "Sending completed protocol to brute")
  (send-message :to (brute-amp-shortname) :type :protocol :host (host *self*)
        :path (protocol-dir (svc *self*)) :have-protocol *have-protocol*)
  )

(defun send-protocol-to-master ()
  (declare (special *self* *have-protocol*))
  (dbug :top "Sending completed protocol to master")
  (send-message :to (master *self*) :type :protocol :host (host *self*)
        :path (protocol-dir (svc *self*)) :have-protocol *have-protocol*)
)
|#

(defvar *client-tail-delim* nil)
(defvar *use-rare-delim* nil)
(defvar *use-auto-rare-delims* t)

;;;-------------------------------------------------------------------------
(defun process-libcgc-func-table-msg (msg)
  (dbug :top "Got libcgc-func-table msg ~A" msg)
  (setf *libcgc-func-table* (getassoc :value msg)))

;;;-------------------------------------------------------------------------
(defun ensure-svc-in-experiment-dir (svc-dir svc-id)
  "To avoid leaving cruft in e.g. /scratch/cgc-data/vulns."
  (cond
   ((string-startswith svc-dir *experiment-dir*) svc-dir)
   (t (let* ((dst-dir (strcat *experiment-dir* "/cbs/" svc-id "/"))
             (src-bin-dir  (strcat svc-dir "/bin"))
             (src-pcap-dir (strcat svc-dir "/pcap"))
             (dst-bin-dir  (strcat dst-dir "bin"))
             (dst-pcap-dir (strcat dst-dir "pcap")))
        (toolchain-command (format nil "mkdir -p ~A ~A" dst-bin-dir dst-pcap-dir))
        (toolchain-command (format nil "cp ~A/* ~A"  src-bin-dir dst-bin-dir))
        (toolchain-command (format nil "ln -s ~A/* ~A"  src-pcap-dir dst-pcap-dir))
        dst-dir))))

(defun process-pcap-target-msg (msg)
  (dbug :top "Got pcap-target msg ~A" msg)
  (when (not *event-start-time*) (setf *event-start-time* (get-internal-real-time)))
  (cond
   ((optimus-p *self*)
    (dbug :amp "Optimus ~A handling new PCAP_TARGET" *self*)
    (dolist (target (getassoc :target msg))
      (let* ((path (getassoc :path target))
             (target (make-instance 'target :path (make-pathname :name path))))
        (push target (targets *self*))
        (handle-pcap-target target)))
    (dbug :top "Done with new pcap target."))
   (T (dbug :amp "Not optimus; ignoring new PCAP-TARGET.")))
  )

(defun process-capture-pcap-msg (msg)
  (dbug :top "Got pcap-capture-msg ~A" msg)
  (when (not *event-start-time*) (setf *event-start-time* (get-internal-real-time)))
  (cond
    ((optimus-p *self*)
     (dbug :amp "Optimus ~A handling new capture pcap msg" *self*)
     (let ((interface (getassoc :interface msg))
           (packet-count (getassoc :packet-count msg))
           (src-host (getassoc :src-host msg))
           (init-cmd (getassoc :init-cmd msg))
           (ready-cmd (getassoc :ready-cmd msg))
           (run-cmd (getassoc :run-cmd msg))
           )
       (handle-capture-pcap interface packet-count src-host init-cmd ready-cmd run-cmd))
     (dbug :top "Done with new capture pcap msg."))
    (T (dbug :amp "Not optimus; ignoring new capture pcap msg."))))

(defun handle-capture-pcap (interface packet-count src-host init-cmd ready-cmd run-cmd)
  (dbug :top "Handling capture pcap on interface ~s.  packet-count: ~s, src-host: ~s" interface packet-count src-host)
  (let ((task (make-instance 'capture-pcap-task
                             :interface interface
                             :task-pcap-file (format nil "~a/~a.pcap" *experiment-dir* (uiop:getenv "CONTAINER_PREFIX"))
                             :packet-count packet-count
                             :src-host src-host
                             :init-cmd init-cmd
                             :ready-cmd ready-cmd
                             :run-cmd run-cmd)))
    (push task (tasks *self*))
    task))

(defun process-gui-update-msg (msg)
  (cond (*use-gui*
         (dbug :gui "Sending msg to gui: ~S" msg)
         (send-msg "OPTIMUS0-GUI" msg))
        (t
         (dbug-once :amp-no-gui "No gui to send msg to: ~S" msg))))

(defun process-gui-command-msg (msg)
  (dbug :gui "Received gui command: ~S" msg)
  )

;;; This is only used for targets to be handled locally, not distributed...
;;; For distribution, use new-neo-target
;; (defun process-new-target-msg (msg)
;;   (dbug :top "Got new-target msg ~S" msg)
;;   (when (not *event-start-time*) (setf *event-start-time* (get-internal-real-time)))
;;   (dolist (target-spec (getassoc :target msg))
;;     (let ((target (new-target-from-spec target-spec)))
;;       (process-target target :msg msg)))
;;   (dbug :top "Done with target."))

(defun process-new-target-msg (msg)
  (dbug :top "Got new-target msg ~S" msg)
  (when (not *event-start-time*) (setf *event-start-time* (get-internal-real-time)))
  (dolist (target-spec (getassoc :target msg))
    (let* ((existing-target-id (getassoc :id target-spec))
           (existing-target (and existing-target-id
                                 (find-target-by-id existing-target-id)))
           (next-task (getassoc :next-task msg)))
      (dbug :top "existing-target-id: ~s" existing-target-id)
      (dbug :top "existing-target: ~s" existing-target)
      (dbug :top "next-task: ~s" next-task)
      (let ((target (cond (existing-target)
                          (t (new-target-from-spec target-spec)))))
        (setf (next-task target) next-task)
        (process-target target :msg msg))))
  (dbug :top "Done with target."))

(defun process-target (target &key msg)
  (dbug :target "new target: ~s" target)
  (when target
    (dbug :timeline "Got target ~a." (brief-string target))
    (let ((target-tree-node (make-instance 'hist-tree-target-node
                                           :target target
                                           :target-msg msg)))
      (when (not (target-trees *self*))
        (dbug :top "No history tree root set. Setting history tree root.")
        (push target-tree-node (target-trees *self*)))
      ;; If the new target came from a task, add the new target node
      ;; to the children of the source task
      (let ((source-name (source target))
            source)
        (when source-name (setf (source target) source-name))
        (when (fixnump source-name)
          (setf source (find-object source-name))
          (dbug :top "Adding new target as child of named object: ~A" source-name)
          (push target-tree-node (children source))
          (setf (parent target-tree-node) source)))
      (push target (targets *self*))

      ;; Loop through all task that can be triggered by the creation
      ;; of a new target and find targets which apply to this new
      ;; target. Create an instance and add to the task queue task's
      ;; which apply.
      (loop for task-class in (target-triggered-tasks)
            for task-name = (class-name task-class)
            do (dbug :top "checking Task ~A" task-name)
            when (dont-error (task-applies-to-target-p task-name target-tree-node))
;;;                  (error (c)
;;;                    (dbug :top "Caught a condition: ~A" c)
;;;                    nil))
              do (progn
                   (dbug :top "Task ~A applies to target" task-name)
                   (let ((new-task (make-instance task-name :target target :parent target-tree-node)))
                     ;; Make the new task a child of the target's tree node
                     (push new-task (children target-tree-node))
                     (push new-task (tasks *self*)))
                   (dbug :top "Created task ~A" task-name)))
      ))
  (dbug :top "Done with target."))

;;; -------------------------------------------------------------------------

;; (defun process-new-madeira-target-msg (msg)
;;   (dbug (:top :triage) "Got new-madeira-target msg ~S" msg)
;;   ;;(when (not *event-start-time*) (setf *event-start-time* (get-internal-real-time)))
;;   (cond ;; OPT decides where to fwd; FBs handle as a new-target-msg
;;    ((optimus-p *self*)
;;     (dbug :amp "Optimus ~A handling new-madeira-target msg" *self*)
;;     ;; then stuff ripped from assign-master-fuzzbomb
;;     (let ((fb (acquire-fuzzbomb))       ;; NOTE this could fail if we dont keep track of m-f-tasks locally
;;           )
;;       (cond (fb
;;              (dbug :amp "Assigning and sending it to ~a" fb)
;;              (send-msg fb msg))
;;             (T
;;              (dbug :amp "Couldnt find an unassigned FB, taking myself")
;;              (process-new-target-msg msg)))
;;       ))
;;    (T (dbug (:amp :triage) "I'm not optimus; handling new-madeira-target msg as a new-target-msg.")
;;       (process-new-target-msg msg)
;;       )
;;    )
;;   (dbug (:top :triage) "Done with new-madeira-target.")
;;   )

;;; Somewhat confusingly this is
;;; (1) the msg that starts the whole (happy?) enchilada and
;;; (2) the msg that Optimus sends to a Fuzzbomb to assign new task.
;;; Optimus should only *get* this msg once, to start the show.
;;; Fuzzbombs will handle this msg repeatedly, see process-new-target-msg.
(defun process-new-challenge-project-msg (msg)
  (dbug (:top :triage) "Got new-challenge-project msg ~S" msg)
  (when (not *event-start-time*) (setf *event-start-time* (get-internal-real-time)))
  (cond ;; OPT decides where to fwd; FBs handle as a new-target-msg
   ((optimus-p *self*)
    (dbug :amp "Optimus ~A handling new-challenge-project msg" *self*)

    ;; one-time initialization for crs
    (dbug :top "Calling ensure-directories-exist: ~s"  *experiment-dir*)
    (ensure-directories-exist *experiment-dir*)

    (let ((cp-path (namestring (first (uiop:subdirectories *cp-root*)))))
      (dbug :amp "cp-path: ~s" cp-path)
      (let ((target (new-target-from-spec cp-path)))
        (dbug :amp "Our target: ~s" target)
        (push target (targets *self*))
        (dbug :amp "project-props: ~s" (project-properties target))
        (dbug :amp "(cp-prop target :cp_sources): ~s" (cp-prop target :cp_sources))
        (dbug :amp "(target-source-roots target): ~s" (target-source-roots target))
        (dbug :amp "(list-length (target-sources)): ~s" (list-length (target-sources target)))
        (dbug :amp "(target-sources): ~s" (target-sources target))
        (cond ((< (list-length (target-sources target)) 10)
               (dbug :amp "(target-sources): ~s" (target-sources target)))
              (t (dbug :amp "(first (target-sources): ~s" (first (target-sources target)))
                 (dbug :amp "len target-sources: ~s" (list-length (target-sources target)))))
        ;;(dbug :amp "(target-all-files-in-source target): ~s" (target-all-files-in-source target))
        ;;(dbug :amp "(target-c-files: ~s" (target-c-files target))
        ;;(dbug :amp "(target-java-files: ~s" (target-java-files target))

	(iter (for harness in (cp-prop target :harnesses))
	  (let ((seeds-dir (uiop:parse-unix-namestring
			    (format nil "~a/fuzz/seeds/~a"
				    (shared-path target)
				    (string (harness-prop-id harness)))
			    :type :directory)))
	    (ensure-directories-exist seeds-dir)
	    (uiop:run-program (format nil "cp -rf /lacrosse/code/corpus/* ~a" seeds-dir))))
				    
        (let ((next-task-list (initial-fuzz-next-tasks target)))
          (dbug :amp "Kicking off lax w: ~s" next-task-list)
          (assign-next-tasks target next-task-list)))))

   (T (dbug (:amp :triage) "I'm not optimus; handling new-challenge-project msg as a new-target-msg.")
      (process-new-target-msg msg)
      ))
  (dbug (:top :triage) "Done with new-challenge-project."))

(defun initial-fuzz-next-tasks (target)
  "Return some initial fuzzing tasks depending on number of avail fuzzbombs."
  (let* ((num-fbs (list-length *fuzzbombs*))
         (num-fuzz-tasks-to-choose (cond ((<= 1 num-fbs 2) 1)
                                         ((<= 3 num-fbs 4) 2)
                                         (t                (- num-fbs 3)))))
    (iter
      (for i below num-fuzz-tasks-to-choose)
      (collect (poss-initial-fuzz-next-task target i)))))
    
;; [nringo:20240714.1428CDT] This is called in afl-task.lisp, in the method
;; (post-exec afl-task), in an error-handling path -- if you tweak it here, be
;; sure that you're not breaking it there.
(defun poss-initial-fuzz-next-task (target i)
  "Returns the :next-task form for the i-th build."

  ;; Distribute between harnesses.
  (let* ((original-i i)
         (harness-list (cp-prop target :harnesses))
         (harness (elt harness-list (mod i (length harness-list)))))
    (setf i (floor i (length harness-list)))
    (cond
      ;; [nringo:20240710.1232CDT] TODO Linux???
      ((target-language-c-p target)
       ;; Choose a fuzzer. When both are enabled, we split half-and-half.
       (let ((fuzzer-kinds nil))
         (when *use-afl*       (push 'afl-task       fuzzer-kinds))
         (when *use-libfuzzer* (push 'libfuzzer-task fuzzer-kinds))
         (let ((fuzzer-kind
                 (elt fuzzer-kinds (mod i (length fuzzer-kinds)))))
           (setf i (floor i (length fuzzer-kinds)))
           ;; Make the :next-task form. Fuzzer arg diversity lives here.
           (ecase fuzzer-kind
             (libfuzzer-task
               ;; Choose an LLM.
               (let ((llm-arg (elt *lacrosse-gen-seeds-llms*
                                   (mod i (length *lacrosse-gen-seeds-llms*)))))
                 (setf i (floor i (length *lacrosse-gen-seeds-llms*)))
                 ;; Choose whether to enable -use_value_profile=1. This is
                 ;; pretty profitable, so we're enabling it 3/4 of the time.
                 (let* ((use-value-profiles '(nil t t t))
                        (use-value-profile (elt use-value-profiles
                                                (mod i (length use-value-profiles)))))
                   (setf i (floor i (length use-value-profiles)))
                   `(:next-task
                      ((:task-type libfuzzer-task)
                       (:chosen-harness-id ,(harness-prop-id harness))
                       (:examples ,*libfuzzer-examples-default*)
                       (:llm-arg ,llm-arg)
                       (:use-value-profile ,use-value-profile))))))
             (afl-task
               ;; Set the distributed fuzz ID and choose a power schedule. Note
               ;; that we _cannot_ allow a choice other than choice-of-harness above this!!
               (let* ((distributed-fuzz-id (if (zerop i) :default i))
                      (power-schedules '(:fast :explore :fast :exploit :fast :rare))
                      (power-schedule (elt power-schedules
                                           (mod i (length power-schedules)))))
                 (setf i (floor i (length fuzzer-kinds)))
                 ;; Choose an LLM.
                 (let ((llm-arg (elt *lacrosse-gen-seeds-llms*
                                     (mod i (length *lacrosse-gen-seeds-llms*)))))
                   (setf i (floor i (length *lacrosse-gen-seeds-llms*)))
                   `(:next-task
                      ((:task-type afl-task)
                       (:poss-initial-fuzz-next-task-i ,original-i)
                       (:chosen-harness-id ,(harness-prop-id harness))
                       (:distributed-fuzz-id ,distributed-fuzz-id)
                       (:examples ,*libfuzzer-examples-default*)
                       (:llm-arg ,llm-arg)
                       (:power-schedule ,power-schedule))))))))))
      ((target-language-java-p target)
       ;; Use -use_value_profile=1 a whopping 90% of the time.
       (let* ((use-value-profiles '(nil t t t t t t t t t))
              (use-value-profile (elt use-value-profiles
                                      (mod i (length use-value-profiles))))
              (sanitizer-classes  '("com.code_intelligence.jazzer.sanitizers.ClojureLangHooks"
                                    "com.code_intelligence.jazzer.sanitizers.Deserialization"
                                    "com.code_intelligence.jazzer.sanitizers.ExpressionLanguageInjection"
                                    "com.code_intelligence.jazzer.sanitizers.FileReadWrite"
                                    "com.code_intelligence.jazzer.sanitizers.FileSystemTraversal"
                                    "com.code_intelligence.jazzer.sanitizers.IntegerOverflow"
                                    "com.code_intelligence.jazzer.sanitizers.LdapInjection"
                                    "com.code_intelligence.jazzer.sanitizers.NamingContextLookup"
                                    "com.code_intelligence.jazzer.sanitizers.OsCommandInjection"
                                    "com.code_intelligence.jazzer.sanitizers.ReflectiveCall"
                                    "com.code_intelligence.jazzer.sanitizers.RegexInjection"
                                    "com.code_intelligence.jazzer.sanitizers.RegexRoadblocks"
                                    "com.code_intelligence.jazzer.sanitizers.ScriptEngineInjection"
                                    "com.code_intelligence.jazzer.sanitizers.ServerSideRequestForgery"
                                    "com.code_intelligence.jazzer.sanitizers.SqlInjection"
                                    "com.code_intelligence.jazzer.sanitizers.XPathInjection"
                                    ))
              (sanitizer-classname (elt sanitizer-classes (mod i (length sanitizer-classes))))
              )
         (setf i (floor i (length use-value-profiles)))
         ;; Choose an LLM.
         (let ((llm-arg (elt *lacrosse-gen-seeds-llms*
                             (mod i (length *lacrosse-gen-seeds-llms*)))))
           (setf i (floor i (length *lacrosse-gen-seeds-llms*)))
           `(:next-task
              ((:task-type jazzer-task)
               (:chosen-harness-id ,(harness-prop-id harness))
               (:examples ,*libfuzzer-examples-default*)
               (:llm-arg ,llm-arg)
               (:use-value-profile ,use-value-profile)
               (:sanitizer-classname ,sanitizer-classname)
               ))))))))

(defun assign-next-tasks (target next-task-list)
  (iter (for next-task in next-task-list)
    (assign-challenge-project target :next-task next-task)))

(defun process-vuln-cand-msg (msg)
  (dbug (:top :triage) "Got vuln-cand-msg ~S" msg)
  ;; FIXME check!  does this vc already exist?  lookup by id should do it
  (let* ((target (find-target-by-id (getassoc :target-id msg)))
         (id (getassoc :id msg))
         (existing-vc (when id
                        (find-vc-by-id id target)))
         (vc (or existing-vc
                 (make-instance 'vuln-cand
                                :target target
                                :blob (getassoc :blob msg)
                                :sanitizer-id (getassoc :sanitizer-id msg)
                                :harness-id (getassoc :harness-id msg)
                                ))))
    (dbug :top "target: ~s" target)
    (dbug :top "(vuln-cands target): ~s" (vuln-cands target))
    (dbug :top "existing-vc: ~s" existing-vc)
    (dbug :amp "vuln-cand: ~s" vc)
    (unless existing-vc
      (push vc (vuln-cands target)))

    (cond
      ((not (blob vc))
       (dbug :top "No blob found."))
      ;; unverified
      ((not (sanitizer-id vc))
       (assign-challenge-project (asc-target) :next-task
                                 `(:next-task
                                    ((:task-type lacrosse-cp-run-pov-task)
                                     (:vc-id ,(id vc))
                                     (:harness-id ,(harness-id vc))
                                     (:blob ,(blob vc))
                                     ))))
      ;; verified, but no bic
      ((not (bic vc))
       (let ((next-tasks (git-bisect-next-tasks vc)))
         (assign-next-tasks (target vc) next-tasks)))
      (t
       (dbug :top "vuln-cand was fully filled out: ~s " vc)))))

(defun git-bisect-next-tasks (vc)
  "Return list of :next-task forms for git-bisect on vc."
  (let* ((target (target vc))
         (cp-srcs (cp-srcs target))
         (sorted-cp-srcs (sort cp-srcs #'> :key #'(lambda (cp-src)
                                                    (list-length (git-commits-for-cp-src target cp-src))))))
    (dbug :top "cp-srcs: ~s" cp-srcs)
    (dbug :top "sorted-cp-srcs: ~s" sorted-cp-srcs)
    (iter (for cp-src in sorted-cp-srcs)
      (collect `(:next-task
                 ((:task-type lacrosse-git-bisect-task)
                  (:vc-id ,(id vc))
                  (:harness-id ,(harness-id vc))
                  (:blob ,(blob vc))
                  (:cp-src ,cp-src)
                  ))))))

(defun process-bic-found-msg (msg)
  ;; just one target and just one vc!
  (let* ((vc (find-vc-by-id (getassoc :vc-id msg) (asc-target)))
         (bic (getassoc :bic msg))
         (cp-src (getassoc :cp-src msg)))
    (setf (bic vc) bic)
    (setf (cp-src vc) cp-src)
    (dbug :top "Set bic of ~s to ~s (and cp-src to ~s)." vc bic cp-src)
    (setf (files-in-commit vc) (split-string-into-words (getassoc :files-in-commit msg)))

    ;; finally, check whether any vuln-cands are ready to submit!
    (consider-submit-vc (asc-target))))

(defun process-patch-generation-succeeded-msg (msg)
  (let* ((target (asc-target))
         (vc (find-vc-by-id (getassoc :vc-id msg) target))
         (json-output-file (getassoc :json-output-file msg))
         (pc (make-instance 'patch-cand
                           :target target
                           :vuln-cand vc
                           :patch-file (getassoc :patch-file msg)))
         (new-patch-namestring (strcat (shared-data-dir pc) (uiop-file-namestring (patch-file pc))))
         )
    (dbug :top "Copying patch to ~s." new-patch-namestring)
    (cp (patch-file pc) new-patch-namestring)
    (setf (patch-file pc) new-patch-namestring)
    
    ;; llm patcher creates this, other patchers may not
    (when json-output-file
      (cp json-output-file (shared-data-dir pc)))
    (push pc (patch-cands vc))
    (dbug :top "Created patch-cand: ~s" pc)
    (consider-submit-patch target)
    ))

(defun process-patch-generation-failed-msg (msg)
  (dbug :top "process-patch-generation-failed-msg: ~s" msg)
  ;; Lookup patch-cand and mark it as failed.
  ;; refine patch task?
  )
  
(defun assign-challenge-project (target &key next-task)
  (dbug :top "assign-challenge-project: ~s" target)
  (when (not *event-start-time*) (setf *event-start-time* (get-internal-real-time)))
  (assert (optimus-p *self*))
  (let ((fb (acquire-fuzzbomb)) ;; Now acquire-fuzzbomb always returns one
        )
    (cond (fb
           (send-target-w-next-task target next-task fb))
          (t
            (dbug :amp "ERROR Couldnt find an FB to assign, ALL IS PROBABLY LOST")
            (dbug :amp "Trying to send to myself, in desperation")
            (send-target-w-next-task target next-task (shortname *self*)))
                )))

(defun send-target-w-next-task (target next-task fb)
  (let ((msg `((:type :new-challenge-project)
               (:target ((,@(spec target))))
               ,@(when next-task (list next-task)))))
    (dbug :amp "Assigning and sending it to ~a" fb)
    (push (list target next-task) (lacrosse-tasks (find-amp fb)))
    (send-msg fb msg)))

;;; Q need a way to resume if want to try more?
(defun try-patching-through-hunk-reversion (vc)
  (dbug :top "try-patching-through-hunk-reversion: ~s" vc)

  (prepare-hunk-dir vc)

  ;;(let ((bic-namestring (commit-namestring (target vc) (bic vc))))
  (let ((hunk-sets
          (append (reasonably-sized-set-of-hunk-combinations vc)
                  (list (list (commit-namestring (target vc) (bic vc)))))))
    (dbug :top "hunk-sets: ~s" hunk-sets)
    (iter (for hunk-set in hunk-sets)
      (collect
          `(:next-task
            ((:task-type revert-to-patch-task)
             (:to-revert ,hunk-set)
             (:vc-id ,(id vc))
             (:harness-id ,(harness-id vc))
             (:blob ,(blob vc))
             (:bic ,(bic vc))
             (:cp-src ,(cp-src vc))))
        ))))

(defun reasonably-sized-set-of-hunk-combinations (vc)
  (let ((hunks (hunks (target vc) (bic vc))))
    (cond ((< (size-of-powerset hunks) *revert-to-patch-limit-per-bic*)
	   (sort (remove nil (powerset hunks)) #'< :key #'list-length))
	  ((< (list-length hunks) *revert-to-patch-limit-per-bic*)
	   (mapcar #'list hunks))
	  (t (mapcar #'list (subseq hunks 0 *revert-to-patch-limit-per-bic*))))))

(defun prepare-hunk-dir (vc)
  (dbug :top "prepare-hunk-dir: ~s" vc)
  (let* ((target (asc-target))
         (bic (bic vc))
         (cp-src (target-source-w-commit target bic))
         (git-info-dir (git-info-dir-for-cp-src target cp-src))
         (hunk-dir (hunk-dir target bic)))
    (declare (ignorable git-info-dir))
    (cond ((uiop:directory-exists-p hunk-dir)
           (dbug :top "hunk dir ~s already exists for ~s." hunk-dir bic))
          (t
           ;;(dbug :top "cp-src: ~s" cp-src)
           ;;(dbug :top "git-info-dir: ~s" git-info-dir)
           ;;(dbug :top "hunk-dir: ~a" hunk-dir)
           ;;(dbug :top "(commit-namestring target bic): ~a" (namestring-realpath (commit-namestring target bic)))
           (ensure-directories-exist hunk-dir)
           (let ((split-cmd (format nil "cd ~a && splitpatch --hunks ~a" hunk-dir (namestring-realpath (commit-namestring target bic)))))
             (dbug :top "split-cmd: ~s" split-cmd)
             (uiop:run-program split-cmd :output t :force-shell t :ignore-error-status t :error-output :output))
           (let ((hunks (hunks target bic)))
             (dbug :top "hunks: ~s" hunks)
             hunks)
           ))))

;;; -------------------------------------------------------------------------

(defparameter *madeira-debloating-successes* 0)

(defun process-madeira-success-msg (msg)
  (dbug :top "Got madeira-success msg ~S" msg)
  (incf *madeira-debloating-successes*)
  (dbug :top "Now there are ~A MADEIRA successful debloating results." *madeira-debloating-successes*)
)


;;; -------------------------------------------------------------------------


;;; FIXME pass in the path to the cp, iow /cp_root/<cp_name>
;;; Can assume that /cp_root/<cp_name>/project.json exists.
(defmethod new-target-from-spec ((cp-path string))
  "Given a list defining a target (eg, from a new-target-msg),
   create a new target object and return it."
  (let (
        (dir cp-path)
        ;;(cp-address (getassoc :cp-address target-spec))
        )
    ;;(dbug :target "Creating new target for spec: ~s" target-spec)
    (dbug :target "dir: ~s" dir)
    ;;(dbug :target "cp-address: ~s" cp-address)
    ;;(dbug :target "(uiop:directory-exists-p path): ~s" (uiop:directory-exists-p path))
    ;; Lacrosse challenge project

    ;;(dbug :top "Creating lacrosse-cp-target for: ~s" cp-address)
    (make-instance 'lacrosse-cp-target :cp-path cp-path)))

(defmethod new-target-from-spec ((spec list))
  "Given a list defining a target (eg, from a new-target-msg),
   create a new target object and return it."
  (dbug :target "new-target-from-spec: ~s" spec)
  ;; avoid infinite method loop!
  (unless spec
    (error "nil is not a spec!"))
  (let* (
         (cp-path (getassoc :cp-path spec))
         (id (getassoc :id spec))
         (make-inst-args `(lacrosse-cp-target :cp-path ,cp-path ,@(when id (list :id id))))
        )
    (dbug :target "cp-path: ~s" cp-path)
    (dbug :target "make-inst-args: ~s" make-inst-args)
    ;;(new-target-from-spec cp-path)
    (apply #'make-instance make-inst-args)
    ))

(defun process-new-pereti-step-msg (msg)
  (dbug :top "Got new-pereti-next-step msg ~S" msg)
  (when (not *event-start-time*) (setf *event-start-time* (get-internal-real-time)))
  (cond
    ((optimus-p *self*)
     (dbug :amp "Optimus ~A handling pereti next step" *self*)
     (let* ((new-task (make-instance 'pereti-step-task
                                     :analysis-step (getassoc :pereti-analysis-step msg)
                                     :target-filename (getassoc :pereti-target msg)
                                     :hooks-file (getassoc :hooks-file msg))))
       (push new-task (tasks *self*)))
       (dbug :top "Done with pereti step."))
    (T (dbug :amp "Not optimus; ignoring new PERETI STEP.")))
  )

(defun process-new-pereti-path-msg (msg)
  (dbug :top "Got new-pereti-path msg ~S" msg)
  (when (not *event-start-time*) (setf *event-start-time* (get-internal-real-time)))
  (cond
    ((optimus-p *self*)
     (dbug :amp "Optimus ~A handling new PERETI PATH" *self*)
     (let* ((path (getassoc :path msg))
            (target (make-instance 'target :path path))
            (json-path (getassoc :json-path msg))
            )
        (setf *fuzzball-more-args* (format nil " -pereti-path ~A"
                                          (namestring json-path )) )
        (push target (targets *self*))
        (triage-target target)
        )
     (dbug :top "Done adding PERETI PATH."))
    (T (dbug :amp "Not optimus; ignoring new PERETI PATH.")))
  )

(defun process-workflow-complete-msg (msg)
  (dbug :top "Got workflow-complete msg ~S" msg)
  (cond
    ((optimus-p *self*)
     (incf *pereti-workflows-completed*)
     (dbug :amp "~A handling pereti workflow-complete number ~a" *self* *pereti-workflows-completed*)
;;     (when (eql *pereti-workflows-completed* *total-pereti-targets*)
;;      (dbug :top "Completed all pereti target workflows")
;;      (when *use-canned-povs*
;;        (dbug :top "Using canned povs, so telling FBs to run latent isabel tasks")
;;          (broadcast-message :type :run-latent-tasks :classname 'isabel-task)
;;      ))
     (dbug :top "Done with pereti workflow-complete."))
    (T (dbug :amp "Not optimus; ignoring workflow-complete."))) ;; this shouldnt happen
)

;;; -------------------------------------------------------------------------
;;(defun process-run-latent-tasks-msg (msg)
;;  (dbug :top "Got run-latent-tasks msg ~S" msg)
;;  (let* ((classname (getassoc :classname msg))
;;       (tasks (find-all-instances classname))
;;      )
;;     (dbug :top "Found tasks ~S" tasks)
;;     (my-mapcar #'pushfn tasks (tasks *self*))
;;))

;;; -------------------------------------------------------------------------
;;; push is a macro, who knew, so need this to make my-mapcar work above.
;;(defun pushfn (a b)
;;  (push a b))

;;; -------------------------------------------------------------------------
;;; We can use all of the PoVs we know about, either for this service or for all known prior govt examples.
;;; The way we do this is just to find them all and create "magic" new-test-case msgs for them
;;; that appear with a new provenance (:prior-pov) and get otherwise treated as usual.

;;(defvar *use-prior-service-povs* nil)
(defvar *use-all-prior-povs* nil)

;;(defvar *all-service-directories* (directory "/scratch/cgc-data/vulns/darpa-examples-with-pcaps/*"))
;;(defvar *all-service-directories* (directory "/scratch/cgc-data/vulns/darpa-examples/examples/*"))
(defvar *all-service-directories* (directory "UNUSED-FIXME-NUKE_STUFF_RELATINGTOTHIS"))

(defvar *all-prior-povs* nil)
(dolist (dir *all-service-directories*)
  (setappend *all-prior-povs* (directory (strcat (namestring dir) "/pov/*.xml"))))

#|
;;; note we have to copy them into our own dir, and can use the new test-case-dir msg
(defun send-prior-povs-as-test-cases ()
  (let* ((newdir (merge-pathnames "prior-povs/" (make-pathname :directory (dir (svc *self*)))))
         (newdirname (namestring newdir))
         (index 0)      ;; give unique names in case they have same name in orig form
        )
  (dbug :top "Sending prior povs as test cases")
  (ensure-directories-exist newdir)
  (dolist (pov *all-prior-povs*)
    (cp (namestring pov) (format nil "~A/~A-~A.xml" newdirname (incf index) (pathname-name pov))))
  (process-new-test-case-dir-msg (list (list :type :new-test-case-dir)
                          (list :dir newdirname)
                          (list :provenance :prior-pov)
                          (list :from (shortname *self*))
                        ))
))
|#

#+testing
(progn
  (setf *self* (make-amp "OPTIMUS0" :master-p T :role :optimus))
  (setf (svc *self*) (make-instance 'svc :id 'fake :dir (namestring (make-pathname :directory "/tmp/foo"))))
)
;;; -------------------------------------------------------------------------

(defvar *test-case-strings* (make-hash-table :test #'equal :size 1000))

;;; -------------------------------------------------------------------------
;;; from http://www.ymeme.com/slurping-a-file-common-lisp-83.html
(defun slurp-stream5 (stream)
  (let ((seq (make-array (file-length stream) :element-type 'character :fill-pointer t)))
    (setf (fill-pointer seq) (read-sequence seq stream))
    seq))
;;; -------------------------------------------------------------------------
        ;; check for duplication... this could get too expensive if the xml is really big... so maybe we should
        ;; FIXME check for filesize before doing this.  Or maybe saving duplication of large test cases is even more valuable??

(defvar *duplicate-test-case-count* 0)

(defun duplicate-test-case-file (filename)
  (declare (ignore filename))
  nil)
;
;  (let ((hash (hash-file filename))
;        past-fn)
;   (setf past-fn (gethash hash *test-case-strings*))
;   (cond (past-fn
;         (dbug :top "Found duplicate test case ~a: ~a is same as ~a" (incf *duplicate-test-case-count*) filename past-fn)
;         T)
;        (T (sethash hash *test-case-strings* filename)
;         (dbug :top "Not a duplicate test case")
;          (write-reload-sexp (list 'sethash hash '*test-case-strings* filename))
;         nil))))

;;; -------------------------------------------------------------------------
(defvar *cached-new-test-case-msgs* nil)

;;; -------------------------------------------------------------------------
(defvar *num-fuzzball-pollers* 0)

;;; -------------------------------------------------------------------------

(defun process-new-pov-msg (msg)
  (dbug :top "Got new-pov msg ~A" msg)
  (when (or (optimus-p *self*) (master-fuzzbomb-p *self*))
    (let* ((target-id (getassoc :target-id msg))
           (pov-path (getassoc :pov-path msg))
           (host (host (find-amp (getassoc :from msg))))
           (target (find-target-by-id target-id)))
      (dbug :top "Recording new POV ~A for ~A" pov-path target)
      (cond (target
             ;; Make sure we push this to the front, so
             ;; choose-pov-for-upload grabs the newest.
             (push pov-path (povs target))
             (dbug :top "target ~A now has ~A POVs" target (list-length (povs target)))
             (dbug :top "RESULT: total count of svc-metas that have POVs: ~A" (num-povs *self*)))
            ;; else print error, but continue (?)
            (t
             (dbug :top "Error: Could not find target ~A for POV ~A" target-id pov-path)))

      (dbug :amp-deep "targets: ~A" (targets *self*))

      ;; rsync the file.
      ;; FIXME now we are getting a whole afl dir of data not just a single xml file
      (dbug :top "WARNING: we aren't rsyncing remote pov data yet; host is ~A" host)
      ;;(rsync-file host pov-path)

      ;; Since submit- will re-choose best POV, this should not mess us up even if we like a diff pov better
      (when (optimus-prime-p *self*)
        ;; don't make this a RESULT: b/c it includes variable working time
        (dbug :top "Got new POV for ~A after working for ~A" target (- (get-internal-real-time) *event-start-time*))
        ;; (see things-to-save in persistence-fuzzbomb.lisp)
        ;;(dbug :target "[after POV] (target-trees *self*): ~s" (persistence:object-string (target-trees *self*)))
        (upload-pov-to-analystio pov-path target)
        )
)))

;;; -------------------------------------------------------------------------
(defun upload-pov-to-analystio (pov-path target)
  (when *upload-povs*
        (dbug :top "Uploading POV to analytics console")
;;      (let ((cmd (list "../tools/upload-first-afl-crash" pov-path
;;                                      (format nil "POV for ~A" target)))   ;; FIXME extend descrip, eg w/ tgt path string
        (let ((cmd (format nil "../tools/upload-first-afl-crash ~A \"POV for ~A\"" pov-path target))   ;; FIXME extend descrip, eg w/ tgt path string
              output)
          (dbug :top "cmd: ~s~%" cmd)
          (dont-error (setf output (uiop:run-program cmd :output t :ignore-error-status t)))
          (dbug :top "output: ~s~%" output))))

;;; -------------------------------------------------------------------------
(defun process-fb-slave-assignment-msg (msg)
  (dbug :top "Got fb-slave-assignment msg ~A" msg)
  (let ((mfb (getassoc :fuzzbomb msg))
        (sfb (getassoc :slave msg)))
    (cond
     ((optimus-p *self*)
      (dbug :amp "Optimus ~A tracking DVM assignment." *self*)
      ;; FIXME hot backup optimi should track this.
      )
     ((string-equal mfb (shortname *self*))
      (dbug :amp "I am taking control of FB ~A" sfb)
      (push sfb (slave-fuzzbombs *self*))))))

;;; -------------------------------------------------------------------------
(defun faults-bsf (test-case)
  (find (best-so-far (svc *self*))
        (faulting-revs test-case)))

;;; Since new PoVs are pushed onto the pov-test-cases list, this results in slightly non-stable
;;; best-pov selection (ie, new povs are preferred over old, all else being equal).  I don't think that's a big problem for CQE.
;;; FIXME For an event where uploads cost us something, it should be changed (possibly just by appending new povs?)

(defun better-pov (a b)
  "returns non-nil if pov a is better than b."
  (or (and (not (faults-bsf a)) (faults-bsf b)) ;; Don't fault our best rev; would kill consensus score.
      (> (length (faulting-revs a)) (length (faulting-revs b))) ;; Faults more.
      (and (= (length (faulting-revs a)) (length (faulting-revs b)))
                    (< (file-size (file a)) (file-size (file b)))))) ;; Shorter.

(defun get-best-pov ()
  (let ((best nil))
    (dolist (pov (pov-test-cases (svc *self*)))
      (when (or (null best)
                (better-pov pov best))
        (setf best pov)))
    (if best
        (dbug :top "Best pov ~A faults: ~A faults-bsf: ~A size: ~A" best (length (faulting-revs best)) (faults-bsf best) (file-size (file best)))
      (dbug :top "Best pov is nil, must not have found one yet."))
    best))


;;; -------------------------------------------------------------------------

(defun run-brute-p ()
  (declare (special *run-brute* *do-protocol-inference*))
  (and *run-brute*
       *do-protocol-inference*
       (i-am-brute-fuzzer)
       ;; Brute will run against default proto.regex if no pcap files to run against.
       ;; (pcap-files (svc *self*))
       ))

;;; -------------------------------------------------------------------------
(defun i-am-brute-fuzzer ()
  (dvm-p *self*)
  ;; (and (> *total-slaves* 0) (= (slave-index *self*) *total-slaves*))
  )

;;; -------------------------------------------------------------------------
(defun same-host-as-master ()
  (dbug :top "same-host-as-master: slave host ~A master host ~A" (host *self*) (master-host *self*))
  (equal (host *self*) (master-host *self*)))

;;; -------------------------------------------------------------------------
(defun process-eval-msg (msg)
  (dbug :top "Got eval msg ~A" msg)
  (let ((form (getassoc :form msg)))
      (dbug :amp "Handling new eval msg: ~A" form)
      (eval (read-from-string form))
))
;;; -------------------------------------------------------------------------
(defun process-dbug-msg (msg)
  (dbug :top "Got dbug msg ~A" msg)
  (let ((add (getassoc :add msg))
        (del (or (getassoc :del msg) (getassoc :remove msg) (getassoc :delete msg) (getassoc :rem msg))))
  (pushnew add *debug-list*)
  (when del (setf *debug-list* (delete del *debug-list*)))))

;;; -------------------------------------------------------------------------
;;; Fuzzbomb helpers.
(defun init-resources ()
  "Populate the Fuzzbomb and DVM lists from the experiment configuration."
  (when (optimus-p *self*)
    (dbug :amp "Initializing resource lists.")
    (setf (unassigned-fuzzbombs *self*) (sort (copy-list *fuzzbombs*) #'string<))
    (setf (unassigned-dvms *self*) (sort (copy-list *dvms*) #'string<))
    (setf (dvms *self*) (sort (copy-list *dvms*) #'string<))
    (dbug :amp "Fuzzbombs: ~A" (unassigned-fuzzbombs *self*))
    (dbug :amp "DVMs: ~A" (unassigned-dvms *self*))))

(defun unstrip-libcgc (dvm)
  "Run libcgc.a unstripping on DVM, stash the result in a global, and broadcast the results."
  (dbug :amp "Telling ~A to unstrip libcgc." dvm)
  (send-message :to dvm
                :type :run-shell-cmd
                :task :unstrip-libcgc
                :cmd (strcat "cd tools && ./unlink-map --mode=cgc --file=/usr/lib/libcgc.a > "
                             (strcat *experiment-dir* "libcgc.json"))))

(defun precomputations ()
  "Precompute stuff we can."
  ;; libcgc.a unstripping.
  (let ((dvm (acquire-dvm)))
    (if dvm
        (unstrip-libcgc dvm)
      (warn "No DVM; skipping libcgc.a unstripping."))
    ;; Even though the shell command may not have finished yet, no
    ;; need to hog the dvm...worst case the next user waits a few
    ;; seconds for it to finish.
    (release-dvm dvm)))

(defun rsync-file (host file &optional (dest ""))
  "rsync HOST:FILE to FILE. Assumes the file path should be the same on remote host and local host. Should work for directories too--just remember to include the trailing slash!"
  (declare (special *self*))
  (when (string= host (host *self*))
        (dbug :amp "not rsyncing ~A from ~A, because on same host" file host)
        (return-from rsync-file))
  (dbug :amp "rsyncing ~A from ~A with dest [~A]" file host dest)
  (uiop:run-program (format nil "../tools/locked-rsync.sh ~A ~A \"\" ~A" host file dest))
)

;; this ver allows diff dest filename, reqd b/c in-docker has /neo-fuzz and outside is in NFH
(defun rsync-file-to-host (file host dest)
  "rsync FILE to HOST:dest. Should work for directories too--just remember to include the trailing slash!"
  (declare (special *self*))
  (dbug :amp "host is ~A host *self* is ~A" host (host *self*))

  (when (string= host (host *self*))
        (dbug :amp "not rsyncing ~A from ~A, because on same host" file host)
        (return-from rsync-file-to-host))
  (dbug :amp "rsyncing ~A to ~A:~A" file host dest)
  (uiop:run-program (format nil "../tools/locked-rsync.sh \"\" ~A ~A ~A" file host dest) :output t)
)

;;; NOTE this is not called during idle msg triggered re-assignment.
(defun acquire-fuzzbomb (&key (host nil))
  "Remove a Fuzzbomb from the unassigned pool, and return it. Return NIL if none available."
  (dbug :amp-deep "acquire-fuzzbomb :host ~s" host)
  (let ((fuzzbomb
          (if host
              (let ((found-host
                      (find-if #'(lambda (fb) (string= host (host (find-amp fb))))
                               (unassigned-fuzzbombs *self*))))
                (remove found-host (unassigned-fuzzbombs *self*))
                found-host)

              (cond ((unassigned-fuzzbombs *self*)
                     (pop (unassigned-fuzzbombs *self*)))
                    (t
                     (dbug :amp-deep "(master-fuzzbomb-tasks *self*): ~s~%" (master-fuzzbomb-tasks *self*))
                     ;; assigned to this fuzzbomb.  Should first check to see if there are any current tasks.
                     (let ((fb-task-elt (musliner:rank-and-choose #'(lambda (elt)
                                                                      (list-length (second elt)))
                                                                  #'min
                                                                  (master-fuzzbomb-tasks *self*))))
                       (dbug :amp-deep "fb-task-elt: ~a" fb-task-elt)
                       (when (or (not *init-task-limit-per-fuzzbomb*)
                                 (> *init-task-limit-per-fuzzbomb* (list-length (second fb-task-elt))))
                         (car fb-task-elt))))))))
    (when (not fuzzbomb) (setf fuzzbomb (cl-variates:random-element *fuzzball-seed*  *fuzzbombs*)))
    (dbug :amp-deep "Acquired fuzzbomb ~A" fuzzbomb)
    fuzzbomb))

(defun release-fuzzbomb (fuzzbomb)
  "Return FUZZBOMB to the unassigned pool."
  (dbug :amp "Releasing fuzzbomb ~A" fuzzbomb)
  (push fuzzbomb (unassigned-fuzzbombs *self*)))

;;;-------------------------------------------------------------------------
;;; semi-replicated from musliner-tools but using cl-variates and non-destructive
(defun randomize-list (l &aux choice (newl nil))
  (declare (special *fuzzball-seed*))
  (loop for i from 1 to (length l)
      do
        (setf choice (cl-variates::random-element *fuzzball-seed* l))
        (push choice newl)
        (setf l (remove choice l)))
 newl)

;;;-------------------------------------------------------------------------
(defun acquire-dvm ()
  "Remove a DVM from the unassigned pool, and return it. Return NIL if none available.
        we always have to give *some* DVM... so if none unassigned, choose one from the list....  "
  (declare (special *fuzzball-seed*))
  (dbug :top "unassigned dvms: ~A" (unassigned-dvms *self*))
  (dbug :top "dvms: ~A" (dvms *self*))
  (let ((dvm (or (pop (unassigned-dvms *self*))
                 (cl-variates:random-element *fuzzball-seed* (dvms *self*)))))
    (dbug :amp "Acquired dvm ~A" dvm)
    dvm))

(defun release-dvm (dvm)
  "Return DVM to the unassigned pool."
  (dbug :amp "Releasing dvm ~A" dvm)
  (push dvm (unassigned-dvms *self*)))

(defun pcap-files (svc)
  "Return a list of pcap file paths on SVC."
  (mapcar #'namestring
          (directory
           (merge-pathnames
            "*.pcap"
            (merge-pathnames "pcap/" (make-pathname :directory (dir svc)))))))

(defun pcap-file (svc)
  "Return the one pcap file path of SVC, print warning if there are more."
  (let ((files (pcap-files svc)))
    (when (> (length files) 1)
      (dbug :top "WARNING: ~A pcap files exist for ~A, but only using first." (length files) svc))
    (first files)))

;;; -------------------------------------------------------------------------
(defvar *fuzzball-pov-only-mode* nil)
;;; -------------------------------------------------------------------------

(defvar *pre-protocol-bad-pollers* nil) ;; we collect up timed-out and non-matched pollers before we have protocol analysis done, for later repair attempt.

;;; -------------------------------------------------------------------------
(defun restart-fuzzball ()
  (dbug :top "Restarting fuzzball")
  (kill-delib-process *fuzzball-task*)
  (setf (status *fuzzball-task*) :new)
  (execute-task *fuzzball-task*)
)

;; But as before, as long as it gets master, it'll be happy
(defun assign-master-fuzzbomb (target &key (fb nil) (host nil))
  "Acquire master and assign this task to it.  If fb is supplied, use that master (NO CHECKING)."
  (let ((master (or fb
                    (acquire-fuzzbomb :host host))))
    (cond (master
           (dbug :amp "~A is now master of ~A." master target)
           (setassoc master target (master-fuzzbombs *self*) :test #'string-equal)
           (dbug :amp-deep "(master-fuzzbombs *self*): ~s" (master-fuzzbombs *self*))
           (addlassoc master target (master-fuzzbomb-tasks *self*) :test #'string-equal)
           (dbug :amp-deep "(master-fuzzbomb-tasks *self*): ~s" (master-fuzzbomb-tasks *self*))
           (setf (status target) :assigned)

           ;; now we push the target there.
           ;; If all goes well, the source and dest paths/names should be the same.
           ;; Since this is now run only w/in docker, we must remap the /neo-fuzz name to rsync it to the
           ;; right place on the remote host.  But once it is there, we tell the master the /neo-fuzz name b/c he'll be in a
           ;; docker image too!
           ;; because some tasks, like pereti-unpack-task, make their own directories
           ;; or keep directories on one host.
           (when (dir target)
             (rsync-file-to-host (dir target) (host (find-amp master)) (remap-dir (dir target))))
           (dbug :timeline "Send target ~a to ~a." target master)
           (send-message :to master
                         :type :new-target
                         :id (id target)    ; target attr
                         :source :optimus              ; target attr
                         :target (list (list (list :path (path target))
                                             (list :source :optimus)
                                             (list :id (id target))
                                             (list :input-channel (persistence:object-string (input-channel target)))
                                             (serialize-attrs target)
                                             )))
           )
          (t  ; null master
           (dbug :amp "No master fuzzbomb available for ~s" target)
           (setf (status target) :blocked-waiting-for-fb)
           master))))

(defun remap-dir (dir)
  (let ((neo-fuzz-home (cond ((uiop:getenv "HOST_NEO_FUZZ_HOME"))
                             ((uiop:getenv "NEO_FUZZ_HOME"))
                             (t (asdf:system-relative-pathname :fuzzbomb "../../"))))
        (rest-dir (multiple-value-bind (absrel dirlist name fileonly)
                      (uiop:split-unix-namestring-directory-components dir)
                    (declare (ignore fileonly))
                    (setf dirlist (rest dirlist))       ;; drop the leading dir, /neo-fuzz
                    (push absrel dirlist)
                    (dbug :amp-deep "dirlist is ~s" dirlist)
                    (namestring (make-pathname :directory dirlist :name name)))))
    (strcat neo-fuzz-home rest-dir)))

;; FIXME this msg not handled on receive side yet.
(defun assign-fb-slave (fb fuzzbomb)
  (dbug :amp "Assigning slave FB ~A to the control of master FB ~A" fb fuzzbomb)
  (send-message-incl-optimi :to fuzzbomb :type :fb-slave-assignment :slave fb :fuzzbomb fuzzbomb))

;;; -------------------------------------------------------------------------

(defun any-dvm-is-not-busy ()
  ;;(remove-if #'pending-test-cases *dvms* :key #'find-amp)
  (dolist (dvmname *dvms*)
        (when (not (pending-test-cases (find-amp dvmname)))
           (return-from any-dvm-is-not-busy T)))
  nil
)

;;; -------------------------------------------------------------------------
(defun make-new-fuzzball-task ()
  "Make fuzzball task for a new svc.  Note for now only one svc per agent"
  (dbug :amp "make-new-fuzzball-task")
  (let ((task (make-instance 'fuzzball-task :svc-rev (original-rev (svc *self*)))))
    (push task (tasks *self*))
    task
    ))

;;; -------------------------------------------------------------------------
(defun original-binaries (svc-dir)
  "Return a list of paths to the original binaries of the svc in svc-dir. Assumes directory structure: bins are in SVC-DIR/bin/*"
  ;; FIXME remove the remove-if once we start working off of our real
  ;; directory structure instead of precanned darpa examples.
  (let ((bins (remove-if #'not-an-original-binary
                         (sort (mapcar #'namestring (directory (strcat (namestring svc-dir) "/bin/*")))
                               #'string-lessp))))
    (dbug :amp "original-binaries from ~A is: ~A" svc-dir bins)
    bins))

(defun not-an-original-binary (filename)
  (some #'(lambda (suffix)
            (string-endswith filename suffix))
        '("_patched"
          ".cfg"
          ".elf"
          ".json"
          ".svn"
          ".targets"
          ".addrs"
          ".log"
          ".makecfgout")))

(defvar *target-under-triage* nil)
(defun triage-target (target)
  "Examine an incoming target and create new testable objects."
  (setf *target-under-triage* target)
  (let* ((target-pathname (namestring (path target)))
         (target-filename (file-namestring (path target)))
         (target-exp-pathname (merge-pathnames (format nil "target-~d" (name target))
                                               (make-pathname :directory *experiment-dir*)))
         (target-exp-namestring (namestring target-exp-pathname))
         ;; create new directory
         (mkdir-cmd (format nil "mkdir --parents ~a" target-exp-namestring))
         (mkdir-output (uiop:run-program mkdir-cmd :output :string))
         )
    (dbug :top "mkdir-cmd: ~s" mkdir-cmd)
    (dbug :top "mkdir-output: ~s" mkdir-output)
    (let* (
           ;; copy the target file to exper dir so that it can be accessed inside docker containers too, for sure (via always having /scratch)
           (cp-cmd (format nil "cp ~a ~a" target-pathname target-exp-namestring))
           (cp-output ;;(uiop:run-program cp-cmd :output :string)
             #+ccl
             (ccl:copy-file target-pathname target-exp-namestring)
             #-ccl
             (uiop:copy-file target-pathname target-exp-namestring)
             ))
      (dbug :top "cp-cmd: ~s" cp-cmd)
      (dbug :top "cp-output: ~s" cp-output)
      ;; firmadyne docker image.
      (let* ((run-binwalk-cmd (format nil "../tools/run-binwalk ~a ~a" target-filename target-exp-namestring))
             (ignore1 (dbug :top "run-binwalk-cmd: ~s" run-binwalk-cmd))
             (run-binwalk-output (run-command run-binwalk-cmd))
             (ignore2 (dbug :top "run-binwalk-output: ~s" run-binwalk-output))
             ;; now run find interesting executables on the directory that the run binwalk extracted to
             (find-interesting-executables-cmd (format nil "../tools/find-interesting-executables ~a" target-exp-namestring))
             (ignore3 (dbug :top "find-interesting-executables-cmd: ~s%" find-interesting-executables-cmd))
             (find-interesting-executables-output (uiop:run-program find-interesting-executables-cmd :output :string)))
        (declare (ignore ignore1 ignore2 ignore3))
        (dbug :top "run-binwalk-cmd: ~s" run-binwalk-cmd)
        (dbug :top "run-binwalk-output: ~s" run-binwalk-output)
        (dbug :top "find-interesting-executables-cmd: ~s" find-interesting-executables-cmd)
        (dbug :top "find-interesting-executables-output: ~s" find-interesting-executables-output)
        (eval-all-from-string find-interesting-executables-output )))
    (toolchain-command (format nil "chmod -R 777 ~A" target-exp-namestring))
    ))

(defun handle-pcap-target (target)
  "Examine an incoming target and create new testable objects."
  (let* ((target-pathname (namestring (path target)))
         (target-exp-pathname (merge-pathnames (format nil "target-~d" (name target))
                                               (make-pathname :directory *experiment-dir*)))
         (target-exp-namestring (namestring target-exp-pathname))
         ;; create new directory
         (mkdir-cmd (format nil "mkdir --parents ~a" target-exp-namestring))
         (mkdir-output (uiop:run-program mkdir-cmd :output :string))
         (bin (make-instance 'bin :original-path target-pathname))
         )
      (dbug :amp-deep "mkdir-cmd: ~s~%" mkdir-cmd)
      (dbug :amp-deep "mkdir-output: ~s~%" mkdir-output)
      (setf (target bin) target)
      (make-new-pcap-task bin -1))
      (dbug :amp "Proto experiment dir: ~A" *experiment-dir*))
;;;    (dbug :top "list-images-cmd: ~s~%" list-images-cmd)
;;;    (dbug :top "list-images-output: ~s~%" list-images-output)
;;;    (dbug :top "image-paths: ~s~%" image-paths)

(defun triage-neo-target (target)
  "Examine an incoming target and create new testable objects."
  (dbug :timeline "BEGIN triage for ~a." (brief-string target))
  (cond
    ;;handling archive into pereti
    ((and *use-pereti-unpack* (or (eql (source target) :external)
                                  (eql (source target) :pereti)))
     (dbug :amp "pereti target handling in optimus for ~A" target)
     (when (not (assign-master-fuzzbomb target :host (host target)))
       (dbug :amp "Could not find fuzzbomb for ~A with host ~A" target (host target))
       (push target (tasks *self*))
       )
     )

    ;; handling pereti artifact output
    ((and *use-pereti-unpack* (eql (source target) :pereti-final))
     (dbug :amp "pereti target artifact being handled: ~A" target)
     (when (not (assign-master-fuzzbomb target))
       (dbug :amp "Could not find fuzzbomb for ~A" target)
       (push target (tasks *self*))
       )
     )

    ;; handling new target from heapbuster task
    ((getattr :clib-vuln target)
     (dbug :amp "heapbuster target being handled: ~A" target)
     (push target (tasks *self*))
    )

    ;; handling new target from fuzzball task
    ((getattr :exploit target)
      (dbug :amp "demo-exploit target being handled: ~A" target)
      (push target (tasks *self*))
    )

    ;; handling new target from pereti-warnings or pereti-cfg
    ((or (getattr :pereti-warning-path target)
         (getattr :pereti-no-warnings  target)
         (getattr :pereti-cfg-path     target)
         (getattr :pereti-jump-path    target))
     (push target (tasks *self*)))

    ;; default behavior that treats target as an archive, distributes it
    (t (let* ((target-pathname (namestring (path target)))
              (target-exp-pathname (merge-pathnames (make-pathname :directory `(:relative ,(format nil "target-~8,'0d" (name target)) "contents"))
                                                    (make-pathname :directory *experiment-dir*)))
              (target-exp-namestring (namestring target-exp-pathname))
              ;; create new directory
              (mkdir-cmd (format nil "mkdir --parents ~a" target-exp-namestring))
              (mkdir-output (uiop:run-program mkdir-cmd :output :string))
              ;; now, run the list-images command and collect the output
              (list-images-cmd (format nil "../tools/list-images ~a ~a" target-pathname target-exp-namestring))
              (list-images-output (uiop:run-program list-images-cmd :output :string))
              (image-paths (cl-ppcre:split #\Newline list-images-output)))
         (dbug :triage "mkdir-cmd: ~s~%" mkdir-cmd)
         (dbug :triage "mkdir-output: ~s~%" mkdir-output)
         (dbug :triage "list-images-cmd: ~s~%" list-images-cmd)
         (dbug :triage "list-images-output: ~s~%" list-images-output)
         (dbug :triage "image-paths: ~s~%" image-paths)
         (make-targets-for-neo-target target target-exp-pathname image-paths)

         ;; assignment business from fast-triage
         (let ((num-unassigned-fbs (length (unassigned-fuzzbombs *self*)))
               (num-targets (length (targets *self*))))
           (dbug :triage "I have ~d unassigned FBs and ~d targets." num-unassigned-fbs num-targets)))))
  (dbug :timeline "END triage for ~a." (brief-string target)))

(defun make-targets-for-neo-target (neo-target target-dir-pathname image-paths)
  (let ((bin-ctr 0))
    ;; each path points to a binary for analysis
    (dolist (image-path image-paths)
      (dbug :triage "image-path: ~s~%" image-path)
      ;; move image to bin directory
      (let* ((bin-dir (merge-pathnames (make-pathname :directory `(:relative :up ,(format nil "bin-~8,'0d" bin-ctr)))
                                       target-dir-pathname))
             (inner-dir (merge-pathnames (make-pathname :directory '(:relative "bin"))
                                         bin-dir))
             (target-path (make-pathname :directory (pathname-directory inner-dir)
                                         :name  (pathname-name image-path)
                                         :type (pathname-type image-path))))
        (dbug :triage "bin-dir: ~s~%" bin-dir)
        (dbug :triage "inner-dir: ~s~%" inner-dir)
        (dbug :triage "target-path: ~s~%" target-path)
        (ensure-directories-exist inner-dir)
        (dbug :timing "after ensure-directories-exist [~s]" (get-internal-real-time))
        (rename-file image-path inner-dir)
        (dbug :timing "after cp-file-output [~s]" (get-internal-real-time))
        (incf bin-ctr)
        (let* ((bin (make-instance 'bin :original-path bin-dir))
               (new-target (make-instance 'target
                                          :path target-path
                                          :dir inner-dir
                                          :source :triage))
               (hist-tree-node (make-instance 'hist-tree-target-node
                                              :target new-target
                                              :parent (hist-tree-node neo-target))))
          (push hist-tree-node (children (hist-tree-node neo-target)))
          (push new-target (targets *self*))
          (setf (target bin) new-target)
          (when (optimus-prime-p *self*)
            (unless (assign-master-fuzzbomb new-target) ;; if fails to find a master, returns nil
              (dbug (:triage :amp) "Adding task for target: ~A" new-target)
              (push new-target (tasks *self*))
              (dbug (:triage :amp-deep) "Tasks are now: ~A" (tasks *self*))))
          )))))

(defun make-new-pcap-task (pcap id)
  ;; pcap of type bin
  (dbug :amp "make-new-pcap-task for target ~A with path ~s" id (path pcap))
  ;;(let* ((task (make-instance 'pcap-task :pcap pcap :nickname "AFL"))
  (let* ((task (make-instance 'pcap-task :id id :pcap pcap :nickname (format nil "PCAP-~A-~A-~A" (condor-submit-node) (condor-cluster) (name *self*))))
         ;;(output-dirname (strcat (namestring (path pcap)) "-output/"))
         (output-dirname (strcat (namestring *experiment-dir*) "/output/"))
         (output-pathname (parse-namestring output-dirname))
         )
    ;; create dirs
    (ensure-directories-exist output-pathname)
    (setf (input-pathname task) (namestring (path pcap))) ;; This is actually a filename (not path)
    (setf (output-pathname task) output-pathname)
    (push task (tasks *self*))
    task))

;;; -------------------------------------------------------------------------
(defun process-reboot-msg (msg)
  (dbug :amp "Got reboot msg ~a" msg)
  (when *reboot-in-progress*
    (error "Already in the reboot process"))
;  (when (getassoc :rebuild msg)
;    (build-rts))
  (setf *reboot-in-progress* T)
  (setf *halt* T))

;; List of (target task) entries from (lacrosse-tasks amp) that have reported dead.
;; If we ever think about re-assigning a task that is on this list, it suggests the task 
;; had already killed an agent and reassigning is probably a bad idea and could kill everyone.
(defvar *death-tasks* nil)

(defun process-eof-msg (msg sock)
  (declare (special *reassign-dead-amp-tasks*))
  (let* ((sockpair (find-sockpair-from-sock sock)) ;; sockpair is (amp somethingorother)
         (dead-amp (when sockpair (first sockpair)))
         last-taskpair ;; a list of (target task)
        )

    (format T "WARNING got unexpected EOF msg ~A from ~A!" msg dead-amp)
    (cond (sock
           (setf (peer-sockets *self*)
             (remove-if #'(lambda (s) (eq (second s) sock)) (peer-sockets *self*)))
           (dbug :top "Before remove dead sock ~A, list is ~a long" sock (length *sockets*))
           (setf *sockets* (remove sock *sockets*))
           (dbug :top "After remove dead sock, list is ~a long" (length *sockets*))

           (when dead-amp
                (format T "Removing ~A from *amps* and *fuzzbombs*" dead-amp)
                (dbug :top "Before remove dead amp ~A, list is ~a long" dead-amp (length *amps*))
                (setf *amps* (delete dead-amp *amps*))
                (dbug :top "After remove dead amp, list is ~a long" (length *amps*))
                (setf *fuzzbombs* (delete (shortname dead-amp) *fuzzbombs* :test #'string-equal))
                (dbug :top "Before remove dead amp ~A, list is ~a long" dead-amp (length (unassigned-fuzzbombs *self*)))
                (setf (unassigned-fuzzbombs *self*) (delete (shortname dead-amp) (unassigned-fuzzbombs *self*) :test #'string-equal))
                (dbug :top "After remove dead amp, list is ~a long" (length (unassigned-fuzzbombs *self*)))
                (setf (master-fuzzbombs *self*) (delete (shortname dead-amp) (master-fuzzbombs *self*) :test #'string-equal))
                (setf last-taskpair (first (lacrosse-tasks dead-amp)))  ;; a list of (target task)
                (cond ((and *reassign-dead-amp-tasks*
                        (not (member last-taskpair *death-tasks* 
                             :test #'(lambda (tpa tpb) (and (eql (first tpa) (first tpb))
                                                                (eql (second tpa) (second tpb))))))) ;; consider retasking

                        (assign-challenge-project (first last-taskpair) :next-task (second last-taskpair))
                        (push last-taskpair *death-tasks*)
                        )
                      (T 
                        (dbug :top "Warning-- I think we have a death task, never reassigning again")
                        (setf *reassign-dead-amp-tasks* nil)))
           )
;          (format T "closing socket ~A" sock)
;           (close sock)
           )
          ;; otherwise we couldnt find sock to close, and we're
          ;; gonna have big problems...
          (T
           (error "~% WARNING: COULD NOT FIND SOCK THAT GOT EOF~%")))
    ;;(handle-dying-amp (first sockpair))
    ))

;;; -------------------------------------------------------------------------
(defun find-sockpair-from-sock (sock)
  (find sock (peer-sockets *self*) :key #'second))

;;; -------------------------------------------------------------------------

(defun process-new-master-msg (msg)
  (dbug :top "Got new-master msg ~A" msg)
  (setf *master-amp* (find-amp (getassoc :who msg)))
  )

;;; -------------------------------------------------------------------------
;;; for now, we do this based on the *amps* list:
;;; after removing the dying guy, if we are next optimus in the list, we are master.

(defun i-am-next-master (dying-amp)
  (eq *self* (first (remove dying-amp (remove-if-not #'optimus-p *amps*)))))

;;; -------------------------------------------------------------------------
;;; This function changes the status of dying contracts to :failed
;;; and resubmits them as tasks. Failed contracts are then restored
;;; and reannounced.

;;; NOTE when a guy dies we should either remove from *amps* so we dont
;;; expect him to bid on contracts (and dont send him announcements) or
;;; do something like that to handle those potential problems.  *living-amps*?

(defun process-dying-msg (msg)
  (let* ((dying-name (getassoc :who-died msg))
         (halt-status (getassoc :halt-status msg))
         (dying-amp (find-amp dying-name))
         (dying-socket (socket-to dying-amp)))

    (dbug :top "Got dying msg from ~A, halt-status ~a, cleaning him up" dying-name halt-status)
    ;; Clean up the socket
    ;; Forget the socket but don't close it (for now).
    ;;  (close (socket-to dying-amp))
    (setf (peer-sockets *self*)
      (remove-if #'(lambda (s) (eq (first s) dying-amp)) (peer-sockets *self*)))
    (setf *sockets* (remove dying-socket *sockets*))
    ;;(handle-dying-amp dying-amp)
))

;;; -------------------------------------------------------------------------

(defun process-init-msg (msg)
  (let ((from (getassoc :from msg)))

    (dbug :top "Got init msg from ~A" from)
    ))

;;; -------------------------------------------------------------------------

;;;-------------------------------------------------------------------------
;;; Announcement Message:
;;;
;;; This code has been generalized to support several negotiation
;;; strategies. Currently, we just support bid and no-bid, stored
;;; in the global *negotiation-protocol*. Once a contract announcement
;;; msg is processed, the right class of contract is instantiated,
;;; and methods for various functions can be specialized on the class.

(defun process-announcement-msg (msg)
  (ecase *negotiation-protocol*
    ;; Use single-agent (bidding) contracts
    (:bid (process-announcement-msg-with-bidding msg))
    ;;    ;; Else, must be no-bid, or error
    ;;    (:no-bid (process-announcement-msg-no-bidding msg))
    ))


;;; Not done.
;;; If no bidding, make a multi-agent contract, and immediately
;;; taskify it.
;;;(defun process-announcement-msg-no-bidding (msg)
;;;  (let ((contract (make-multi-agent-torg-contract-from-msg msg)))
;;;  (push contract (contracts *self*))
;;;  (send-message (manager contract) :bid
;;;               :contract-name (name contract) :value bid)
;;;  ))

;;; Code for :bidding protocol.
;;;
;;; The method compute-bid is specialized on the type of contract obj.
;;; A value of -1 is returned for no-bid (not capable).
;;; (The default initial bid for a contract is 0.)
;;; Some positive value should be returned for a legitimate bid.
;;;
;;; The announcer and each bidder make their own contract object.

;;; Note: Now we want to implement a no-bidding protocol.
;;; Should we just change this code in place, or somehow preserve it
;;; in case we want to go back to bidding someday?


(defun process-announcement-msg-with-bidding (msg)
  (let* ((contract (make-contract-from-msg msg))
         (bid (compute-bid contract)))
    (push contract (contracts *self*))
    (send-message :to (manager contract)
                  :type :bid :contract-name (name contract) :value bid)
    ))


;;;-------------------------------------------------------------------------
;;; Bid Message:
;;; Someone bidding on a contract we announced.

(defun process-bid-msg (msg)
  (let ((contract (find-contract (getassoc :contract-name msg)))
        (from (getassoc :from msg))
        (bid (getassoc :value msg)))

    (dbug :top "Rcvd bid ~A on ~A from ~A" bid contract from)
    ;;  (cond ((> *sim-time* (bid-deadline contract))
    ;;         (dbug :top "WARNING: Bid msg rcvd after bid deadline, ignored"))
    ;;        (T (push (list from bid) (bids contract))))
    (push (list from bid) (bids contract))
    ;; if we have all the bids, put this contract on the tasks list so
    ;; it gets attention to award it...
    (when (= (length (bids contract)) (1- (length *amps*)))
      (push contract (tasks *self*)))
    ))

;;;-------------------------------------------------------------------------
;;; Award Message:
;;; Someone got a contract.  If it was us, do something.

;;; Old. One task per contract.
;;;(defun process-award-msg (msg)
;;;  (let* ((contract-name (getassoc :contract-name msg))
;;;        (contract (find-contract contract-name)))

;;;  (when contract
;;;    (cond ((string= (getassoc :contractor msg) (name *self*))
;;;          (dbug :top "Accepting award of contract ~A" (name contract))
;;;          (setf (status contract) :accepted)
;;;          (setf (contractor contract) *self*)
;;;          (aid-set-contract-flag contract :MINEUNPLANNED)
;;;          ;;(push contract (tasks *self*))
;;;               ;; rather than doing this via the task list, just do it here.
;;;          (execute-contract contract)
;;;          )
;;;        (T
;;;         (setf (status contract) :awarded-to-another)
;;;        (aid-set-contract-flag contract :NOTMINE)
;;;         (dbug :top "Contract ~A awarded to someone else" contract)))
;;;  )))

;;;-------------------------------------------------------------------------
;;; Someone won a contract. If it was us, add all of the new tasks
;;; implied by that contract to our task list.
;;; Else mark and ignore.

(defun process-award-msg (msg)
  (let* ((contract-name (getassoc :contract-name msg))
         (contract (find-contract contract-name)))

    (when contract
      (cond ((string= (getassoc :contractor msg) (name *self*))
             (dbug :top "Accepting award of contract ~A" (name contract))
             (setf (status contract) :accepted)
             (setf (contractor contract) *self*)
             ;; Will need to show combos on display now.
             ;(aid-set-contract-flag contract :MINEUNPLANNED)
             ;;
             ;; Old: Rather than doing this via the task list, just do it here.
             ;;(push contract (tasks *self*))
             ;;
             ;; New: Rather than treating a contract like a task, add all new
             ;; plan-config-tasks implied by contract to the task list
             ;; Easiest to do as a side-effect.
             ;(add-implied-configs-and-tasks contract)
             )
            (T
             (setf (status contract) :awarded-to-another)
             (setf (contractor contract) (find-amp (getassoc :contractor msg)))
             ;(aid-set-contract-flag contract :NOTMINE)
             (dbug :top "Contract ~A awarded to someone else" contract)))
      )))

;;;-------------------------------------------------------------------------
;;; Map over all configs (planned and pending) for this phase only.
;;; New number of plan-config tasks should be 2n, for n existing configs.
;;; Example: Configs before:  init, init+t1 (aka, t1)                  (n=2)
;;; Add t2:  After: init, t1, init+t2 (t2), t1+t2                      (n=4)
;;; Add t3: init, t1, t2, t1+t2, init+t3 (t3), t1+t3, t2+t3, t1+t2+t3  (n=8)
;;;
;;; Note: This side-effect function modifies (tasks *self*).

;;;(defmethod add-implied-tasks ((c contract))
;;;  (setf (tasks *self*)
;;;       ;; new tasks pushed on front of task list
;;;       (append
;;;        ;; create new task
;;;        (mapcar #'(lambda (old-task)
;;;                    (create-new-task-from-old old-task c))
;;;                ;; if old task in correct phase
;;;                (remove-if-not
;;;                 #'(lambda (task) (string= (phasename task) (phasename c)))
;;;                 ;; and old task is a plan-config-task
;;;                 (remove-if-not #'plan-config-task-p (tasks *self*)))
;;;                ;; append to old task list
;;;                (tasks *self*))))
;;;  (dbug: :delib "New task list is ~A" (tasks *self*))
;;;  )

;; Do this correctly with configs, not tasks.
;(defmethod add-implied-configs-and-tasks ((c contract))
;  (let* ((p (find-phase (phasename c)))
;         (cur-configs (all-configs p))
;         (new-configs
;          (mapcar #'(lambda (old-config)
;                      (create-new-config-from-old old-config c))
;                  cur-configs))
;         (new-tasks
;          (mapcar #'(lambda (new-config)
;                      (create-new-task-from-config new-config c))
;                  new-configs))
;         )
;    ;;(dbug :delib-trace "add-implied-configs-and-tasks found existing configs ~A" cur-configs)
;    (setf (configs c) (append (configs c) new-configs))
;    (setf (pending-configs p) (append (pending-configs p) new-configs))
;    (setf (tasks *self*) (append (tasks *self*) new-tasks))
;    ))


;;; Create a new config from an old config plus new contract.
;;; Note that add-threat/goal is destructive!
;(defmethod create-new-config-from-old (old-config new-contract)
;  (let ((new-config
;         (make-instance 'config
;           :mission-phase (mission-phase old-config)
;           :api-list (api-list old-config)
;           :plan nil
;           :contracts (append (contracts old-config)
;                              (list new-contract)))))
;    ;; Destructively modify api-list of new config
;    (if (threat-contract-p new-contract)
;        (add-threat new-config (threat new-contract))
;      (add-goal new-config (goal new-contract)))
;    (dbug :combo "Created new config ~A  from old config ~A for contracts: ~A with TorG ~A"  new-config old-config (contracts new-config) (if (threat-contract-p new-contract) (threat new-contract) (goal new-contract)))
;    (dbug :combo "      API list: ~A"  (api-list new-config))
;
;    new-config                          ;return the result
;    ))
;
;;; Create a new task from an old task plus new contract.
(defmethod create-new-task-from-config ((new-config config) new-contract)
  (let ((new-task
         (make-instance 'plan-config-task
           ;; concatentation of all involved contracts
           ;;:name (strcat (name old-task) (name new-contract))
           :name (name new-contract)
           :contracts (contracts new-config)
           :config new-config)))
    (dbug :combo "Created new task ~A  for contracts: ~A"
          new-task (contracts new-task))
    (setf (task new-config) new-task)
    new-task                            ;return the result
    ))

;;;-------------------------------------------------------------------------
(defun find-contract (c)
  (find c (contracts *self*) :key #'name :test #'string=))

(defun find-contracts-for-phasename (phasename)
  (remove-if-not #'(lambda (c) (string= phasename (phasename c)))
                 (contracts *self*)))

(defun find-plan-config-tasks-for-phasename (phasename)
  (remove-if-not #'(lambda (task)
                     (and (plan-config-task-p task)
                          (string= phasename (phasename task))))
                 (tasks *self*)))

;;;-------------------------------------------------------------------------
;;; - this is for contracts that have nobody bidding at all, incl master...
;;; - for now, we just print this msg and they probably stay red or pink
;;; on the AID...

(defun fail-contract (c)
  (dbug :top "Failing ~A" c)
  (setf (status c) :failed))

;;;-------------------------------------------------------------------------
;;; Report Message:
;;; Status update on a contract from the contractor.

(defun process-report-msg (msg)
  (let ((contract (find-contract (getassoc :contract-name msg))))

    (setf (status contract) (getassoc :status msg))
    (dbug :top "Got ~A report for ~A" (status contract) contract)
    ))

;; (format t "WARNING: report msg ~A ignored!" msg)

;;;  ;;(dbug :top "                NOTE: no constraints asserted by reports")
;;;  (case (status contract)
;;;        (:started
;;;              (make-instance 'constraint :begin (time0) :end (begin contract)
;;;                      :bounds (make-range (getassoc :start-time msg)
;;;                                              (getassoc :start-time msg)))
;;;              (check-schedule "status msg setting start of contract")
;;;                )
;;;        (:finished
;;;              (make-instance 'constraint :begin (time0) :end (end contract)
;;;                      :bounds (make-range (getassoc :end-time msg)
;;;                                              (getassoc :end-time msg)))
;;;              (check-schedule "status msg setting end of contract")
;;;                )
;;;        (:failed (setf *contracts* (delete contract *contracts*))
;;;                 (push *contracts* *failed-contracts*))
;;;        (T (error "Unknown status report type")))
;;;)



;;;-------------------------------------------------------------------------


(defun award-contract (contract)
  (let ((my-bid (compute-bid contract))
        (winning-bid
         (if (bids contract)    ;; trap for no bids
             (rank-and-choose #'second #'max (bids contract))
           nil)))

    (dbug (:top :award) "Awarding contract ~A  My-bid: ~A  Winning-bid: ~A"
          contract my-bid winning-bid)

    (cond ((and (<= my-bid 0) (null winning-bid))
           (fail-contract contract)
           (dbug :top "WARNING: No bids, infeasible contract announcement ~A failed!" contract))
          ;; if best remote bid is better than ours, award to
          ;; remote contractor, else take contract ourselves.
          ((and winning-bid (> (second winning-bid) my-bid))
           (setf (contractor contract) (find-amp (first winning-bid)))
           (setf (status contract) :awarded-to-another)
           ;(aid-set-contract-flag contract :NOTMINE)
           (broadcast-message :type :award
                              :contract-name (name contract)
                              :contractor (name (contractor contract)))
           (dbug (:top :award) "Awarded ~A to ~A with bid ~A"
                 contract (contractor contract) (second winning-bid))
           )
          (T ;; Award to self
           ;; Will need to show combos on display now.
           ;(aid-set-contract-flag contract :MINEUNPLANNED)

           (dbug (:top :award) "Awarded ~A to self with bid ~A" contract my-bid)
           (dbug :top "Accepting award of contract ~A" (name contract))

           (setf (contractor contract) *self*)
           (setf (status contract) :accepted)
           (broadcast-message :type :award
                              :contract-name (name contract)
                              :contractor (name *self*))
           )
          )))

;;; -------------------------------------------------------------------------
(defun sort-amp-contracts ()
  (setf (contracts *self*)
    (sort (contracts *self*) #'<
          :key #'(lambda (c) (position (phasename c) *phasename-order*)))))

;;; ---------------------------------------------------------------------
;;; Utility methods.
;;; ---------------------------------------------------------------------

;;; Make a task to create an original plan for each config on initial
;;; pending-configs.
(defmethod create-init-config-tasks ((mission mission))
  (dolist (p (phases mission))
    (dbug :test "Creating init config tasks for phase: ~A" p)
    (dolist (c (pending-configs p))
      (dbug :test "Creating init config task for config: ~A" c)
      (let ((new-task (create-config-task-for-config c)))
        (setf (task c) new-task)
        (push new-task (tasks *self*))))))

(defmethod create-config-task-for-config ((config config))
  (make-instance 'plan-config-task :name "Init" :config config))

;;; ---------------------------------------------------------------------
;;; Turn each threat into a contract.

(defmethod create-contracts-for-threats ((mission mission) &aux c)
  (dolist (threat (threats mission))
    (when (or (null (first threat)) (null (second threat))
              (null (third threat)))
      (error "threat ~A specified in -amp file is missing a value"
             threat))
    (setf c (make-instance 'threat-contract
              :manager (name *self*)
              :threat (second threat)
              :lethality (third threat)
              :phasename (first threat)))
    (push c (contracts *self*))
    (push c (tasks *self*))))

;;; ---------------------------------------------------------------------
;;; Turn each goal into a contract

(defmethod create-contracts-for-goals ((mission mission) &aux c)
  (dolist (g (goals mission))
    (setf c (make-instance 'goal-contract
              :manager (name *self*)
              :goal (second g)
              :opp-prob (third g)
              :phasename (first g)))
    (push c (contracts *self*))
    (push c (tasks *self*))))


;;; ---------------------------------------------------------------------
;;; here are some handlers for domain-specific msgs; probably should
;;; have a separate way to include these
;;; ---------------------------------------------------------------------

;;; ---------------------------------------------------------------------
(defun find-contract-for-goal (g)
  (find g (contracts *self*) :key #'goal))

(defun process-add-phase-msg (msg)
  (let* ((add-phase-task (make-add-phase-task-from-msg msg)))
    ;; [jrye:20120229.1548CST] Instead of adding this to the task
    ;; list, we execute it immediately. Sometimes we process an
    ;; add-phase message and then contract messages in a single pass
    ;; through the loop. When this happens, if we put the add-phase
    ;; task on the list, we may try to bid on a contract before the
    ;; phase exists, thus triggering an error.
    ;;
    ;; (push add-phase-task (tasks *self*))
    (dbug :delib "Received an add-phase message, created and about to execute task: ~A" add-phase-task)
    (execute-task add-phase-task)
    )
  )

(defmethod execute-task ((task add-phase))
  "Add a phase to the AMP's mission."
  (dbug :delib "Adding phase (~A) to mission" (phase-name task))

  ;; When we are the master, send the add-phase task to our
  ;; collaborators too.
  (when (master-p *self*)
    (broadcast-message :type :add-phase
                       :phase-name (phase-name task)
                       :initial-states (initial-states task)
                       :goals (goals task)
                       :goal-skills (goal-skills task)
                       :threats (threats task)
                       :threat-skills (threat-skills task)
                       :duration (duration task)
                       ))

  ;; Verify that the phase-name is not in the phase list already. For
  ;; now, we are making unique names the task's responsibility. We
  ;; could do that ourselves right here.
  (assert (null (find-phase (phase-name task))))
  (assert (null (find (phase-name task) *phasename-order*)))

  ;; Make the init config and the phase to add.
  (let* ((mission (mission *self*))
         (init-config
          (make-instance 'config
            :api-list
            (list 'def-problem (phase-name task)
                  :version (name mission)
                  :initial-states (initial-states task))))
         (new-phase
          (make-instance 'mission-phase
            :name (phase-name task)
            :pending-configs (list init-config)
            :duration (duration task)
            :mission mission)))
    (setf (mission-phase init-config) new-phase)

    ;; Create and add the goals and threats.
    (when (master-p *self*)
      (dbug :delib-trace "Adding goals to phase (~A): ~A" (phase-name task) (goals task))
      (dolist (g (goals task))
        (push (list (phase-name task) (first g) (second g)) (goals mission))
        (let ((c (make-instance 'goal-contract
                   :manager (name *self*)
                   :goal (first g)
                   :opp-prob (second g)
                   :phasename (phase-name task))))
          (push c (contracts *self*))
          (push c (tasks *self*))
          )))

    (when (master-p *self*)
      (dbug :delib-trace "Adding threats to phase (~A): ~A" (phase-name task) (threats task))
      (dolist (threat (threats task))
        (push (list (phase-name task) (first threat) (second threat)) (threats mission))
        (let ((c (make-instance 'threat-contract
                   :manager (name *self*)
                   :threat (first threat)
                   :lethality (second threat)
                   :phasename (phase-name task))))
          (push c (contracts *self*))
          (push c (tasks *self*))
          )))

    ;; First add the phase-specific skills, then the mission-wide
    ;; ones.
    (dolist (gs (append (goal-skills task) (goal-skills mission)))
      ;;(add-goal-skill new-phase (first gs) (second gs) (third gs))
      (add-goal-skill new-phase gs))

    (dolist (ts (append (threat-skills task) (threat-skills mission)))
      ;;(add-threat-skill new-phase (first ts) (second ts) (third ts))
      (add-threat-skill new-phase ts))

    ;; And, we need to set up the timing for the phase.
    (let ((q-ctr 0)
          (last-phase (car (last (phases mission)))))
      (when last-phase
        (setf q-ctr (end-time last-phase)))
      (setf (start-time new-phase) q-ctr)
      (setf q-ctr (+ q-ctr (duration new-phase))))

    ;; Create a config-task and add it to the task list.
    (dbug :delib-trace "Create config-task for new phase.")
    (let ((new-task (create-config-task-for-config init-config)))
      (setf (task init-config) new-task)
      (push new-task (tasks *self*)))

    ;; Lastly, we add the phase to the mission and the phasename-order.
    (dbug :delib-trace "Appending new phase to phases and phasename-order lists.")
    (setf (phases mission) (append (phases mission) (list new-phase)))
    (setf *phasename-order* (append *phasename-order* (list (phase-name task))))

    (dbug :delib-trace "mission has phases: ~A" (phases mission))
    (dbug :delib-trace "phasenames: ~A" *phasename-order*)

    ;; If we don't have any phase yet, just right into this one.
    ;; [jrye:20120221.1407CST] FIXME I'm not sure how this works if
    ;; the RTS is determining the phase...
    (when (null *cur-phase*)
      (setf *cur-phase* new-phase)
      ;; (find-phase (first *phasename-order*)))
      (dbug :delib "Set cur-phase to ~A" *cur-phase*)
      )
    ))

(defun process-modify-current-phase-msg (msg)
  "Modify the currently-executing phase in the AMP's mission.  This might eventually work for any phase.
   Will raise error, if specified phase is not the current phase."
    (declare (ignore msg))
    (dbug :delib "NOT DOING modifying current phase" )
)

(defun process-modify-skills-msg (msg)
  "Modify skills stored in amp.  May require re-bidding or re-planning.
   For now, require new goal and threats skills."
    (declare (ignore msg))
    (dbug :delib " NOT DOING Modifying amp skills."))

(defun select-blocked-task (&key (blocked-status :blocked-waiting-for-fb))
  "Select task blocked on blocked-status."
  (dbug :delib-trace "select-blocked-task on status ~s." blocked-status)
  (loop for task in (tasks *self*)
     with best-score = -.0001   ;; FIXME why is this not zero? non-positive score means no value in doing a task.
     with best-task = nil
     when (eq :blocked-waiting-for-fb (status task))
       do (let ((score (funcall #'scoring-fn task)))
            (when (> score best-score)
              (dbug :delib-trace "Found new best-task ~s with score ~s." task score)
              (setf best-score score)
              (setf best-task task)))
     finally (return best-task)))

(defun process-idle-msg (msg)
  "When a master fuzzbomb reports it is idle, try to assign a task immediately.
   If no task suits, update the bookkeeping."
  (dbug :delib "Handling idle msg: ~s" msg)
  (let ((from (getassoc :from msg))
        (unblocked-task (select-blocked-task)))
    (cond (unblocked-task
           (assign-master-fuzzbomb unblocked-task :fb from)
           (setf (tasks *self*) (remove unblocked-task (tasks *self*)))
           )
          (t   ; This stuff needs testing.
           (dbug :delib "This is where we mark agent as free for future tasks.")
           (setassoc from nil (master-fuzzbombs *self*) :test #'string-equal)
           (dbug :amp-deep "(master-fuzzbombs *self*): ~s" (master-fuzzbombs *self*))
           (release-fuzzbomb from)
           (dbug :amp-deep "(unassigned-fuzzbombs *self*): ~s" (unassigned-fuzzbombs *self*))
           ))))

  ;; example of start of harnesses slot entry
  ;; (:HARNESSES (:ID_1 (:NAME . "filein_harness") (:SOURCE . "src/test/filein_harness.c") (:BINARY . "out/filein_harness")))))

(defun process-lacrosse-cp-updates (target msg)

    ;; [Pavan K: If the target has a "project-properties" (specifically lacrosse target), then
    ;; add project properties to the target. lacrosse-build-task updates "project-properties" of a lacrosse
    ;; target, but that info does not get propagated here. Same thing with "harnesses." It seems that the targets used
    ;; by the FUZZBOMB agents are not pointers to OPTIMUS' target.]
    (when (and (slot-exists-p target 'project-properties)
               (assoc :project-prop msg))
      ;; Should be created via cp-init task

      (dbug :top "Project properties ~a made by a process. Adding it to the target..."
        (getassoc :project-prop msg))
      (setf (project-properties target) (getassoc :project-prop msg))
      (setf (harnesses target) (getassoc :harnesses msg))
      (setf (sanitizers target) (getassoc :sanitizers msg)))

    (when (and (slot-exists-p target 'source-name)
               (slot-exists-p target 'source-path)
               (assoc :source-name msg)
               (assoc :source-path msg))
      ;; Should be created via cp-init task

      (dbug :top "Source name ~a and path ~a was created by a process. Adding it to the target..."
        (getassoc :source-name msg)
        (getassoc :source-path msg))
      (setf (source-name target) (getassoc :source-name msg))
      (setf (source-path target) (getassoc :source-path msg)))

    (when (and (slot-exists-p target 'built-p)
               (assoc :built-p msg))
      ;; Should be created via cp-build

      (dbug :top "Init build complete flag (value=~a) was set by a process. Updating target..."
        (getassoc :built-p msg))
      (setf (built-p target) (getassoc :built-p msg)))

    (when (and (slot-exists-p target 'make-pov-blob)
               (assoc :make-pov-blob msg))
      ;; Should be updated via cp-build, gen-pov-blob, and run-pov

      (dbug :top "Make PoV blob flag (value=~a) was set by a process. Updating target..."
        (getassoc :make-pov-blob msg))
      (setf (make-pov-blob target) (getassoc :make-pov-blob msg)))

    (when (and (slot-exists-p target 'run-pov)
               (assoc :run-pov msg))
      ;; Should be updated via gen-pov-blob and run-pov

      (dbug :top "Run PoV flag (value=~a) was set by a process. Updating target..."
        (getassoc :run-pov msg))
      (setf (run-pov target) (getassoc :run-pov msg)))

    (when (and (slot-exists-p target 'blobs)
               (assoc :blobs msg))
      ;; Should be created via gen-pov-blob

      (dbug :top "PoV blob path ~a was created by a process. Adding it to the target..."
        (getassoc :blobs msg))
      (setf (blobs target) (getassoc :blobs msg)))

    (when (and (slot-exists-p target 'make-patch)
               (assoc :make-patch msg))
      ;; Should be adjusted via run-pov, gen-patch, and build-with-patch

      (dbug :top "Make patch flag (value=~a) was set by a process. Updating target..."
        (getassoc :make-patch msg))
      (setf (make-patch target) (getassoc :make-patch msg)))

    (when (and (slot-exists-p target 'patches)
               (assoc :patches msg))
      ;; Should be created via gen-patch

      (dbug :top "Patch path ~a was created by a process. Adding it to the target..."
        (getassoc :patches msg))
      (setf (patches target) (getassoc :patches msg)))

    (when (and (slot-exists-p target 'build-with-patch)
               (assoc :build-with-patch msg))
      ;; Should be updated via gen-patch and build-with-patch

      (dbug :top "Build with patch flag (value=~a) was set by a process. Updating target..."
        (getassoc :build-with-patch msg))
      (setf (build-with-patch target) (getassoc :build-with-patch msg)))

    (when (assoc :run-tests-p msg)
      ;; Should be updated via build-with-patch
      (dbug :top "Run test flag (value=~a) was set by a process. Updating target..."
            (getassoc :run-tests-p msg))
      (setf (run-tests-p target) (getassoc :run-tests-p msg)))
)

(defun process-challenge-project-update (msg)
  (dbug :top "Got process-challenge-project-update msg ~A" msg)
  ;; For now, assume nothing interesting in msg beyond target id and dir.
  ;; (and dir only for some sanity check that ids are correct)

  (dbug-target-list *self*)

  ;; When update is rcvd, poss states: (1) done, success! (2) done, fail; (3) more work to do
  ;; How to recognize:
  ;; - (1) if patched, patch-tested, reported are all true.
  ;; - (2) if no tasks apply
  ;; - (3) one or more tasks apply
  (let* ((target-id (getassoc :target-id msg))
         (target (get-target-by-id *self* target-id)))

    (when (equalp (type-of target) 'lacrosse-cp-target)
      (process-lacrosse-cp-updates target msg))

    (dbug :top "target-id: ~s" target-id)
    (dbug :top "target: ~s" target)
    (dbug :top "directory for target: ~a" (dir target))
    (when (getassoc :vuln-confirmed-p msg)
      (dbug :top "json result from llm confirm vuln: ~s"
            (cl-json:decode-json-from-source
             (results-pathname-for-task-class 'lacrosse-llm-confirm-vuln-task target))))

    ;;(reset-file-results target)
    (assign-challenge-project target)
    )
  )

(defun update-pending-work-when-idle ()
  "Called by print-hearbeat."
  )

;; Internal functions to handle formatting of pragmadev tracer messages
(defun tracer-task (isCreated pid &optional taskname)
  ;; Construct message header
  (let ((debug-msg (if isCreated
                        "tracer-task-created: PID="
                        "tracer-task-deleted: PID="))
        (tracer-msg (if isCreated
                        "taskCreated| "
                        "taskDeleted| ")))
    ;; Add PID to debug message, and timestamp to tracer message
    (setf debug-msg (concatenate 'string debug-msg (format nil "~d" pid)))
    (setf tracer-msg (concatenate 'string tracer-msg (format nil "-t~s| " (get-universal-time))))
    ;; Optional fields
    (if taskname
      (progn
        (setf debug-msg (concatenate 'string debug-msg (format nil " PNAME=~s" taskname)))
        (setf tracer-msg (concatenate 'string tracer-msg (format nil "-n~s| " taskname)))))
    ;; Now add PID to tracer message
    (setf tracer-msg (concatenate 'string tracer-msg (format nil "~d|" pid)))
    (dbug :top "~s" debug-msg)
    (dbug :tracer "~s" tracer-msg))
)

(defun tracer-msg (isSend pid signum msgname &optional msgdata taskname messageid)
  ;; Construct message header
  (let ((debug-msg (if isSend
                        "tracer-msg-sent: PID="
                        "tracer-msg-recv: PID="))
        (tracer-msg (if isSend
                        "messageSent| "
                        "messageReceived| ")))
    ;; Add PID to debug message, and timestamp to tracer message
    (setf debug-msg (concatenate 'string debug-msg (format nil "~d" pid)))
    (setf tracer-msg (concatenate 'string tracer-msg (format nil "-t~s| " (get-universal-time))))
    ;; Optional fields
    (if msgdata
      (progn
        (setf debug-msg (concatenate 'string debug-msg (format nil " DATA=~s" msgdata)))
        (setf tracer-msg (concatenate 'string tracer-msg (format nil "-d~s| " msgdata)))))
    (if taskname
      (progn
        (setf debug-msg (concatenate 'string debug-msg (format nil " PNAME=~s" taskname)))
        (setf tracer-msg (concatenate 'string tracer-msg (format nil "-n~s| " taskname)))))
    (if messageid
      (progn
        (setf debug-msg (concatenate 'string debug-msg (format nil " MSGID=~d" messageid)))
        (setf tracer-msg (concatenate 'string tracer-msg (format nil "-i~d| " messageid)))))
    ;; Now add PID to tracer message
    (setf tracer-msg (concatenate 'string tracer-msg (format nil "~d| ~A| ~s|" pid signum msgname)))
    (dbug :top "~s" debug-msg)
    (dbug :tracer "~s" tracer-msg))
)

;; Functions to call for various trace events
(defun tracer-task-created (pid &optional taskname)
  (tracer-task t pid
    (if taskname taskname))
)

(defun tracer-task-deleted (pid &optional taskname)
  (tracer-task nil pid
    (if taskname taskname))
)

(defun tracer-msg-sent (pid signum msgname &optional msgdata taskname messageid)
  (tracer-msg t pid signum msgname
    (if msgdata msgdata)
    (if taskname taskname)
    (if messageid messageid))
)

(defun tracer-msg-recv (pid signum msgname &optional msgdata taskname messageid)
  (tracer-msg nil pid signum msgname
    (if msgdata msgdata)
    (if taskname taskname)
    (if messageid messageid))
)

(defun process-vd-status-msg (msg)
  "Process a vulnerability discovery status message"
  ;; Message has the following structure:
  ;;  "((:type :vd-status) (:status STATUS) (:vd_uuid VD_UUID) (:cpv_uuid CPV_UUID))"
  (let ((status (getassoc :status msg))
        (vd-uuid (getassoc :vd-uuid msg))
        (cpv-uuid (getassoc :cpv-uuid msg))
        (vc-id (getassoc :vc-id msg)))
    (dbug :top "Status received: ~a, VD_UUID=~a, CPV_UUID=~a, vc-id=~a" status vd-uuid cpv-uuid vc-id)
    ;; FIXME: Populate internal state with CPV_UUID
    (cond ((string-equal "accepted" status)
           (let ((vc (find-vc-by-id vc-id (asc-target))))
             (setf (cpv-uuid vc) (getassoc :cpv-uuid msg))
             (assign-next-tasks (asc-target) (gen-patch-next-tasks vc))))
          ((string-equal "rejected" status)
           (let ((vc (find-vc-by-id vc-id (asc-target))))
             (dbug :top "Rejected ~s." vc)
             (setf (rejected-p vc) t)
             ;; see if there's a waiting vc that's now ready-to-submit-p
             (consider-submit-vc (asc-target))
             ))
          (t
           (dbug :top "WARNWARN: unexpected status: ~s" status)))))

;;; FIXME extend this to also iter over the files-to-patch? then can elim choose-file-to-patch
(defun gen-patch-next-tasks (vc)
  "Return list of :next-task forms for gen-patch on vc."
  (append
   (when *use-lacrosse-llm-gen-patch-p*
     (iter (for llm in *lacrosse-gen-patch-llms*)
       (collect `(:next-task
                  ((:task-type lacrosse-llm-gen-patch-task)
                   (:vc-id ,(id vc))
                   (:file-to-patch ,(choose-file-to-patch vc))
                   (:harness-id ,(harness-id vc))
                   (:blob ,(blob vc))
                   (:llm-arg ,llm)
                   (:bic ,(bic vc))
                   (:cp-src ,(cp-src vc)))))))
   (when *use-revert-to-patch-p*
     (try-patching-through-hunk-reversion vc))
   ))

(defun choose-file-to-patch (vc)
  (first (files-in-commit vc)))

(defun process-gp-status-msg (msg)
  "Process a generated patch status message"
  ;; Message has the following structure:
  ;;  "((:type :gp-status) (:status STATUS) (:cpv_uuid CPV_UUID) (:gp_uuid GP_UUID))"
  (let ((status (getassoc :status msg))
        (cpv-uuid (getassoc :cpv-uuid msg))
        (gp-uuid (getassoc :gp-uuid msg))
        (vc-id (getassoc :vc-id msg))
        (pc-id (getassoc :pc-id msg)))
    (dbug :top "Patch status received: ~a, CPV_UUID=~a, GP_UUID=~a" status cpv-uuid gp-uuid)
    (let* ((vc (find-vc-by-id vc-id (asc-target)))
           (pc (find-pc-by-id pc-id vc)))
      (setf (gp-uuid pc) gp-uuid)
      (cond ((string-equal "accepted" status)
             (setf (accepted-p pc) t))
            ((string-equal "rejected" status)
             (setf (rejected-p pc) t)
             ;; FIXME this is where we might refine a patch or re-try w mods
             ;; see if there's a waiting patch that's now ready-to-submit-p
             (consider-submit-patch (asc-target)))))))

(defun asc-target ()
  (first (targets *self*)))

(defun split-string-into-words (input-string)
  (cl-ppcre:split "\\s+" input-string))

