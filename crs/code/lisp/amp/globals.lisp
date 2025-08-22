;;; -------------------------------------------------------------------------
;;; globals.lisp
;;; - Global definitions (mainly variables) for CIRCA Adaptive Mission Planner.
;;; - $Revision: 1.13 $
;;; -------------------------------------------------------------------------
;;; COPYRIGHT START
;;; COPYRIGHT END
;;; -------------------------------------------------------------------------

(in-package :fuzzbomb)

(defvar *long-program-name* "CIRCA AMP")

(defvar *amp-directory* nil
  "Runtime location of amp executable.  Default location of
   domain and performance profile files.  Set in init-globals.")

(defvar *neo-fuzz-home* nil
  "Runtime location of NEO-FUZZ home directory.  Defaults to
   value of NEO_FUZZ_HOME env var, and falls back to a system
   relative path otherwise.  Set in init-globals.")

(defvar *lacrosse* nil
  "Is this Neo-Fuzz instance a Lacrosse (AIxCC entry) agent?")

(defvar *lax-home* nil
  "Runtime location of lacrosse, set in init-globals.")

(defvar *lax-tools* nil
  "Runtime location of lacrosse tools dir for convenience, set in init-globals.")

(defvar *cp-root* nil
  "Supplied by env, set in init-globals.")

(defvar *crs-scratch* nil
  "Supplied by env, set in init-globals.")

;;; TEMP flag to use fake fuzzing tasks for task dispatch testing.
(defvar *fake-fuzz* nil
  "TEMP flag to use fake fuzzing tasks for task dispatch testing.")

;;; TEMP flag to use fake git bisect task for task dispatch testing.
(defvar *fake-git-bisect* nil
  "TEMP flag to use fake git bisect task for task dispatch testing.")

;;; FIXME This TEMP hack only works in 1fb mode.
(defvar *fake-git-bisect-done-p* nil
  "TEMP flag to ensure fake-git-bisect only runs once.")

(defvar *use-afl* t "If non-nil, AFL tasks can be created.")

(defvar *use-libfuzzer* t "If non-nil, libfuzzer tasks can be created.")

(defvar *lacrosse-llm-stub-patch-p* nil
  "Run with the stubbed patching script?")

(defvar *use-lacrosse-llm-gen-patch-p* t
  "When t, Use llm patch gen task.")

(defvar *lacrosse-llm-gen-patch-test-p* t
  "Test the patch after generation.")

(defvar *lacrosse-llm-gen-patch-test-canned-patch-p* nil
  "Use the example patch for testing.")

;;; Checked by lacrosse-gen-pov-blob-task
(defvar *lacrosse-stub-pov-blob-p* nil  
  "Run with the stubbed pov blob script?")

;;; valid options: "--opus" "--chat-gpt" "--sonnet-3.5" "--haiku"
(defvar *lacrosse-gen-patch-llms* '("--chat-gpt" "--sonnet-3.5" "--gemini"))
;; (defvar *lacrosse-gen-patch-llms* '( "--gemini"))
(defvar *lacrosse-gen-seeds-llms* '("--chat-gpt" "--sonnet-3.5" "--gemini"))

(defvar *libfuzzer-examples-default* 20
  "Default number of examples per fuzzer task to generate as seeds before fuzzing")

(defvar *use-revert-to-patch-p* t
  "When t, use the revert-to-patch task to gen patches.")

(defvar *revert-to-patch-limit-per-bic* 50
  "Max number of revert-to-patch to tasks to create per bic found.")

(defvar *reassign-dead-amp-tasks* T
  "If an AMP dies, should OPT try to reassign his task?")

;;; -------------------------------------------------------------------------
;;; *-amp.lisp

(defvar *phasename-order*)

(defvar *default-mission-planner-fn*)

;;; -------------------------------------------------------------------------

(defvar *upload-every-revision* nil "Should we upload all CB revisions we ever make?  Only useful for Scored Events")


;;; When you introduce a new global variable that should be displayed,
;;; add the variable name to the special declaration and the list of
;;; flags in the DOLIST.  
(defun print-flags (&optional (stream t))
  (declare (special
            *run-brute*
            *max-stars-to-wait*
            *send-idle-msg*
            *init-task-limit-per-fuzzbomb*
            *run-fake-bridge*
            *afl-qemu-mode*
            *use-afl*
            *use-libfuzzer*
            *no-afl-plusplus*
            *use-afl-blind-thread*
            *use-afl-via-driller*
            *use-driller*
            *use-canned-povs*
            *use-isabel*
            *use-gui*
            *driller-timeout*
            *driller-workers*
            *driller-force-interval*
            *driller-more-args*
            *lacrosse*
            *lacrosse-llm-stub-patch-p*
            *lacrosse-stub-pov-blob-p*
            *fake-fuzz*
            *fake-git-bisect*
                *lacrosse-gen-patch-llms*
                *lacrosse-gen-seeds-llms*
                *use-revert-to-patch-p*
                *revert-to-patch-limit-per-bic*
                *reassign-dead-amp-tasks*
            ))
  (dbug :top "printenv: ~s" (uiop:run-program "printenv" :output :string))

  (format stream "----------------------------------------------~%")
  (format stream "DOCKER_TAG: ~A~%" (uiop:getenv "DOCKER_TAG"))
  (format stream "(asdf:asdf-version): ~s~%" (asdf:asdf-version))
  (format stream "FUZZBOMB flags:~%")
  ;;(format stream "start time:  ~A~%~%" (tstamp nil))
  (dolist (flag '(*run-brute*
                  *max-stars-to-wait*
                  *send-idle-msg*
                  *init-task-limit-per-fuzzbomb*
                  *run-fake-bridge*
                  *upload-povs*
                  *use-afl*
                  *use-libfuzzer*
                  *no-afl-plusplus*
                  *use-afl-blind-thread*
                  *afl-qemu-mode*
                  *use-afl-via-driller*
                  *use-driller*
                  *use-canned-povs*
                  *use-isabel*
                  *use-gui*
                  *driller-timeout*
                  *driller-workers*
                  *driller-force-interval*
                  *driller-more-args*
                  *lacrosse*
                  *lacrosse-llm-stub-patch-p*
                  *lacrosse-stub-pov-blob-p*
                  *fake-fuzz*
                  *fake-git-bisect*
                *lacrosse-gen-patch-llms*
                *lacrosse-gen-seeds-llms*
                *use-revert-to-patch-p*
                *revert-to-patch-limit-per-bic*
                *reassign-dead-amp-tasks*
                  *features*
                  ))
    (format stream "   ~A      ~S~%" (string-downcase flag) (symbol-value flag)))
  (dbug :top "*debug-list* is ~s" *debug-list*)
  )

;;; -------------------------------------------------------------------------
;;; domain.lisp

(defvar *domain* nil
  "The string naming an AMP/CSM domain (prefix to -amp.lisp and -csm.lisp).
        [AMP]")

;;;-------------------------------------------------------------------------
;;; time.lisp

(defvar *secs-per-quantum* 1 "Seconds per deliberation quantum.  [AMP]")

(defvar *cur-quantum* nil "The current time quantum.  [AMP]")

(defvar *zero-time* nil "The internal-time-units zero reference.  [AMP]")

;;;-------------------------------------------------------------------------
;;; amp.lisp

(defvar *halt* nil "When non-nil, the infinite AMP loop stops.  [AMP]")

(defvar *use-timeouts* T
  "If non-nil (the default), use timeouts to limit CSM processing
time according to performance profiles.  Turn this off if you have
problems with a CSM run inside the AMP, to keep the break point available. [AMP]")

(defvar *sockets* nil)  ;; list of sockets we wait for input on...

(defvar *run-aid* nil "Should we run the AMP Information Display?  [AMP]")

(defvar *dot-heartbeat* 0 "Should we print dots for AMP heartbeat? If integer, include newline every 30 beats [AMP]")

(defvar *heartbeat-period* .75 "Time between idle heartbeats.  [AMP]")

;; CCL uses acl-compat which incorrectly treats process-wait-with-timeout timeout arg as in integer ticks, vs. seconds in orig ACL.
;; So since this is only used in mp-amp.lisp in a mp:wait-for-input-available call, which uses p-w-w-t, we need to do this:
#+ccl
(setf *heartbeat-period* (round (* *heartbeat-period* ccl:*ticks-per-second*)))

(defvar *send-idle-msg* t
  "When non-null, non-optimus fuzzbombs send a msg to optimus when they have no tasks ready to execute.")

(defvar *ask-to-start* nil "Should we wait for user to say go?  [AMP]")

(defvar *amp-paused* nil)       ;; T at start if *ask-to-start*, reset by :sync msg from master.

(defvar *perf-prof-fallback-function* nil
  "If non-null, should name a function that takes two args (goals threats)
   and returns estimate of the time required by the CSM to create a plan.
   This function will be called when there is not an entry in the
   *perf-prof-hash* for that combination of goals and threats.")

(defvar *greedy-discount-factor* .99)

(defvar *cur-phase* nil "The current phase object")

(define-restricted-var *negotiation-protocol*
    (:bid
     )
  "Valid values:
      * :bid    - only mode avail.")

(defvar *use-stop-amp* nil "Should the AMP use a STOP-AMP task? If set, the AMP creates a STOP-AMP task after calling the *DEFAULT-MISSION-PLANNER-FN*. [AMP]")

(defvar *upload-povs* nil "Should OPTIMUS upload PoVs to Leidos analytics console? [AMP]")

;;;----------------------------------------------------------------------
;;; skill class
(defvar *use-old-phase-level-skill-code* t
  "When non-nil, we use backward-compatibility mode for skills.")

;;;-------------------------------------------------------------------------
;;; aid-interface.lisp

;; eventually should handle trying to run AID on diff host; not for now...
;;(defvar *aid-hostname* (hostname))

;; NOTE eventually if we can get init-aid to happen after the
;; default-misison-planner, we may be able to define number of slots automaticly
(defvar *aid-command* "../aid/aid -s 10")
(defvar *aid-directory* "../aid/")

(defvar *aid-meters* nil "Should we display the AID quality meters?")

;;; this holds socket to AID; it should be one of those global
;;; specials w/ a dynamic binding in run-amp, to handle multiprocessing.
(defvar *aid-sock* nil)

(defvar *expect-aid-dont-runit* nil
  "Set to T if you want to run the AID by hand (esp on a diff machine)...
        AMP will prompt you to start it at appropriate time.  [AMP]")

;;;-------------------------------------------------------------------------
;;; amp-class.lisp

(defvar *amps* nil
  "List of all amp objects. [AMP]")

(defvar *master-amp* nil
  "bound to local obj rep the master. [AMP]")

(defvar *self* nil
  "pointer to this AMP's object... , ie me. [AMP]")

(defvar *num-amps* 0)

;;;-------------------------------------------------------------------------
;;; base.lisp

;;;  All set in init-globals now, to decouple build and run-time environments.
;;; [mboldt:20140929] In FB, these are defined in defs.lisp.
; (defvar *circa-baseport* nil
;   "Value used to compute socket ports used by this AMP and its collaborators.
;    Depends on environment variable CIRCA\\_BASEPORT [CIRCA]")
; (defvar *circa-basename* nil
;   "Value used to compute matchmaker names used by this AMP and its collaborators.
;    Depends on environment variable CIRCA\\_BASENAME [CIRCA]")
(defvar *matchmaker-host* nil
  "Name of a host for the matchmaker instance this AMP will connect to.
   Set to value of the environment variable CIRCA\\_MM\\_HOST or localhost
   if the environment variable isnt set. [CIRCA]")
(defvar *matchmaker-port* nil
  "Port used for connecting to a matchmaker. [CIRCA]")
(defvar *amp-port* nil
  "Port used for connecting to this amp [CIRCA]")
(defvar *bridge-port* nil
  "Port used for connecting to the bridge [CIRCA]")

;;;-------------------------------------------------------------------------
;;; collab.lisp

(defvar *mm-sock* nil)

;;;-------------------------------------------------------------------------
;;; gantt.lisp

(defvar *contract-statuses* nil
  "assoc list of cur state of each contract for agent")

(defvar *gantt* nil
  "assoc list of contracts w/ pairs of start/end coverage times")

;;;-------------------------------------------------------------------------
;;; mission.lisp

;;; Number to decrement by 2 for each new tap index.
;;; The base index is always odd.
(defvar *rts-current-index-number* -1
  "Negated odd index of next new rts index.")

;;;-------------------------------------------------------------------------
;;; perf-prof.lisp

(defvar *perf-prof-hash* nil)

;;; Initialized in init-globals based on run-time environment.
;;;(defvar *default-perf-prof-file* "circa:amp;default-perf-prof.lisp")
(defvar *default-perf-prof-file* nil)

(defvar *perf-prof-suffix* "perf-prof.lisp")

(defvar *perf-prof-prob* nil)

;;;-------------------------------------------------------------------------
;;; rts-interface.lisp

(defvar *reboot-in-progress* nil
  "T if the AMP is currently rebooting. This process starts with a :reboot msg and initializes
an RTS handoff process. The entire process ends when the old RTS acknowledges the handoff and then
sends up a :rts-handoff msg to the AMP. NOTE: (or *generate-handoff-taps* *rts-handles-handoff-requests*)
and *run-rts* must be T, and the RTS should be started via run-rts-locally.")

#|

(define-restricted-var *rts-run-mode*
    (:auto-start :manual-start :plexil-manual-start :plexil-auto-start)
  "Determines how RTS is executed. Valid values:
        * :auto-start   - forks RTS in separate process, on this machine.
        * :manual-start - prompts user to run RTS, waits for it to connect.
        * :plexil-auto-start - forks RTS in separate process, sends schedules in Plexilisp.
        * :plexil-manual-start - prompts user to run RTS, waits, sends schedules in Plexilisp.")

;; eventually should handle trying to run RTS on diff host; not for now...
;;(defvar *rts-hostname* (hostname))

;; uses old config, that sets standalone=1
;;(defvar *rts-command* "../rts/rts -nocomm -config ../rts/solo-config -mm foom -acm foom")

;; this generic version doesnt specify config filename, leaves
;; that to be auto-generated and appended at calling time, below
(defvar *rts-command* "../rts/rts -nocomm -schedpair_mode -config ")

;; AMP RTS handoff globals
(defvar *rts-port-offset* 7)
(defvar *rts-handoff-offset* 5)
(defvar *rts-handoff-sock* nil)

(defvar *always-use-same-rts-sched-index-for-phase* nil
  "If nil (default), AMP tries to pick the not-currently-running schedpair index to download an improved
        plan to the RTS. If T, AMP will just use the same schedule index for a phase over and over,
        and rely on the new-as-of-Nov2012 RTS ability to detect this and put the schedule into the
        other schedpair slot.  [AMP]")

;; for debugging RTS...
;;(defvar *rts-command* "../rts/rts -d -nocomm -config ")

;;(defvar *rts-command* "../rts/rts -nocomm -config ../rts/solo-config -mm foom -acm foom")


(defvar *rts-domain-options* nil
  "A domain-specific string of options added to the RTS function call.")

;;; if we're going to use this to hold socket to RTS, it
;;; should be one of those global specials w/ a dynamic binding
;;; in run-amp.
(defvar *rts-sock* nil)

(defvar *redirect-rts-output-to-file* t
  "If t, redirect the rts output to a log file in the /tmp directory.")

(defvar *handle-state-requests* t
  "Should the RTS respond automatically to SEND-STATE requests from AMP or
        should it require AMP to include a special TAP to do so?  [AMP]")

(defvar *handle-handoff-requests* t
  "Should the RTS respond automatically to HANDOFF requests from another RTS?  [AMP]")

(defvar *RTS-strings-for-goals* nil
  "This is the semi-hack that allows AMP to download custom strings
to RTS when it is awarded goals; this can be used to give RTS target
specification information when an agent wins a target contract.  See
5plane-oep-amp.lisp for example.")

|#
;;;-------------------------------------------------------------------------
;;; contract-class.lisp

(defvar *contract-number* 0 "Index of next new contract generated locally")

;;;-------------------------------------------------------------------------
;;; task-class.lisp

(defvar *phase-number* 0 "Index of next new phase generated locally")

;;;-------------------------------------------------------------------------
;;; gui-interface.lisp

(defvar *run-gui* nil)
(defvar *gui-sock* nil)

;;;-------------------------------------------------------------------------
;;; fuzzball-wrapper.lisp
(defvar *fuzzball-seed* (cl-variates:make-random-number-generator 42))
