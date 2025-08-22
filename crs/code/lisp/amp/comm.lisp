;;; -------------------------------------------------------------------------
;;; comm.lisp
;;; - socket based communication b/w AMPs.
;;; - $Revision: 1.33 $
;;; -------------------------------------------------------------------------
;;; COPYRIGHT START
;;; COPYRIGHT END
;;; -------------------------------------------------------------------------
;;; -------------------------------------------------------------------------
;;; generic design: agent has a *message-queue* that it pulls messages
;;; out of and processes.  Various parallel processes can be used to
;;; listen/talk on different sockets, and they all get serialized onto
;;; that message queue.  Critical section code protects mutex on queue
;;; and other global data (e.g., the list of connected agents).

;;; Alternatively : agent loop polls over sockets, processes one message
;;; per socket and doing other stuff until none left...

;;; The master-AMP opens a well-known socket to get connections.
;;; Other agents connect to that socket and establish conversation with
;;; master-AMP.

;;; The master knows it is the master because we tell it so, either in
;;; an argument to the lisp process as it is started
;;; (e.g., lisp -- master)
;;; or by responding to the human-interface prompt that is triggered if
;;; no command line is specified.

;;; The master keeps sockets to connect to each slave in the slave's
;;; socket slot.  Each slave only has one socket, to talk to master, and
;;; that it stores in the master's socket slot.

;;; When a slave first connects to master, it sends it's name.  Master uses
;;; this to find the appropriate amp object and store socket to that slave.

;;; Note the use of global for *self* is incompatible with
;;; a scheme in which the Allegro multiprocessing is used to run multiple
;;; AMPs, since the globals are shared amongst all those Lisp threads.
;;; To get around this, we'd have to pass around a self variable locally
;;; to all functions.  Or, alternatively, we just form dynamic binding
;;; scopes for the specials (globals) and keep all of the agent computations
;;; after the "self" is determined inside the dynamic scope.  OK, let's do
;;; that!
;;; - Note for now the multiprocessing stuff is not built, but we'll
;;; design/build so will be easy retrofit, using dynamic specials.

;;; -------------------------------------------------------------------------

(cl:in-package :fuzzbomb)

;;;-------------------------------------------------------------------------
(debug-list-value :msg "To trace all messages sent via comm.lisp code.")

;;(pushnew :msg *debug-list*)

;;;-------------------------------------------------------------------------
(defun reliably-make-socket (&rest keyword-pairs &key (retries 20) &allow-other-keys &aux sock)
  (dotimes (i retries)
    #+allegro (setf sock (dont-error (apply #'make-socket :allow-other-keys t keyword-pairs)))
    #+ccl (setf sock (dont-error (apply #'ccl:make-socket :allow-other-keys t keyword-pairs)))
    (when sock (return-from reliably-make-socket sock))
    (dbug :top "reliably-make-socket: make-socket failed, sleeping before trying again")
    (sleep 1))
  nil)
    
;(defun disable-chunking (sock)
;  #+allegro 
;  #+ccl (declare (ignore sock))	;; you dont need to turn off chunking in CCL unless you accidentally get acl-compat streams, and then it doesnt work.  Boo
;)

;;#+ccl (import (list 'ccl:local-port 'ccl:local-host 'ccl:remote-port 'ccl:remote-host 'ccl:ipaddr-to-hostname))

;; FIXME these should be imports
#+ccl (defun local-port (s) (ccl::local-port s))
(memoize 'local-port)	;; this was consing a lot, fwr
#+ccl (defun local-host (s) (ccl::local-host s))
#+ccl (defun remote-port (s) (ccl::remote-port s))
#+ccl (defun remote-host (s) (ccl::remote-host s))
#+ccl (defun ipaddr-to-hostname (s) (ccl::ipaddr-to-hostname s))

(defun my-accept-connection (sock)
  #+allegro (let ((s (accept-connection sock)))
  		  (socket-control s :output-chunking nil :input-chunking nil)
		 s)
	;; FIXME could we just import this?
  #+ccl (ccl:accept-connection sock)
)

;;;-------------------------------------------------------------------------
;;; - sets up comms for AMP agent: open well-known socket,
;;; register w/ MM, and connect to all collaborators.
;;; - this works like the RTS's initialize_collaborations in collab.c
;;; - returns the amp object that is self.

(defmethod init-comm ((amp amp))
  (declare (special *connect-to-bridge*))
  (when (not (init-comm-p amp))
    (dbug :top "Initializing communications for ~A" *self*)
    (when (matchmaker-p amp) (initialize-collaborations))
    (when *connect-to-bridge* (init-my-bridge amp))
    (setf (init-comm-p amp) t)))

;;;(defun deinit-comm ()
;;;  (when *self*
;;;    (dolist (a (remove *self* *amps*))
;;;      (send-msg (shortname a)
;;;                (list '(:type :dying) (list :who-died (name *self*)))))
;;;    (sleep 1)
;;;    (dolist (s (peer-sockets *self*))
;;;      (close (second s)))
;;;    (when (amp-sock *self*) (close (amp-sock *self*)))
;;;
;;;    ;;(setf *self* nil)
;;;    ))

(defmethod deinit-comm ((amp amp) halt-status)
  (declare (special *connect-to-bridge* *reboot-in-progress* *fake-bridge-process*))
  (dbug :comm "deinit-comm")
  (dbug :comm "(amp-sock *self*): ~s" (amp-sock *self*))
  (unless *reboot-in-progress*
    (when *fake-bridge-process*
      (dbug :comm "    Killing fake bridge process")
      (mp:process-kill *fake-bridge-process*)
      (setf *fake-bridge-process* nil))
    (when (bridge-socket *self*)
      (setf *sockets* (delete (bridge-socket *self*) *sockets*))
      (dbug :comm "closing bridge-socket")
      (close (bridge-socket *self*))
      (setf (bridge-socket *self*) nil))
    )
  (when (init-comm-p amp)
    (dolist (a (remove *self* *amps*))
      (send-msg (shortname a)
                (list '(:type :dying) (list :who-died (name *self*)) (list :halt-status halt-status))))
    (sleep 1)
    (dbug :comm "closing peer-sockets")
    (dolist (s (peer-sockets *self*))
      (close (second s))
      (setf *sockets* (delete (second s) *sockets*)))
    (when (amp-sock *self*) 
      (dbug :comm "closing (amp-sock *self*)")
      (close (amp-sock *self*)))
    (setf (init-comm-p amp) nil)))

;;;-------------------------------------------------------------------------

(defun send-string (to msg &optional (newline T))
  (let ((sock (socket-to to)))
    (cond (sock
           (write-string msg sock)
           (if newline 
               (terpri sock)
               )
           (force-output sock)
	   T
           )
          (T
           (format t "ERROR: trying to send ~A to ~A with no socket~%" msg to)
	   (process-eof-msg '(:type :eof) sock)
	   nil))
    ))

;;;-------------------------------------------------------------------------
;;; this sends string that is a quoted assoc list
;;; - ex: (send-msg "master" '((:from foo)(:to boo)(:type :bid)(:value 200)))

(defvar *timestamp-messages* nil)

(defvar *msg-counter* 0)

(defun send-msg (to msg)
  ;;(dbug :msg "send-msg: ~s ~s" to msg)
;;  ;; sanity check that to arg matches :to field value, if any.
;;  (let ((msg-to (getassoc :to msg)))
;;    (when (and msg-to
;;               (not (eq to msg-to)))
;;      (error "send-msg: to arg to send-msg (~s) doesn't match the :to field value of the msg (~s)" to msg-to)))

  (setassoc :from (shortname *self*) msg)
  (dbug :msg "Sending msg ~S to ~A" msg to)
  (incf *msg-counter*)
  (let* ((msgID (format nil "~d~d" (port *self*) *msg-counter*))   ;; Unique message ID
         (msgName (format nil "~s-~d" (getassoc :type msg) msgID)))  ;; Unique message name
    ;; Add unique message identifier and name to message assoc list
    (setassoc :uid msgID msg)
    (setassoc :msgname msgName msg)
    ;; Log out in pragmatracer format
    (tracer-msg-sent (port *self*) msgID msgName (getassoc :type msg) (shortname *self*)))
  (let ((*print-pretty* nil))     ;; turn off PP to avoid line breaks.
    (dont-error (send-string to (format nil "~S" msg))))
)

(defun get-msg (sock)
  (let ((message (read-from-string (get-string sock))))
    (tracer-msg-recv (port *self*) (getassoc :uid message) (getassoc :msgname message) (getassoc :type message) (shortname *self*))
    (dont-error message))
  ;;(dont-error (read sock nil (eof-msg sock)))
)

;;;-------------------------------------------------------------------------
;;; Sends a message about a contract, where rest-args is in
;;; keyword value form just like normal function keyword args.  However,
;;; these are compiled into an assoc list form and passed as a string.
;;; - ex: (send-message "master" :announcement (id contract)
;;;                     :status (status contract))

(defun send-message (&rest msg)
  "Msg must include a :to field."
  (dbug :comm "send-message: ~s" msg)
  (let* ((msg-pairs (pairup msg))
         (to (getassoc :to msg-pairs)))
    (if *timestamp-messages* 
	(setf msg-pairs (timestamp-message msg-pairs)))
   (if to
      (send-msg to msg-pairs)
      (dbug :top "Error: send-message: Msg (~s) doesn't have a :to field; ignoring." msg-pairs))
))

;;;-------------------------------------------------------------------------
;;; Broadcast 


(defun broadcast-message (&rest msg)
  "Sends msg to all agents except one that sends it."
  (dbug :msg "broadcast-message: ~s" msg)
  (let ((msg-pairs (pairup msg)))
    (if *timestamp-messages* 
	(setf msg-pairs (timestamp-message msg-pairs)))
    (dolist (amp *amps*)
      (unless (eq *self* amp)
        (send-msg (name amp) msg-pairs)))))

(defun send-message-incl-optimi (&rest msg)
  "Sends msg to one agent (in the :to field) plus all the Optimi (except for self, if being called by an Optimus)."
  (dbug :msg "send-message-incl-optimi: ~s" msg)
  (let* ((msg-pairs (pairup msg))
         (to (getassoc :to msg-pairs)))
  (if *timestamp-messages* 
	(setf msg-pairs (timestamp-message msg-pairs)))
  (if to
      (send-msg to msg-pairs)
      (dbug :top "Error: send-message-incl-optimi: Msg (~s) doesn't have a :to field; ignoring." msg-pairs))
    (dolist (ampname *optimi*)		;; these are actually names
      (unless (string= (shortname *self*) ampname)
        (send-msg ampname msg-pairs)))))

(defun send-message-to-optimi (&rest msg)
  "Sends msg to all the Optimi (except for self, if being called by an Optimus)."
  (dbug :msg "send-message-to-optimi: ~s" msg)
  (let ((msg-pairs (pairup msg)))
    (if *timestamp-messages* 
	(setf msg-pairs (timestamp-message msg-pairs)))
    (dolist (ampname *optimi*)		;; these are actually names
      (unless (string= (shortname *self*) ampname)
        (send-msg ampname msg-pairs)))))
  
(defun timestamp-message (msg-pairs)
  "Adds a timestamp to msg.  Assumes msg is in paired format.  Please use the return value!."
  (setassoc :timestamp (get-internal-real-time) msg-pairs))

(defun pairup (plist)
  "Given plist (1 a 2 b...) group the adjacent items into pairs ((1 a) (2 b) ... )."
  ;; This could be re-implemented to traverse the list just once.
  (let ((result nil))
    (musliner:for (index 0 (length plist) 2)
         (push (list (nth index plist) (nth (1+ index) plist)) result))
    result))

;;; 6/7 -- note #2, tasks from clients, are not done via messages anymore...
;;; sim.lisp puts them directly into sched/*contracts*, so not needed.

                                        ;(defun message-priority (msg)
                                        ;  (+ (* (message-type-priority msg) 100000000000000)
                                        ;     (getassoc :timestamp msg)))
                                        ;
                                        ;(defun message-type-priority (msg)
                                        ;  (case (getassoc :type msg)
                                        ;        ((:sim-clock-update :sim-cycle-complete) 0)
                                        ;        (:report 1)
;;;      (:task 2)       ;; not used: sim puts contracts directly into sched.
                                        ;        (:award 3)
                                        ;        (:bid 4)
                                        ;        (:announcement 5)
                                        ;        ((otherwise) 6)))


;;;-------------------------------------------------------------------------
;;; returns socket to talk to the arg amp agent.

(defmethod socket-to ((ampname string))
  (getassoc (find-amp ampname) (peer-sockets *self*)))

(defmethod socket-to ((ampobj amp))
  (getassoc ampobj (peer-sockets *self*)))

#+ccl
(defmethod socket-to ((ampsock ccl::basic-tcp-stream))
  ampsock)

;;;-------------------------------------------------------------------------
;;; returns msg as a string; when get to EOF, returns a bogus
;;; msg of form ((:type :eof)) so that regular msg parser can eat it...

(defun eof-msg (sock)
  (format nil "((:type :eof) (:sockport ~A))" (local-port sock)))

(memoize 'eof-msg)

(defun get-string (sock)
  ;;(read-line sock nil (format nil "((:type :eof :sockport ~A))" (local-port sock)))
  ;;(read-line sock nil (strcat "((:type :eof) (:sockport " (write-to-string (local-port sock)) "))" ))
  (read-line sock nil (eof-msg sock))
)

;;;-------------------------------------------------------------------------
;;; Returns list of agent names other than *self*

                                        ;(defun agents-other-than-me ()
                                        ;  (remove *self* (mapcar #'first *nameservice*) :test #'string=))

;;;-------------------------------------------------------------------------
;;; - returns AMP object.
;;; - tries to find the AMP based on the name. If not found, it there
;;;   is only one AMP, use that. Otherwise, ask the user to choose.
;;; - note currently assumes names are stored uppercase.

(defun which-agent-am-i (&optional (name nil))
  (cond ((find-amp name))
        ;; if only one amp, we are it!
        ((= 1 (length *amps*))
         (dbug :amp "There is only one CIRCA agent in this domain; I'm it!")
         (first *amps*))
        (T (user-says-which-agent-I-am))))

;;;-------------------------------------------------------------------------
(defun user-says-which-agent-I-am (&aux index (agent nil))
  (mp:without-scheduling
    (musliner:while (not agent)
      (format t "Select which agent I am:~%")
      (musliner:for (i 0 (length *amps*) 1)
           (format t "     ~A : ~A~%" i
                   (name (nth i *amps*))))
      (format t "Enter index: ")
      (setf index (read-from-string (read-line)))
      (cond ((and (numberp index)
                  (< index (length *amps*)))
             (setf agent (nth index *amps*)))
            (T (format t "Invalid index, try again~%")))))
  agent)

;;;-------------------------------------------------------------------------
