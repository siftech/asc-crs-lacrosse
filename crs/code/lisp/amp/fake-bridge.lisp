;;; -------------------------------------------------------------------------
;;; $Id:$
;;; -------------------------------------------------------------------------
;;; COPYRIGHT START
;;; COPYRIGHT END
;;; -------------------------------------------------------------------------
;;; The "acceptor" is an add-on to the fake-bridge functionality.  After the fake-bridge
;;; opens a socket it waits for the AMP to connect and then launches a thread on that
;;; connection to replay the *fake-bridge-script* contents over time.
;;; Now, if *run-acceptor*, it also launches the acceptor thread on the original "listen" socket and
;;; waits for other connections.   The ../rts/tell-amp program is designed
;;; to connect to that listen socket and send in messages that it gets on its command line.
;;; When the acceptor gets a connection it reads incoming lines and sends them immediately 
;;; on to the AMP via the original fake-bridge's connected socket.  So yes, there is a potential
;;; race b/c f-b and acceptor thread (currently managed via scheduler lockout) on writing to that socket.
;;; There are races on closing sockets that could also be protected.

;;; ../rts/tell-amp -k 
;;; is the same as
;;; ../rts/tell-amp "((:type :halt))"
;;; -------------------------------------------------------------------------

(cl:in-package :fuzzbomb)

(defvar *run-acceptor* nil "Should AMP start a process that accepts connections for command-line inputs to testing? [AMP]")

(defvar *fake-bridge-process* nil)
(defvar *acceptor-process* nil)

(defvar *fake-bridge-script* '((12000 ((:type :halt))))
  "A list of lists, each consisting of a number of seconds to wait and a CIRCA-AMP-readable msg to send to the AMP.  This
   may morph the fake-bridge into a more general testing tool for CIRCA in general, and especially the AMP. [AMP]")

(defun run-fake-bridge (&optional (port 2500))
  (let ((sock (reliably-make-socket :connect :passive :reuse-address t :local-port port)) 
 	(*program-name* "FAKE-BRIDGE") ;; dynamic scope of this name used by dbug
	ampsock msg)
    (setf (bridge-port *self*) (local-port sock))
    (unwind-protect 
	(progn
	  (dbug :top "   Waiting for connection from AMP on port ~d..." (local-port sock))
	  #+allegro(mp:wait-for-input-available (list sock))
	  (setf ampsock (my-accept-connection sock))
	  (dbug :top "	Got connection from AMP at local port ~A" (local-port sock))

	  (when *run-acceptor* (run-acceptor-in-separate-process sock ampsock))

	  ;; eventually this may offer some choices of what to send... or may
	  ;; have a sequence of stuff to send with sleeps between...

	  (dolist (delay-msg *fake-bridge-script*)
	    (assert (numberp (first delay-msg)))
	    (sleep (first delay-msg))
	    (setf msg (second delay-msg))
	    (assert (listp msg))
	    (dbug :top "	Sending msg ~S to AMP" msg)
	    (send-msg ampsock msg))
            ;(send-msg ampsock (format nil "~S" msg))

	  (sleep 5))
      
      #+allegro
      (when (open-stream-p ampsock)
      	(dbug :msg "	Closing fake bridge socket to AMP ~A" ampsock)
      	(close ampsock))
      #+allegro
      (when (open-stream-p sock)
      	(dbug :msg "	Closing fake bridge listen socket ~A" sock)
      	(close sock))
      )
))

(defun run-fake-bridge-in-separate-process (&optional (port 2500))
  (when *fake-bridge-process*
	(dbug :top "Warning: fake bridge already running... killing it")
	#+allegro (mp:process-kill *fake-bridge-process* :wait T)
	#+ccl (mp:process-kill *fake-bridge-process*)
   	#-(or ccl allegro) (error "no mp:process-kill defined for use here")
	)
  (dbug :top "Running the fake-bridge in separate process.")
  (setf *fake-bridge-process* (mp:process-run-function '(:name "fake-bridge") #'run-fake-bridge port)))

(defun run-acceptor-in-separate-process (listensock ampsock)
  (when *acceptor-process*
	(dbug :top "Warning: acceptor already running... killing it")
	#+allegro (mp:process-kill *acceptor-process* :wait T)
	#+ccl (mp:process-kill *acceptor-process*)
   	#-(or ccl allegro) (error "no mp:process-kill defined for use here")
	)
  (dbug :top "Running the acceptor in separate process.")
  (setf *acceptor-process* (mp:process-run-function '(:name "acceptor") #'run-acceptor listensock ampsock)))


(defun run-acceptor (listensock ampsock)
  (let (newsock
 	(*program-name* "ACCEPTOR") ;; dynamic scope of this name used by dbug
	)
		;; register the acceptor socket with MM
    (register-with-matchmaker (strcat (name *self*) "-ACCEPTOR") (host *self*) (bridge-port *self*))
    (disconnect-from-matchmaker)
    (unwind-protect 
	(musliner:while #+allegro (open-stream-p listensock) #-allegro T
	  (dbug :top "   Waiting for connection...")
	  #+allegro (mp:wait-for-input-available (list listensock))
	  (setf newsock (my-accept-connection listensock))
	  (dbug :top "	Acceptor got connection at local port ~A, newsock is at port ~A" (local-port listensock) (local-port newsock))
  	  (mp:process-run-function "acceptor-newsock-handler" #'handle-acceptor-connection newsock ampsock)
	 )
      #+allegro
       (when (open-stream-p listensock)
      	 (dbug :msg "	Closing acceptor listen socket ~A" listensock)
      	 (close listensock)))
))

;;; -------------------------------------------------------------------------

(defun handle-acceptor-connection (newsock ampsock &aux str halt)
  (musliner:while (not halt) 
	  	(mp:wait-for-input-available (list newsock))
		(setf str (read-line newsock nil :eof))
		(dbug :amp-deep "Acceptor port ~A got and will forward ~S~%" (local-port newsock) str)
		(if (eq str :eof)
			(setf halt T)
			(mp:without-scheduling (write-string str ampsock) (terpri ampsock) (force-output ampsock))))
 (when (open-stream-p newsock)
       (dbug :msg "Closing incoming socket ~A" newsock)
       (close newsock))
)
