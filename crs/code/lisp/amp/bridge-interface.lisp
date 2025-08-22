;;; -------------------------------------------------------------------------
;;; $Id: bridge-interface.lisp 7525 2013-07-01 21:10:35Z cpotts $
;;; -------------------------------------------------------------------------
;;; COPYRIGHT START
;;; COPYRIGHT END
;;; -------------------------------------------------------------------------

(cl:in-package :fuzzbomb)

(defvar *connect-to-bridge* nil "Should AMP connect to the HAMMER Aspire bridge or fake-bridge? [AMP]")

(defvar *run-fake-bridge* nil "Should AMP start a fake incoming bridge for testing? [AMP]")

(defvar *bridge-host* "localhost" "The host computer running the bridge.  Defaults to localhost.  If environment variable BRIDGE_HOST is set, *bridge-host* will be set to its value. [AMP]")

;;; -------------------------------------------------------------------------

(defun init-my-bridge (self)
  
  (when *reboot-in-progress*
    (dbug :top "Already connected to bridge")
    (return-from init-my-bridge))
  
  (when (getenv "BRIDGE_HOST")
    (setf *bridge-host* (getenv "BRIDGE_HOST")))

  (when (getenv "BRIDGE_PORT")
    (setf *bridge-port* (getenv "BRIDGE_PORT"))
    (setf (bridge-port self) (getenv "BRIDGE_PORT")))
  
  (when *run-fake-bridge* 
	(dbug :top "Running fake bridge")
	(run-fake-bridge-in-separate-process (bridge-port self))
	(sleep 1))

  (dbug :top "   Connecting to bridge at port ~A" (bridge-port self))
  (setf (bridge-socket self) (reliably-make-socket :remote-host *bridge-host* :remote-port (bridge-port self)))
  (push (bridge-socket self) *sockets*)
  (dbug :top "	Got connection to bridge at local port ~A" (local-port (bridge-socket self)))
)
;;; -------------------------------------------------------------------------

