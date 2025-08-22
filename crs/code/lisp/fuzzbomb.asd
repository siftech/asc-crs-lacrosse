;;; -------------------------------------------------------------------------
;;; $Id: fuzzbuster.asd 3464 2014-05-03 21:01:23Z sfriedman $
;;; -------------------------------------------------------------------------
;;; COPYRIGHT START
;;; Copyright (c) 2012, Smart Information Flow Technologies (SIFT).  Developed with the sponsorship of the Defense Advanced Research Projects Agency (DARPA) and Air Force Research Laboratory.
;;; COPYRIGHT END
;;; -------------------------------------------------------------------------

(defpackage :fuzzbomb-asd
  (:use :asdf :common-lisp)
  )
(in-package :fuzzbomb-asd)

(defsystem :fuzzbomb
    :serial t
    :depends-on (
                 :musliner-tools
                 :bordeaux-threads  ; used for platform independent wrappers around all-threads, current-thread and thread-name
                 :cl-variates
                 :xmls
                 :iterate
                 :split-sequence
                 :cl-json
                 #+ccl acl-compat
                 )
    :components (
                 ;; This comes first, cause everyone needs to have the
                 ;; package definition.
                 (:file "package")

                 #+ccl (:module "excl"
                          :pathname "."
                          :components
                          (
                           (:file "excl-package")
                                ;; FIXME for sbcl should try to make excl-sbcl and extend #+ above
                           #+ccl (:file "excl-ccl")
                           ))

                 #+ccl
                 (:module "mp"
                          :pathname "."
                          :components
                          (
                           (:file "mp-package")
                           ))

                 (:file "turn-on-warning-failures")     ;; after this, there shall be no dragons!

                 ;; Utilities
                 (:file "json-schema-to-clos/json-schema-to-clos")
                 (:file "os-utils")
                 (:file "sieve")
                 (:file "print-readably")

                 ;; project definitions
                 (:file "defs")

                 ;; AMP
                 (:file "amp/persistence-package")
                 (:file "amp/persistence")
                 (:file "amp/persistence-fuzzbomb")
                 (:file "amp/instance-collector")
                 (:file "amp/class-p-predicates")
                 (:file "amp/utils")     ;; generic utils used by amp code
                 (:file "amp/globals")   ;; global vars for AMP
                 (:file "amp/circa-stub")    ;; [tzim 4/7/12] load assorted stubbed fcns & vars --minimizes loading
                 (:file "amp/base")
                 (:file "amp/time")      ;; CIRCA AMP's perception of time (as delib. quanta)
                 (:file "amp/amp-class")
                 (:file "amp/skill-class")
                 (:file "amp/mission")   ;; CIRCA mission, mission-phases
                 (:file "amp/perf-prof")   ;; NOTE: this is a FB-specific (modified) vers of the CIRCA/amp.
                 (:file "amp/config-class")
                 (:file "amp/contract-class")  ;; CIRCA contracts -not actively used in FB
                 (:file "amp/task-class")  ;;
                 (:file "amp/target-class"
                  #+allegro #+allegro
                  :around-compile (lambda (thunk)
                                    (let ((excl:*enable-package-locked-errors* nil))
                                      (funcall thunk)))
                  #+allegro #+allegro
                  :perform (load-op :around (op c)
                                    (declare (ignorable op c))
                                    (let ((excl:*enable-package-locked-errors* nil))
                                      (call-next-method))))
                 (:file "amp/vuln-cand-class")
		 (:file "amp/patch-cand-class")
                 (:file "amp/delib-task")  ;;

                 (:file "amp/domains")  ;;
                 (:file "amp/comm")  ;;
                 (:file "amp/collab")  ;;
                 (:file "amp/amp")
                 (:file "amp/bridge-interface")
                 (:file "amp/fake-bridge")
                 (:file "amp/mp-amp")
                 (:file "amp-support")

                 ;; Infrastructure
                 (:file "mr")
                 (:file "input-channel")

                 ;; for ISABEL
                 (:file "amp/isabel")
                 (:file "bugs-framework")

                 (:file "amp/target-trigger")  ;;
                 ;;(:file "amp/binwalk-task")  ;;
                 (:file "amp/docker-task")  ;;
                 ;;(:file "amp/tasks/driller-task")  ;;
                 (:file "amp/tasks/afl-task")  ;;
                 ;;(:file "amp/tasks/pereti-step-task")  ;;
                 ;;(:file "amp/tasks/pereti-unpack-task")  ;;
                 ;;(:file "amp/tasks/pereti-disasm-task")  ;;
                 ;;(:file "amp/tasks/pereti-cfg-task")  ;;
                 ;;(:file "amp/tasks/pereti-jump-task")  ;;
                 ;;(:file "amp/tasks/pereti-warnings-task")  ;;
                 ;;(:file "amp/whatclib-task")  ;;
                 ;;(:file "amp/fuzzball-task")  ;;
                 ;;(:file "amp/tasks/heapbuster-task")  ;;
                 ;;(:file "amp/tasks/demo-exploit-task") ;;
                 ;;(:file "amp/tasks/afl-hb-task") ;;

                 ;;(:file "amp/tasks/chisel-task")  ;; for MADEIRA
                 ;;(:file "amp/tasks/confine-task")  ;; for MADEIRA
                 ;;(:file "amp/tasks/jdebloat-task")  ;; for MADEIRA

                 (:file "amp/tasks/lacrosse-task")
                 (:file "amp/tasks/lacrosse-llm-task")

                 ;;(:file "amp/tasks/fake-fuzz-task")
                 ;;(:file "amp/tasks/lacrosse-cp-init-task")
                 ;;(:file "amp/tasks/lacrosse-llm-confirm-vuln-task")
                 (:file "amp/tasks/lacrosse-llm-gen-patch-task")
                 ;;(:file "amp/tasks/lacrosse-gen-pov-blob-task")
                 ;;(:file "amp/tasks/lacrosse-build-task")
                 ;;(:file "amp/tasks/lacrosse-cp-build-task")
                 ;;(:file "amp/tasks/lacrosse-cp-run-test-task")
                 ;;(:file "amp/tasks/lacrosse-cp-build-patch-task")
                 (:file "amp/tasks/lacrosse-cp-run-pov-task")
                 ;;(:file "amp/tasks/lacrosse-test-patch-task")
                 (:file "amp/tasks/lacrosse-git-bisect-task")
                 (:file "amp/tasks/libfuzzer-task")
                 (:file "amp/tasks/jazzer-task")
		 (:file "amp/tasks/revert-to-patch-task")
                 )

    ;; This gets executed after the system is loaded.
    :perform (load-op :after (op c)
                        ;; Load local.lisp in the system directory, if
                        ;; such a file exists.
                        (let ((local-lisp-pathname (system-relative-pathname :fuzzbomb #P"local.lisp")))
                          (cond ((probe-file local-lisp-pathname)
                                 (format t "Loading local.lisp.~%")
                                 (load local-lisp-pathname))
                                (t
                                 (format t "local.lisp does not exist, not loading it.~%"))
                                ))
                        )
    )
