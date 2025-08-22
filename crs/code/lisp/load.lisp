;;; -------------------------------------------------------------------------
;;; $Id: load.lisp 2011 2014-05-08 18:53:57Z sfriedman $
;;; -------------------------------------------------------------------------
;;; COPYRIGHT START
;;; Copyright (c) 2012, Smart Information Flow Technologies (SIFT).  Developed with the sponsorship of the Defense Advanced Research Projects Agency (DARPA) and Air Force Research Laboratory.
;;; COPYRIGHT END
;;; -------------------------------------------------------------------------
(in-package :common-lisp-user)
(require :asdf)

(format T "Loading NEO-Fuzz~%")

(setf asdf:*compile-file-failure-behaviour* :error)
;;(setf asdf:*compile-file-warnings-behaviour* :error)  ;; can't enable this yet, too many issues w/ external pkgs

;;; Memory config stolen from CIRCA CSM
;;; The quantum parameter controls the step size (and thus minimum size) for
;;; a new/old space allocation. The number is the number of 8KB pages allocated.
#+allegro
(setf (sys:gsgc-parameter :quantum) 3200) ; new/old space increase in multiples of 25MB
#+allegro
(setf (sys:gsgc-parameter :generation-spread) 8)
;;; Allocate 300MB bytes for both new and old space.
;;#+allegro
;;(sys:resize-areas :new (* 300 1024 1024) :old (* 300 1024 1024))

#+allegro
(setq excl::*warn-smp-usage* nil)       ;; Note we only run this in non-SMP lisps, so stop whining about without-scheduling macro

(defvar *fb-code-directory* (make-pathname :directory (pathname-directory *load-truename*)))

(format t "*fb-code-directory* is ~A~%" *fb-code-directory*)

(defvar *fb-instance* nil "Name of this Fuzzbomb instance")

(defvar *compile-for-debug* nil)

;;; put fasls in platform specific subdirectory, under the cur dir
#-asdf3
(asdf:enable-asdf-binary-locations-compatibility)

#+asdf3
(asdf:initialize-output-translations `(:output-translations
                                       (t (:root :**/ :implementation :*.*.*))
                                       :ignore-inherited-configuration))

(defvar *ext-directory* (merge-pathnames (make-pathname :directory '(:relative "ext")) *fb-code-directory*))
(push *ext-directory* asdf:*central-registry*)

(defvar *asd-finder-directory* (merge-pathnames (make-pathname :directory '(:relative "asd-finder")) *ext-directory*))
(push *asd-finder-directory* asdf:*central-registry*)
(format t "*asd-finder-directory* is ~A~%" *asd-finder-directory*)

(asdf:load-system "asd-finder")

;; Now add any packages in this directory (or subdir) to the central
;; registry so that dependencies are found without drama.
(setf asdf:*central-registry*
      (union (asd-finder:asd-finder (list *fb-code-directory*)
                                :prune-names (list ".svn" "cfg"  "targets")
                                :verbose t)
         asdf:*central-registry* :test 'equal))

(defvar *load-forces-recompile* nil
  "If nil, the fuzzbuster load-system in load.lisp does not force recompilation.
   Defaults to nil, but can be set before loading load.lisp."
)

(asdf:load-system :cl-ppcre :force *load-forces-recompile*)


;;;(pushnew :ccl-advice-profiler *features*)
#+ccl-advice-profiler
(asdf:load-system :profiler)


#+ccl-sam-profiler
(asdf:load-system :sam)


(asdf:load-system :fuzzbomb :force *load-forces-recompile*)



;; And switch the fuzzbomb package so that we don't need to prefix
;; anything with the package name.

(in-package :fuzzbomb)

;; Make sure we have our environment set up.
(fb-init-env)

;; brought forward from musliner-tools/named-objects.lisp b/c it apparently doesnt make it out of the packagizing
;; of musliner-tools (or something else about readtables), at least not in CCL.
(defun |#F-reader| (stream subchar arg)
  (declare (ignore subchar arg))
  (find-object (read stream t nil t)))

(set-dispatch-macro-character #\# #\F #'|#F-reader|)

(format t "Finished loading :fuzzbomb~%")
(format t "Fuzzbomb is in the :fuzzbomb package.~%")
(format t "You can make the symbols available with:~%")
(format t "(use-package :fb) or (in-package :fb) or :pa :fb~%")

;; [jrye:20111014.1331CST] Works for Dave, cause he loads this in his
;; .clinit.cl. Don't print a message, though because people who load
;; load.lisp not in their .clinit.cl don't automatically get switched
;; to the FB package.
;;
;; (format t "Oh OK, I'll just do it for you!~%")
;;(tpl:do-command "pa" :fb)
#+allegro
(tpl:setq-default top-level::*package* (find-package :fb))

;;; The idea is that we have some standard default *debug-list* items,
;;; including :top (in there by default) and then you can add in ones
;;; for modules you're working on in your local.lisp, which does not
;;; live in svn.
;;; If you want to see everything, (pushnew :all *debug-list*)
(pushnew :top *debug-list*)
(pushnew :amp *debug-list*)
(pushnew :reload *debug-list*)
;;(pushnew :sieve *debug-list*)
;; (pushnew :irm *debug-list*)
;; (pushnew :c3po *debug-list*)
;; (pushnew :exgen *debug-list*)
;; (pushnew :mft *debug-list*)
;; (pushnew :fuzz-wrapper *debug-list*)
;; (pushnew :patcher *debug-list*)
(pushnew :mr *debug-list*)
;; [jrye:20120130.1421CST] The deftask mechanism is mostly a low-level
;; mechanism, so we don't include it in the default set of tags.
;; (pushnew :deftask *debug-list*)

;; [jrye:20110227.0703CST] Force each dbug print to start on a new
;; line, prefix the output with a comment string so that we don't mess
;; up coloring in Emacs buffers.
;;
;;(setf *debug-prefix-function* #'musliner::flag-debug-prefix)
(setf *debug-prefix-function* #'(lambda (keys)
                                  (format nil "~&;; [~A] ~A"
                                          (musliner::tstamp nil)
                                          (musliner::flag-debug-prefix keys))))

(setf *program-name* "FB:")

#+nil
(progn
 (dbug :top "------ SVN version info:")
 (run-command "svn info")       ;; show what version we've just compiled
)
