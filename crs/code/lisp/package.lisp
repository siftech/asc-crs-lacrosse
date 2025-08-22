;;; -------------------------------------------------------------------------
;;; $Id: package.lisp 1612 2014-05-08 21:58:52Z sfriedman $
;;; -------------------------------------------------------------------------
;;; COPYRIGHT START
;;; Copyright (c) 2012, Smart Information Flow Technologies (SIFT).  Developed with the sponsorship of the Defense Advanced Research Projects Agency (DARPA) and Air Force Research Laboratory.
;;; COPYRIGHT END
;;; -------------------------------------------------------------------------

(defpackage :fuzzbomb
  (:nicknames :fb)
  (:shadowing-import-from :alexandria "WITH-GENSYMS")
  (:shadowing-import-from :bordeaux-threads "ALL-THREADS" "CURRENT-THREAD" "THREAD-NAME")
  (:use :common-lisp
        :iterate
        :musliner
        #+allegro :socket	
        )
  ;; Prefer symbols from iterate package over musliner package for these.
  (:shadowing-import-from :iterate #:for)
  (:shadowing-import-from :iterate #:while)
  )
