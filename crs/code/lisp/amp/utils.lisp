;;; -------------------------------------------------------------------------
;;; utils.lisp
;;; -------------------------------------------------------------------------

(cl:in-package :fuzzbomb)

;;; General utils used w/in the fuzzbomb amp.

(defun eval-all-from-string (string)
  (let ((stream (make-string-input-stream string))
        a)
    (loop while (setf a (read stream nil))
       collecting (eval a))))

;;; ------------------------------------------------------------
;;; Helpers

(defun summarize-string-list (strings &key (length 50))
  "Returns a string summarizing the contents of STRINGS"
  (format nil "~{~a~^, ~}" (mapcar #'(lambda (s) (summarize-string s :length length)) strings)))

(defun summarize-string (str &key length)
  "Returns a summary of STR. The summary includes the length and a quoted version of the string. The quoted version of the string is optionally limited to LENGTH characters."

  (let (summary)
    (when str
      (let ((shortened-str (printable-string str)))
        (when (and length
                   (> length 4)
                   (> (length shortened-str) length))
          (let* ((prefix-length (ceiling (/ (- length 3) 2)))
                 (suffix-length (- length prefix-length 3)))
            (setf shortened-str
              (concatenate 'string
                (subseq shortened-str 0 prefix-length)
                "..."
                (subseq shortened-str (- (length shortened-str) suffix-length))))))
        ;; (string-maxlength shortened-str length)))
        (setf summary (format nil "[~A] \"~A\""
                              (length str)
                              shortened-str
                              ))))
    summary
    ))

(defun pathname-looks-like-c-source-p (pathname)
  (cl-ppcre:scan ".c$|.C$|.cpp|.CPP" (namestring pathname)))

(defun python-boolean-to-lisp (boolean-string)
  "Returns t if string is true, ignoring case."
  (when (string-equal "true" boolean-string)
    t))

(defun powerset (s)
  (if s (mapcan (lambda (x) (list (cons (car s) x) x))
		(powerset (cdr s)))
      '(())))

(defun size-of-powerset (s)
  (expt 2 (list-length s)))
