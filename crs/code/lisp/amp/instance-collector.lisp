;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; COPYRIGHT START
;;; COPYRIGHT END
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;***************************************************************************

(cl:in-package :fuzzbomb)
(eval-when (compile) (optimization-boilerplate))

(defgeneric in-use (obj)
  (:documentation "Used to determine whether things that are potentially
in use in a given scenario are ACTUALLY in use."))

;;;---------------------------------------------------------------------------

(defclass instance-collector (standard-class)
     (
      (instances :initform nil :accessor instances)
      )
  )

#+cmu
(defmethod pcl:validate-superclass ((class instance-collector)
                                    (superclass standard-object))
  t)
#+sbcl
(defmethod sb-mop:validate-superclass ((class instance-collector)
                                       (superclass standard-object))
  t)
#+ccl
(defmethod ccl:validate-superclass ((class instance-collector)
                                    (superclass standard-object))
  t)

(defmethod print-object ((class instance-collector) stream)
  (format stream "#<INSTANCE-COLLECTOR: ~A>" (name class)))

(defmethod name ((class instance-collector))
  (class-name class))

;; The following method does all that MY-DEFCLASS used to do...
;; I am not satisfied that I have a graceful way of defining the
;; <name>-p method yet --- that seems like a macro-y thing to
;; do, not an easy MOP thing...
(defmethod initialize-instance :after ((newclass instance-collector)
                                 &key name
                                 &allow-other-keys)
  ;; automatically defines name-p class determination predicate
  (setf (symbol-function (intern (concatenate 'string
                                              (symbol-name name)
                                              "-P")
                                 :fuzzbomb))
        #'(lambda (x) (typep x name))))


;; aliases for backward compatibility...
(declaim (inline class-p))

(defun class-p (cl)
  (typep cl 'standard-class))

;;;---------------------------------------------------------------------------
;;; Reproducing the functions of MY-MAKE-INSTANCE:
;;;
;;; This used to do make-instance, push a marker onto the backtracking
;;; stack, and track the instances of the class.
;;; There used to be two forms of it, one a NOBT-MAKE-INSTANCE, that
;;; doesn't put the instance on the backtrack stack for undoing.  This
;;; second form is never called, so I pruned it.
;;; 
;;;---------------------------------------------------------------------------

(defmethod make-instance :around ((class instance-collector) &rest initargs)
  (declare (ignore initargs))
  (let ((instance (call-next-method)))
    (push instance (instances class))
    instance))

;;;----------------------------------------------------------------------------
(defun reset-class (name)
  (remove-all-instances name))

;;;***************************************************************************
;;; returns all instances in WM of class-name or a subclass of class-name
;;; - if the in-use arg is non-nil, it only returns those whose "in-use"
;;;     slot is non-nil.

(defun find-all-instances (class-name &optional (in-use nil))
  "Return all the instances of the CLASS-NAME class.  CLASS-NAME must be
a class whose metaclass is INSTANCE-COLLECTOR.  If the optional IN-USE argument is
T, filter out all the instances that are not flagged as IN-USE.  This
supports the CIRCA API.  NOTE:  If there isn't an in-use method for objects 
of class class-name, then you must set in-use to nil, or this will puke."
  (recursive-find-instances #-pcl (find-class class-name)
                            #+pcl (pcl:find-class class-name)
                            in-use))

(defgeneric recursive-find-instances (class &optional in-use)
  (:method ((class instance-collector) &optional (in-use T))
     (let  ((instance-list
             (if in-use
                 (remove-if-not #'in-use (instances class))
                 (instances class)))
            (subclasses
             #+allegro
              (clos:class-direct-subclasses class)
              #+cmu
              (pcl:class-direct-subclasses class)
              #+sbcl
              (sb-mop:class-direct-subclasses class)
              #+ccl
              (ccl:class-direct-subclasses class)
              ))
       (dolist (subclass subclasses)
         (setf instance-list (append instance-list
                                     (recursive-find-instances subclass in-use))))
       instance-list)))


;; this could be made more efficient by not prefetching all of the
;; objects of the subtypes...  Lots of consing goes on here...

(defun find-instance-by-name (type name &optional (error-p t))
 (if (and (symbolp name) (member type '(trans event temporal reliable action)))
  (find-instance-by-symbolname type name error-p)
  (let ((instance (find name (find-all-instances type nil) :test #'equalp
                        :key #'name)))
    (cond ((null error-p) instance)
          (instance)
          (t (error "Unable to find ~A ~A!" type name))))))


(defun find-instance-by-symbolname (type name &optional (error-p t))
   (let ((instance (find (symbol-name name)
                        (find-all-instances type) :test #'equalp
                                :key #'name)))
    (cond ((null error-p) instance)
          (instance)
          (t (error "Unable to find ~A ~A!" type name)))))

;;;------------------------------------------------------------------------
(defun apply-to-all-instances (fctn class-name)
  (mapcar fctn (find-all-instances class-name)))

;;;------------------------------------------------------------------------
;;; - removes all instances that are identified as being of class-name or of
;;;     any of its subclasses.
(defgeneric remove-all-instances (class))

(defmethod remove-all-instances ((class-name symbol))
  (remove-all-instances #-pcl (find-class class-name)
                        #+pcl (pcl:find-class class-name)
                        ))

(defmethod remove-all-instances ((class instance-collector))
  (setf (instances class) nil)
  (dolist (subclass
           #+allegro
           (clos:class-direct-subclasses class)
           #+cmu
           (pcl:class-direct-subclasses class)
           #+sbcl
           (sb-mop:class-direct-subclasses class)
           #+ccl
           (ccl:class-direct-subclasses class)
           )
    (remove-all-instances subclass)))

;;;------------------------------------------------------------------------
(defun remove-instance (inst)
  (let ((class #-pcl(class-of inst) #+pcl(pcl:class-of inst)))
    (assert (member inst (instances class) :test #'eq))
    (setf (instances class) (delete inst (instances class) :test #'eq))))
