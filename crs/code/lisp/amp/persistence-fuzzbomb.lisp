(in-package :persistence)

#-persist-named-objects
(defmethod things-to-save :before ((obj musliner:named-object))
  (error "Persisting named-objects is not allowed."))

#+persist-named-objects
(defmethod things-to-save :around ((obj musliner:named-object))
  (let ((retval (call-next-method)))
    (delete 'musliner:name retval))
  )
