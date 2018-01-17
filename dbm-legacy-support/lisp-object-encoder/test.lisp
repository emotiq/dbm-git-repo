
(defclass thing ()
  ())

(defmethod initialize-instance :around ((th thing) &key &allow-other-keys)
  (call-next-method))

(defmethod reinitialize-instance :around ((th thing) &key &allow-other-keys)
  (print "Hello from reinitialize-instance")
  (call-next-method))

(defmethod shared-initialize :around ((th thing) slot-names &key &allow-other-keys)
  (print "Hello from shared-initialize")
  (format t "~%Slot names: ~A" slot-names)
  (call-next-method))
