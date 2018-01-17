
(in-package :fac)

(defun do-recv (select-fn timeout)
  (let ((self (current-actor)))
    (unless (get-property self 'has-recv)
      (let (old-fn
            selector-fn
            (msgq (priq:make-unsafe-fifo))
            timer)
        (setf old-fn
              (become
               (dlambda
                 (:recv-{204E1756-D84E-11E7-9D93-985AEBDA9C2A} (sel-fn timeout)
                   (setf selector-fn sel-fn)
                   (when timeout
                     (unless timer
                       (setf timer (mp:make-timer #'send self
                                                  :timeout-{3A95A26E-D84E-11E7-9D93-985AEBDA9C2A})))
                     (mp:schedule-timer-relative timer timeout)))
                 (:timeout-{3A95A26E-D84E-11E7-9D93-985AEBDA9C2A} ()
                  (error "RECV Timeout"))
                 (t (&rest msg)
                    (if selector-fn
                        (if-let (ans-fn (funcall selector-fn msg))
                            (progn
                              (when timer
                                (mp:unschedule-timer timer))
                              (setf selector-fn nil)
                              (funcall ans-fn)
                              (foreach (lambda (msg)
                                         (apply self msg))
                                       (priq:contents msgq)))
                          ;; else - not one of the messages we are looking for
                          (priq:addq msgq msg))
                      ;; else -- not currently in a RECV
                      (apply old-fn msg)))
                 ))
              (get-property self 'has-recv) t)))
    (funcall self :recv-{204E1756-D84E-11E7-9D93-985AEBDA9C2A} select-fn timeout)))
