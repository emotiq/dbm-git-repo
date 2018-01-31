
(in-package :um)

(defun do-with-timeout (timeout cleanup fn)
  (let* ((mbox   (mp:make-mailbox))
         (thread (mpcompat:process-run-function "Wait Function" '()
                                          (lambda ()
                                            (mp:mailbox-send mbox (um:capture-ans-or-exn fn)))
                                          )))
    (multiple-value-bind (ans okay)
        (mp:mailbox-read mbox :timeout timeout)
      (if okay
          (values
           (recover-ans-or-exn ans)
           t)
        ;; else
        (progn
          (mp:process-terminate thread)
          (when cleanup
            (funcall cleanup))
          nil))
      )))

(defmacro with-timeout ((timeout &key cleanup) &body body)
  `(do-with-timeout ,timeout ,cleanup
                    (lambda ()
                      ,@body)))

#+:LISPWORKS
(editor:setup-indent "WITH-TIMEOUT" 1)
