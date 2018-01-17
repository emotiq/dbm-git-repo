;; lw-rwgate.lisp -- R/W Gate done with LW sharing locks
;;
;; DM/RAL 02/17
;; ------------------------------------------------------------

(defpackage #:lw-rwgate
  (:use #:common-lisp)
  (:export
   #:make-rwgate
   #:with-read-lock
   #:with-write-lock))

(in-package #:lw-rwgate)

(defun make-rwgate ()
  (mp:make-lock :sharing t))

(defun do-with-lock (lock lock-type timeout fn abortfn)
  (let (has-lock)
    (multiple-value-bind (lockfn unlockfn)
        (case lock-type
          (:read  (values #'mp:process-sharing-lock
                          #'mp:process-sharing-unlock))
          (:write (values #'mp:process-exclusive-lock
                          #'mp:process-exclusive-unlock)))
      
      (labels ((ok-to-proceed ()
                 (mp:with-interrupts-blocked
                   (setf has-lock (funcall lockfn lock :waiting 0)))))
        
        (hcl:unwind-protect-blocking-interrupts-in-cleanups
            (when (or (ok-to-proceed)
                      (mp:wait-processing-events timeout
                                                 :wait-function #'ok-to-proceed))
              (funcall fn))
          (if has-lock
              (funcall unlockfn lock)
            ;; else
            (when abortfn
              ;; abortfn called with interrupts disabled
              (funcall abortfn))
            ))
        ))))

(defmacro with-read-lock ((lock &key timeout abortfn) &body body)
  `(do-with-lock ,lock :read ,timeout (lambda () ,@body) ,abortfn))

(defmacro with-write-lock ((lock &key timeout abortfn) &body body)
  `(do-with-lock ,lock :write ,timeout (lambda () ,@body) ,abortfn))

