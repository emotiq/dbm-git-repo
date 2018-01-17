
(in-package :fac)

(defun do-suspend (fn)
  (let (old-fn)
    (setf old-fn (become
                  (dlambda
                    (:resume (&rest args)
                     (become old-fn)
                     (apply fn args))
                    
                    (t (&rest msg)
                       (apply old-fn msg)))))
    ))

(defmacro suspend (args &body body)
  `(do-suspend (lambda ,args
                 ,@body)))

#|
(spawn (lambda ()
         (pr :Hello)
         (let ((self (current-actor)))
           (spawn (lambda ()
                    (sleep 2)
                    (send self :Doit!)
                    (sleep 1)
                    (send self :resume :A-message)
                    (sleep 2)
                    (send self :What?))))
         (become
          (dlambda
            (t (&rest msg)
               (pr msg))))
         (suspend (msg)
           (pr :Yes msg))))
 |#