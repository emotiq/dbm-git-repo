
(in-package :useful-macros)

;; ------------------------------------------------------

(defmacro! def-typed-fn (type-name args &body body &environment env)
  (let ((maker-name (intern (concatenate 'string (string :MAKE-)
                                         (string type-name)))))
    (um:ensure-thread-eval-def maker-name
      `(progn
         (defclass ,type-name ()
           ()
           (:metaclass clos:funcallable-standard-class))
         
         (defmethod initialize-instance :after ((fn ,type-name) &key behavior &allow-other-keys)
           (clos:set-funcallable-instance-function fn behavior))
         
         (defun ,maker-name ,args
           (let (,a!self)
             (setf ,a!self (make-instance ',type-name
                                          :behavior (progn
                                                      ,@body)))
             )))
      env)))

#+:LISPWORKS
(editor:setup-indent "def-typed-fn" 2)

