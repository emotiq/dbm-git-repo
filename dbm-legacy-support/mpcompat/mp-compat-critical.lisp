
;; --------------------------------------------------
(in-package #:mp-compatibility)
;; --------------------------------------------------

!!! This is complete garbage!! The locks are created dynamically and offer no protection against reentrancy

(um:defmacro! critical (&body body)
  `(let ((,g!lock #.(make-lock :name "CriticalSection")))
     (with-lock (,g!lock)
       ,@body)))

(um:defmacro! spin-critical (&body body)
  `(let ((,g!lock #.(make-lock :name "SpinCriticalSection")))
     (with-spin-lock (,g!lock)
       ,@body)))
