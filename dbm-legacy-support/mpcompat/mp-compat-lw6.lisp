;; mp-compat-lw.lisp
;; --------------------------------------------------------------------------------------
;; Compatibility layer for Lispworks, Allegro, OS X, and Win32, Mulit-Processing Primitives
;;
;; Copyright (C) 2008 by SpectroDynamics, LLC. All rights reserved.
;;
;; DM/SD  08/08
;; --------------------------------------------------------------------------------------

;; --------------------------------------------------
(in-package #:mp-compatibility)
;; equiv to #F
(declaim  (OPTIMIZE (SPEED 3) (SAFETY 0) (FLOAT 0)))
;; --------------------------------------------------
;; Compatibility Layer

;; ------------------------------------------------
;; Spin-locks

(defun do-with-spinlock (lock whostate timeout fn &aux ans)
  ;; THIS IS NOT A SPIN-LOCK! Fix Me!!
  ;; ... but does it need to be fixed? Who uses this?
  (loop until (mp:with-lock (lock whostate timeout)
                (setf ans (multiple-value-list (funcall fn)))
                t))
  (values-list ans))

(defmacro with-spinlock ((lock &optional whostate (timeout 0)) &body body)
  `(do-with-spinlock ,lock ,whostate ,timeout (lambda () ,@body)))

#|(defmacro xwith-spinlock ((lock) &body body)
  `(mp:with-lock (,lock) ,@body))|#

(editor:setup-indent "with-spinlock" 1)

