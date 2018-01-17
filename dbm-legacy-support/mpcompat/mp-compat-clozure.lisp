;; mp-compatibility-clozure.lisp
;; --------------------------------------------------------------------------------------
;; Compatibility layer for Lispworks, Allegro, OS X, and Win32, Mulit-Processing Primitives
;;
;; Copyright (C) 2008 by SpectroDynamics, LLC. All rights reserved.
;;
;; DM/SD  08/08
;; --------------------------------------------------------------------------------------

;; --------------------------------------------------
(in-package #:mp-compatibility)
;; --------------------------------------------------
;; Compatibility Layer

(defun current-process ()
  "Get the current Lisp process."
  mp:*current-process*)

;; --------------------------------------------------------------------------

(defun process-name (proc)
  (mp:process-name proc))

(defun set-process-name (proc name)
  (setf (mp:process-name proc) name))

;; --------------------------------------------------------------------------
#|
;; NOT NEEDED FOR SUITABLY MODIFIED CCL SOURCE ADDING A PROCESS-PLIST SLOT
(defvar *process-plists* (make-hash-table :weak :key :test 'eq))

(defun process-plist (proc)
  "Return the property list for the indicated Lisp process."
  (gethash proc *process-plists*))

(defun set-process-plist-entry (proc key val)
  (um:if-let (lst (process-plist proc))
      (setf (getf lst key) val)
    (setf (gethash proc *process-plists*) (list key val))))
|#

;; FOR SUITABLY MODIFIED CCL SOURCE ADDING A PROCESS-PLIST SLOT
(defun process-plist (proc)
  "Return the property list for the indicated Lisp process."
  (ccl:process-plist proc))

(defun set-process-plist-entry (proc key val)
  (setf (getf (ccl:process-plist proc) key) val))

;; --------------------------------------------------------------------------

(defun process-run-function (name flags proc &rest args)
  "Spawn a new Lisp thread and run the indicated function with inital args."
  (declare (ignore flags))
  (apply #'mp:process-run-function name proc args))

;; --------------------------------------------------------------------------

(defun process-kill (proc)
  "Kill the indicated Lisp process."
  (mp:process-kill proc))

;; --------------------------------------------------------------------------

(defun process-interrupt (proc fn &rest args)
  "Interrupt the indicated Lisp process to have it perform a function."
  (apply #'mp:process-interrupt proc fn args))

;; --------------------------------------------------------------------------

(defmacro without-preemption (&body body)
  "Perform the body forms without preemption."
  `(mp:without-interrupts ,@body)) ;; not quite, but as close as we can get...

;; --------------------------------------------------------------------------
;; --------------------------------------------------------------------------

(defun make-lock (&key name important-p (safep t))
  "Make a Lisp lock."
  (declare (ignorable important-p safep))
  (mp:make-lock name))

;; --------------------------------------------------------------------------

(defmacro with-spin-lock ((lock) &body body)
  `(with-lock (,lock) ,@body))

(defmacro with-lock ((lock &optional whostate timeout) &body body)
  "Wait for lock available, then execute the body while holding the lock."
  `(do-with-lock ,lock ,whostate ,timeout (lambda () ,@body)))

(defun do-with-lock (lock whostate timeout fn)
  (if timeout
      (and
       (do-grab-lock-with-timeout lock whostate timeout)
       (unwind-protect
	    (funcall fn)
	 (mp:release-lock lock)))
      (mp:with-lock-grabbed (lock) (funcall fn))
      ))

;; --------------------------------------------------------------------------

(defun lock-owner (lock)
  (declare (ignorable lock))
  #|(error "lock-owner unimplemented")|#
  "YourGuessIsAsGoodAsMine")

;; --------------------------------------------------------------------------

(defun process-lock (lock &optional whostate timeout)
  (do-grab-lock-with-timeout lock whostate timeout))

(defun do-grab-lock-with-timeout (lock whostate timeout)
  (if timeout
       (or (mp:try-lock lock)
	   (mp:process-wait-with-timeout whostate
					 (round
					  (* timeout mp:*ticks-per-second*))
					 #'mp:try-lock (list lock)))
       (mp:grab-lock lock)))

;; --------------------------------------------------------------------------

(defun process-unlock (lock)
  (mp:release-lock lock))

;; --------------------------------------------------------------------------

(defun make-mailbox (&key size)
  "Make a Lisp mailbox."
  (declare (ignorable size))
  (mrmb:create))

;; --------------------------------------------------------------------------

(defun mailbox-send (mbox msg)
  "Send a message to a Lisp mailbox."
  (mrmb:send msg mbox))

;; --------------------------------------------------------------------------

(defun mailbox-read (mbox &optional timeout)
  (mrmb:receive mbox timeout))

;; --------------------------------------------------------------------------

(defun mailbox-empty? (mbox)
  "Check if the Lisp mailbox is empty. Return generalized T/F."
  (mrmb:is-empty mbox))

;; --------------------------------------------------------------------------

(defun process-wait (wait-reason wait-fn &rest wait-args)
  (apply #'mp:process-wait wait-reason wait-fn wait-args))

;; --------------------------------------------------------------------------

(defun process-wait-with-timeout (wait-reason timeout wait-fn &rest wait-args)
  (apply #'mp:process-wait-with-timeout wait-reason
         (and timeout (round (* timeout mp:*ticks-per-second*)))
         wait-fn wait-args))

;; --------------------------------------------------------------------------

(defun generate-uuid ()
  (uuid:make-v4-uuid))

