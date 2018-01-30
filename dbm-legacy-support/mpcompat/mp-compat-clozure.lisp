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
;; equiv to #F
(declaim  (OPTIMIZE (SPEED 3) (SAFETY 0) (FLOAT 0)))
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
	 (process-unlock lock)))
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
	   (process-wait-with-timeout whostate
                                      timeout
                                      #'mp:try-lock (list lock)))
       (mp:grab-lock lock)))

;; --------------------------------------------------------------------------

(defun process-unlock (lock)
  (mp:release-lock lock))

;; --------------------------------------------------------------------------

(defclass queue ()
  ((lock :initarg :lock :initform (mp:make-lock) :reader get-lock)
   (semaphore :initarg :semaphore :initform (mp:make-semaphore) :accessor get-semaphore)
   (head :initarg :head :initarg :next-out :initform nil :accessor head :accessor next-out)
   (tail :initarg :tail :initarg :next-in :initform nil :accessor tail :accessor next-in)))

(defmethod enqueue ((q queue) item)
  "Enqueues new element at tail of queue."
  (mp:with-lock-grabbed ((get-lock q))
    (if (null (head q))
      (setf (tail q) (setf (head q) (cons item nil)))
      (setf (cdr (tail q)) (cons item nil)
            (tail q) (cdr (tail q))))
    (mp:signal-semaphore (get-semaphore q))
    (values item t)))

(defmethod dequeue ((q queue) &optional timeout)
  ; let OS check to see if anything's in the queue. More efficient to not take lock until after this happens.
  (let ((expired nil))
    (ccl:with-interrupts-enabled
        (if timeout
            (setf expired (not (mp:timed-wait-on-semaphore (get-semaphore q) timeout)))
            (mp:wait-on-semaphore (get-semaphore q))))
    (if expired
        (values nil nil)
        (mp:with-lock-grabbed ((get-lock q))
          (if (null (head q))
              (values nil nil) ; this can only happen if some other process emptied the queue
              ;  after we checked semaphore. Unlikely, but possible.
              (values (pop (head q)) t))))))

(defun make-mailbox (&key size)
  "Make a Lisp mailbox."
  (declare (ignorable size))
  (make-instance 'queue))

;; --------------------------------------------------------------------------

(defun mailbox-send (mbox msg)
  "Send a message to a Lisp mailbox."
  (enqueue mbox msg))

;; --------------------------------------------------------------------------

(defun mailbox-read (mbox &optional timeout)
  (when timeout (setf timeout (coerce timeout 'float)))
  (dequeue mbox timeout))

;; --------------------------------------------------------------------------

(defun mailbox-empty? (mbox)
  "Check if the Lisp mailbox is empty. Return generalized T/F."
  (null (head mbox)))

;; --------------------------------------------------------------------------

(defun process-wait (wait-reason wait-fn &rest wait-args)
  (apply #'mp:process-wait wait-reason wait-fn wait-args))

;; --------------------------------------------------------------------------

(defun process-wait-with-timeout (wait-reason timeout wait-fn &rest wait-args)
  (apply #'mp:process-wait-with-timeout wait-reason (round (* mp:*ticks-per-second* timeout)) wait-fn wait-args))

;; --------------------------------------------------------------------------

(defun generate-uuid ()
  (uuid:make-v4-uuid))

