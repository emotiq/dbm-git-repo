;; mp-compatibility.lisp
;; --------------------------------------------------------------------------------------
;; Compatibility layer for Lispworks, Allegro, OS X, and Win32, Mulit-Processing Primitives
;;
;; Copyright (C) 2008 by SpectroDynamics, LLC. All rights reserved.
;;
;; DM/SD  08/08
;; --------------------------------------------------------------------------------------
#|
The MIT License

Copyright (c) 2017-2018 Refined Audiometrics Laboratory, LLC

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
|#

;; --------------------------------------------------
(in-package #:mp-compatibility)
;; equiv to #F
(declaim  (OPTIMIZE (SPEED 3) (SAFETY 0) #+:LISPWORKS (FLOAT 0)))
;; --------------------------------------------------
;; Compatibility Layer

(defun current-process ()
  "Get the current Lisp process."
  sb-thread:*current-thread*)

;; --------------------------------------------------------------------------

(defun process-name (proc)
  (sb-thread:thread-name proc))

(defun set-process-name (proc name)
  (setf (sb-thread:thread-name proc) name))

;; --------------------------------------------------------------------------

(defvar *process-plists* (make-hash-table :weakness :key :test 'eq))

(defun process-plist (proc)
  "Return the property list for the indicated Lisp process."
  (gethash proc *process-plists*))

(defun set-process-plist-entry (proc key val)
  (um:if-let (lst (process-plist proc))
	     (setf (getf lst key) val)
	     (setf (gethash proc *process-plists*) (list key val))))

;; --------------------------------------------------------------------------

(defun process-run-function (name flags proc &rest args)
  "Spawn a new Lisp thread and run the indicated function with inital args."
  (declare (ignore flags))
  (sb-thread:make-thread (lambda ()
			   (apply proc args))
			 :name name))

;; --------------------------------------------------------------------------

(defun process-kill (proc)
  "Kill the indicated Lisp process."
  (sb-thread:terminate-thread proc))

;; --------------------------------------------------------------------------

(defun process-interrupt (proc fn &rest args)
  "Interrupt the indicated Lisp process to have it perform a function."
  (sb-thread:interrupt-thread proc (lambda ()
				     (apply fn args))))

;; --------------------------------------------------------------------------

(defmacro without-preemption (&body body)
  "Perform the body forms without preemption."
  `(sb-sys:without-interrupts
     ,@body))

;; --------------------------------------------------------------------------
;; --------------------------------------------------------------------------

(defun make-lock (&key name important-p (safep t))
  "Make a Lisp lock."
  (declare (ignorable important-p safep))
  (sb-thread:make-mutex :name name))

;; --------------------------------------------------------------------------

(defun do-with-lock (lock timeout fn)
  (cond ((eq sb-thread:*current-thread* (sb-thread:mutex-owner lock))
	 (funcall fn))

	(timeout
	 (tagbody
	    (sb-sys:without-interrupts
	      (handler-case
		  (sb-ext:with-timeout timeout
		    (sb-sys:allow-with-interrupts
		      (sb-thread:get-mutex lock nil t))
		    (go have-lock))
		(timeout (cx)
		  (declare (ignore cx))
		  (go beyond))))
	    have-lock
	    (unwind-protect
		 (funcall fn)
	      (sb-sys:without-interrupts
		(sb-thread:release-mutex lock)))
	    beyond))

	(t (sb-thread:with-mutex (lock)
	     (funcall fn)))
	))

(defmacro with-spin-lock ((lock) &body body)
  `(with-lock (,lock) ,@body))

(defmacro with-lock ((lock &optional whostate timeout) &body body)
  "Wait for lock available, then execute the body while holding the lock."
  (declare (ignore whostate))
  `(do-with-lock ,lock ,timeout (lambda () ,@body)))

;; --------------------------------------------------------------------------

(defun lock-owner (lock)
  (sb-thread:mutex-owner lock))

;; --------------------------------------------------------------------------

(defun process-lock (lock &optional whostate timeout)
  (declare (ignore whostate))
  (cond ((eq sb-thread:*current-thread* (sb-thread:mutex-owner lock))
	 (funcall fn))
	
	(timeout
	 (tagbody
	    (sb-sys:without-interrupts
	      (handler-case
		  (sb-ext:with-timeout timeout
		    (sb-sys:allow-with-interrupts
		      (sb-thread:get-mutex lock nil t))
		    (go have-lock))
		(timeout (cx)
		  (declare (ignore cx))
		  (return-from process-lock nil))
		))
	  have-lock
	    (unwind-protect
		 (funcall fn)
	      (sb-sys:without-interrupts
		(sb-thread:release-mutex lock)))
	  beyond))
	
	(t (sb-thread:with-mutex (lock)
	     (funcall fn)))
	))

;; --------------------------------------------------------------------------

(defun process-unlock (lock)
  (sb-sys:without-interrupts
    (sb-thread:release-mutex lock)))

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
  "Wait with timeout for a message to arrive at the Lisp mailbox and return it.
A null timeout means wait forever."
  (mrmb:receive mbox timeout))

;; --------------------------------------------------------------------------

(defun mailbox-empty? (mbox)
  "Check if the Lisp mailbox is empty. Return generalized T/F."
  (mrmb:is-empty mbox))

;; --------------------------------------------------------------------------

(defun process-wait (wait-reason wait-fn &rest wait-args)
  #+:LISPWORKS
  (apply #'sb-thread:process-wait wait-reason wait-fn wait-args)
  #+:ALLEGRO
  (apply #'sb-thread:process-wait wait-reason wait-fn wait-args)
  #+:CLOZURE
  (apply #'sb-thread:process-wait wait-reason wait-fn wait-args))

;; --------------------------------------------------------------------------

(defun process-wait-with-timeout (wait-reason timeout
				  &optional wait-fn &rest wait-args)
  #+:LISPWORKS
  (apply #'sb-thread:process-wait-with-timeout wait-reason timeout wait-fn wait-args)
  #+:ALLEGRO
  (if timeout
      (apply #'sb-thread:process-wait-with-timeout wait-reason timeout wait-fn wait-args)
    (progn
      (apply #'sb-thread:process-wait wait-reason wait-fn wait-args)
      t))
  #+:CLOZURE
  (apply #'sb-thread:process-wait-with-timeout wait-reason
	 (round (* timeout sb-thread*ticks-per-second*))
	 wait-fn wait-args))

;; --------------------------------------------------------------------------

(defun generate-uuid ()
  (uuid:make-v4-uuid))

