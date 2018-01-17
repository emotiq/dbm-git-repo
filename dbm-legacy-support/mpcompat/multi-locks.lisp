;; --------------------------------------------------
(in-package #:mp-compatibility)
;; --------------------------------------------------

;; ------------------------------------------------
;; Multi-locks

(defun release-all-locks (held)
  (dolist (lock held)
    (when (lock-owned-by-current-process-p lock)
      (process-unlock lock))))

(defun grab-all-locks (locks spin)
  (um:nlet-tail grab-locks ((pend locks)
                            (held nil))
    (when pend
      (let ((lock (car pend)))
        (if (process-lock lock nil 0)
            (grab-locks (cdr pend) (cons lock held))
          ;; else
          (progn
            (release-all-locks held)
            (unless spin
              (with-lock (lock)
                nil))
            (grab-locks locks nil)) )) )))

(defun do-with-locks (locks fn spin)
  ;; grab all the locks and execute the function fn, or else release
  ;; all the locks grabbed so far and try again. Locks are
  ;; automatically released on exit or abort.
  ;;
  ;; In order to avoid livelock, where one thread wants some locks
  ;; sought by another thread, caller should always supply the list in
  ;; a consistent order.
  (if locks
      (unwind-protect
          (progn
            (grab-all-locks locks spin)
            (funcall fn))
        (release-all-locks locks))
    ;; else - empty lockset
    (funcall fn)))

(defmacro with-locks (locks &body body)
  `(do-with-locks ,locks (lambda () ,@body) nil))

#+:LISPWORKS
(editor:setup-indent "with-locks" 1)
   
;; -------------------------------------------------------------
;; Spin-multi-locks

(defmacro with-spinlocks (locks &body body)
  `(do-with-locks ,locks (lambda () ,@body) t))

#+:LISPWORKS
(editor:setup-indent "with-spinlocks" 1)
   
;; ---------------------------------------------------------------

#| tests...
(let ((l1 (make-lock))
      (l2 (make-lock))
      (l3 (make-lock)))
  (with-locks (list l1 l2 l3)
    (print "hello")))

|#
