;; clozure-timer.lisp -- "Proper" timers for CCL
;;
;; DM/Emotiq  01/18
;; ------------------------------------------------------------------

(in-package :clozure-timer)

(defvar *timeout-queue*  (priq:make-priq))
(defvar *cancel-queue*   (priq:make-priq))
(defvar *cycle-bits*     0.0)
(defvar *last-check*     0)
(defvar *timeout-tree*   (maps:empty))

(defclass timer ()
  ((period   :accessor timer-period
             :initarg  :period
             :initform nil)
   (t0       :accessor timer-t0
             :initarg  :t0
             :initform 0)
   (fn       :accessor timer-fn
             :initarg  :fn
             :initform (constantly nil))
   (args     :accessor timer-args
             :initarg  :args
             :initform nil)
   ))

(defun make-timer (fn &rest args)
  (make-instance 'timer
                 :fn   fn
                 :args args))

(defmethod schedule-timer ((timer timer) t0 &optional repeat)
  (setf (timer-t0 timer) t0
        (timer-period timer) repeat)
  (priq:addq *timeout-queue* timer))

(defmethod schedule-timer-relative ((timer timer) trel &optional repeat)
  (let ((t0 (+ trel (get-universal-time))))
    (setf (timer-t0 timer)     t0
          (timer-period timer) repeat)
    (priq:addq *timeout-queue* timer)))

(defmethod unschedule-timer ((timer timer))
  (priq:addq *cancel-queue* timer))


(defun #1=check-timeouts ()
  ;; read new requests
  (loop for timer = (priq:popq *timeout-queue*)
        while timer
        do
        (let* ((t0     (timer-t0 timer))
               (timers (maps:find t0 *timeout-tree*)))
          (setf *timeout-tree* (maps:add t0 (cons timer timers) *timeout-tree*))))
  ;; process cancellations
  (loop for timer = (priq:popq *cancel-queue*)
        while timer
        do
        (let* ((t0     (timer-t0 timer))
               (timers (maps:find t0 *timeout-tree*)))
          (setf *timeout-tree* (maps:add t0 (delete timer timers) *timeout-tree*))))
  ;; check our current time
  (let ((now (get-universal-time)))
    (if (= now *last-check*)
        (setf now (+ now (incf *cycle-bits* 0.1)))
      (setf *cycle-bits* 0.0
            *last-check* now))
    ;; fire off expired timers
    (maps:iter (lambda (t0 timer-list)
                 (when (> t0 now)
                   (return-from #1#))
                 (setf *timeout-tree* (maps:remove t0 *timeout-tree*))
                 (dolist (timer timer-list)
                   (let ((per (timer-period timer)))
                     (when per
                       (schedule-timer-relative timer per per)))
                   (multiple-value-bind (ans err)
                       (ignore-errors
                         (apply (timer-fn timer) (timer-args timer)))
                     (declare (ignore ans))
                     (when err
                       (unschedule-timer timer)))))
               *timeout-tree*)))
      
(defun make-master-timer ()
  (mp:process-run-function "Master Timer"
                           #+:LISPWORKS ()
                           (lambda ()
                             (loop
                              (sleep 0.1)
                              (check-timeouts)))))

(make-master-timer)

#|
(let ((timer (make-timer (lambda () (print :Howdy!)))))
  (schedule-timer-relative timer 3 :repeat 1)
  (sleep 30)
  (unschedule-timer timer))
 |#

