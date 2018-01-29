;; Actors.lisp -- An implementation of Actors - single thread
;; semantics across multithreaded systems
;;
;; DM/RAL  12/17
;; -----------------------------------------------------------

(in-package #:actors)

;; equiv to #F
(declaim  (OPTIMIZE (SPEED 3) (SAFETY 0) (FLOAT 0)))

;; ------------------------------------------------------

(declaim (inline current-actor))

(defvar *current-actor* nil)

(defun current-actor ()
  ;; each running thread will have its own version of this global
  ;; value. But, if non-nil, it points to the currently active Actor
  ;; running in that thread
  *current-actor*)

(defvar *actor-ready-queue*   (make-prio-mailbox)) ;; a queue of pending Actor activations
(defvar *executive-processes* nil)                 ;; the list of Executive threads

(defun do-nothing (&rest args)
  (declare (ignore args))
  ;; literally, do nothing...
  )
           
;; ----------------------------------------------------
;; An Actor mailbox contains a regular priority mailbox plus a list of
;; previously stashed messages. Stashed messages will be read before
;; additional mailbox messages. Message may become stashed, e.g.,
;; during operation of a selective RECV.

(defclass actor-mailbox ()
  ((mbox   :reader   actor-message-mbox
           :initform (make-prio-mailbox))
   (replay :accessor actor-message-replay
           :initform nil)))

;; -----------------------------------------------------

(defmethod next-message ((mbox actor-mailbox))
  (if (actor-message-replay mbox)
      ;; replay stashed messages in order of arrival
      (values (pop (actor-message-replay mbox)) t)
    ;; else - mailbox read with no name, zero timeout - immediate
    ;; return of (val t/f)
    (mailbox-read (actor-message-mbox mbox) "" 0)))

(defmethod enqueue-replay ((mbox actor-mailbox) lst)
  ;; enqueue our list of messages ahead of pending stashed in mailbox
  (setf (actor-message-replay mbox) (nconc lst (actor-message-replay mbox))))

(defmethod deposit-message ((mbox actor-mailbox) msg &key (priority 0))
  ;; deposit one message into the mailbox
  (mailbox-send (actor-message-mbox mbox) msg :prio priority))

(defmethod mailbox-not-empty-p ((mbox actor-mailbox))
  ;; true if either stashed messsages or some in mailbox
  (or (actor-message-replay mbox)
      (not (mailbox-empty-p (actor-message-mbox mbox)))))

;; -----------------------------------------------------
;; Version 3... make the Actor's internal state more readily visible
;; to debuggers.

(defclass actor ()
  ((properties
    ;; globally visible properties on this Actor. SMP-safe methods are
    ;; provided for getting / setting
    :reader    actor-properties
    :initarg   :properties
    :initform  (make-instance '<shared-plist>))
   (priority
    ;; we don't use this yet, but maybe someday there will be good
    ;; reason to do so.
    :accessor  actor-priority
    :initarg   :priority
    :initform  0)
   (mbox
    ;; the Actor's message queue. SMP-safe
    :reader    actor-mailbox
    :initform  (make-instance 'actor-mailbox))
   (recv-info
    ;; when non-nil this points to the RECV block in control. Only the
    ;; Actor queries so SMP safety not a concern on this slot.
    :accessor  actor-recv-info
    :initform  nil)
   (busy
    ;; when non-nil this Actor is either already enqueued for running,
    ;; or is running. We use a CONS cell for the flag for SMP CAS
    :reader    actor-busy
    :initform  (list nil))
   (user-fn
    ;; points to the user code describing the behavior of this Actor.
    ;; This pointer is changed when the Actor performs a BECOME. Only
    ;; the Actor queries this slot so SMP safety not a concern.
    :accessor  actor-user-fn
    :initarg   :fn
    :initform  'do-nothing)
   ))

;; -----------------------------------------------------
;; These methods can be called from any thread. SMP safe.

(defun make-actor (fn &key (priority 0) properties)
  (make-instance 'actor
                 :fn         fn
                 :priority   priority
                 :properties (make-instance '<shared-plist>
                                    :initial properties)))

(defmethod get-property ((actor actor) key &optional default)
  ;; SMP-safe
  (get-kv key (actor-properties actor) default))

(defmethod set-property ((actor actor) key value)
  ;; SMP-safe
  (setf (get-kv key (actor-properties actor)) value))

(defsetf get-property set-property)

;; --------------------------------------------------------

(defmethod send ((self actor) &rest msg)
  ;; send a message to an Actor and possibly activate it if not
  ;; already running. SMP-safe
  (let ((mbox (actor-mailbox self)))
    (labels ((add-self-to-ready-queue ()
               ;; Mark busy, if not already marked. And if it wasn't
               ;; already marked, place it into the ready queue and be
               ;; sure there are Executives running.
               (when (CAS (car (actor-busy self)) nil t)
                 ;; The Ready Queue just contains function closures to
                 ;; be dequeued and executed by the Executives.
                 (mailbox-send *actor-ready-queue* #'run
                               :prio (actor-priority self))
                 (unless *executive-processes*
                   (ensure-executives))))
             
             (run ()
               (#+:LISPWORKS hcl:unwind-protect-blocking-interrupts-in-cleanups
                #+:ALLEGRO   unwind-protect
                   (let ((*current-actor* self))
                     (loop for (msg ok) = (multiple-value-list
                                           (next-message mbox))
                           while ok do (apply 'dispatch-message self msg)))
                 ;; <-- a message could have arrived here, but would
                 ;; have failed to enqueue the Actor.  So we double
                 ;; check after clearing the busy mark.
                 (CAS (car (actor-busy self)) t nil)
                 (when (mailbox-not-empty-p mbox)
                   (add-self-to-ready-queue)))))
      (deposit-message mbox msg)
      (add-self-to-ready-queue))
    ))

;; -----------------------------------------------
;; Since these methods are called against (CURRENT-ACTOR) they can
;; only be called from within a currently active Actor.

(defmethod become (new-fn)
  ;; change behavior, returning old. If an Actor didn't call this, an
  ;; error will result.
  (shiftf (actor-user-fn (current-actor)) new-fn))

(defun self-call (&rest msg)
  ;; send a message to myself, immediate execution. If an Actor didn't
  ;; call this, an error will result.
  (apply 'dispatch-message (current-actor) msg))

;; ------------------------------------------------
;; RECV handling
;;
;; RECV under Actors is asynchronous with callback.  Consecutive RECV
;; forms in the body code enqueue internal messages to ensure
;; sequential performance of the successive RECV clauses. When the
;; first RECV clause finishes its callback or timeout, the next will
;; start.
;;
;; But RECV clauses perform without waiting, just falling through on
;; first encounter. While a RECV clause is active, it modifies the
;; behavior of the Actor to intercept messages selectively, stashing
;; those that don't match one of the RECV clauses, for later
;; execution.
;;
;; When a RECV is in operation, the RECV-INFO slot of the Actor points
;; to one of these control blocks.

(defclass recv-info ()
  ((id          :reader   recv-info-id
                :initarg  :id)
   (recvq       :reader   recv-info-recvq
                :initform (make-unsafe-fifo))
   (msgq        :reader   recv-info-msgq
                :initform (make-unsafe-fifo))
   (selector-fn :reader   recv-info-selector-fn
                :initarg  :selector-fn)
   (timeout-fn  :reader   recv-info-timeout-fn
                :initarg  :timeout-fn)
   ;; sharing lock for timer - concession for ACL
   (lock        :reader   recv-info-timer-lock
                :initform (mpcompat:make-lock))
   ;; currently active timer - when nil => none
   (timer       :accessor recv-info-timer
                :initarg  :timer)))

;; ----------------------------------------
;; RECV handlers...

(defmethod enqueue-replay ((self actor) (info recv-info))
  ;; the RECV has finished... enqueue all the stashed messages in the
  ;; Actor's mailbox, giving priority to internal RECV messages.
  ;;
  ;; This method overrides the one for the Actor's mailbox
  (enqueue-replay (actor-mailbox self)
                  (nconc (contents (recv-info-recvq info))
                         (contents (recv-info-msgq  info))))
  (setf (actor-recv-info self) nil)) ;; revert to non-RECV behavior

(defmethod actor-recv-timeout ((self actor) timer-id)
  ;; a timeout occurred... is it ours? If not, just ignore.
  (let ((recv-info (actor-recv-info self)))
    (when (and recv-info
               (eq (recv-info-id recv-info) timer-id))
      (enqueue-replay self recv-info)
      (if-let (fn (recv-info-timeout-fn recv-info))
          (funcall fn)
        (error "RECV Timeout")))))
         
(defmethod actor-recv-setup ((self actor) conds-fn timeout-fn timeout-expr)
  ;; setup a new RECV control block in the current Actor, hence
  ;; activating RECV behavior until we find a message we want, or
  ;; else timeout waiting for one.
  (let ((this-id (vector))) ;; make a unique object
    (setf (actor-recv-info self)
          (make-instance 'recv-info
                         :id          this-id
                         :selector-fn conds-fn
                         :timeout-fn  timeout-fn
                         :timer       (make-timeout-timer timeout-expr self this-id)
                         ))))

;; -------------------------------------------------------------
;; Timeout Timers...

(defun send-timeout-message (self this-id)
  (when (readout-timer (actor-recv-info self) this-id)
    ;; are we still awaited?
    (send self
          :recv-timeout-{3A95A26E-D84E-11E7-9D93-985AEBDA9C2A}
          this-id)))

#+:LISPWORKS
(defun make-timeout-timer (delta self this-id)
  (when delta
    (let ((timer (mp:make-timer
                  'send-timeout-message self this-id)))
      (mp:schedule-timer-relative timer delta)
      timer)))

#+:ALLEGRO
(defun make-timeout-timer (delta self this-id)
  (when delta
    (mp:process-run-function
     "RECV Timeout Timer"
     (lambda ()
       (mp:process-sleep delta)
       (send-timeout-message self this-id))
     )))
  
(defmethod readout-timer ((info recv-info) tid)
  ;; destructively read out the timer
  (when info
    ;; might not be any info by now...
    (mpcompat:with-lock ((recv-info-timer-lock info))
      (when (eq tid (recv-info-id info))
        ;; might be a new timer info block...
        (shiftf (recv-info-timer info) nil)))))

(defmethod kill-timer ((info recv-info))
  (when-let (timer (readout-timer info (recv-info-id info)))
    #+:LISPWORKS
    (mp:unschedule-timer timer)
    #+:ALLEGRO
    (mp:process-kill timer)
    ))

;; -------------------------------------------------------------

(defmethod actor-recv-test-message ((self actor) msg)
  ;; see if the incoming message matches one of our RECV handlers
  (let* ((recv-info (actor-recv-info self))
         (ans-fn    (funcall (recv-info-selector-fn recv-info) msg)))
    (if ans-fn
        (progn
          (kill-timer recv-info)
          (enqueue-replay self recv-info)
          (funcall ans-fn))
      ;; else - not a message we are looking for
      (addq (recv-info-msgq recv-info) msg))))
            
;; ------------------------------------------------------
;; The main outer dispatch method for all Actors. It is here that we
;; differentiate among messages during active RECV, intercept RPC ASK
;; messages to reflect errors back to the caller, and perform
;; continuation messages resulting from callbacks. Otherwise, we
;; forward the message to the user's Actor code.

(defmethod dispatch-message ((self actor) &rest msg)
  (dcase msg
    
    (:continuation-{14AFB5F8-D01F-11E7-A1BE-985AEBDA9C2A} (fn &rest vals)
     ;; Used for callbacks into the Actor
     (apply fn vals))
    
    (:recv-timeout-{3A95A26E-D84E-11E7-9D93-985AEBDA9C2A} (timer-id)
     ;; an incoming RECV timeout message
     (actor-recv-timeout self timer-id))
    
    (:recv-setup-{204E1756-D84E-11E7-9D93-985AEBDA9C2A} (conds-fn timeout-fn timeout-expr)
     ;; another RECV clause. If not already in a RECV clause, activate
     ;; it. Otherwise stash it as an internal RECV message to be run
     ;; after the current RECV clause finishes.
     (if-let (recv-info (actor-recv-info self))
         (addq (recv-info-recvq recv-info) msg)
       (actor-recv-setup self conds-fn timeout-fn timeout-expr)))
    
    (t (&rest msg)
       (cond ((actor-recv-info self)
              ;; we are in an active RECV clause - handle or stash
              ;; this message
              (actor-recv-test-message self msg))
             
             (t 
              ;; else -- not currently in a RECV
              (dcase msg
                (:ask-{061B3878-CD81-11E7-9B0D-985AEBDA9C2A} (replyTo &rest msg)
                 ;; Intercept queries to send back a response from the
                 ;; following message, reflecting any errors back to
                 ;; the caller.
                 (send replyTo (apply 'capture-ans-or-exn
                                'self-call msg)))
                     
                (t (&rest args)
                   ;; anything else is up to the programmer who
                   ;; constructed this Actor
                   (apply (actor-user-fn self) args))
                ))
             ))
    ))

;; ------------------------------------------
;; Create a callback on the function argument

(defclass callback-function ()
  ()
  (:metaclass #+:LISPWORKS clos:funcallable-standard-class
              #+:ALLEGRO   mop:funcallable-standard-class))

(defmethod initialize-instance :after ((obj callback-function) &key behavior &allow-other-keys)
  (#+:LISPWORKS clos:set-funcallable-instance-function
   #+:ALLEGRO   mop:set-funcallable-instance-function
   obj behavior))

(defmethod =cont ((contfn callback-function))
  contfn)

(defmethod =cont ((contfn function))
  (if-let (self (current-actor))
      ;;
      ;; If the callback originated from inside an Actor, we ensure
      ;; that it will later execute inside that Actor only when that
      ;; Actor is alive.
      ;;
      ;; Code inside an Actor should only be executing on one thread
      ;; at a time, in order to preserve SMP single-thread semantics.
      ;;
      (make-instance 'callback-function
                     :behavior (lambda (&rest args)
                                 (if (eq self (current-actor))
                                     (apply contfn args)
                                   (apply 'send self :continuation-{14AFB5F8-D01F-11E7-A1BE-985AEBDA9C2A} contfn args))))
    ;; else - not originating from inside of an Actor, just return the
    ;; function unchanged
    contfn))

;; ------------------------------------------

(defmacro without-actor-status (&body body)
  ;;
  ;; Used to avoid deadlocking an Actor.
  ;;
  ;; In general, if you will have the Actor hang waiting on a resource
  ;; (e.g. a local mailbox), and the only way to release that resource
  ;; is to perform a callback function, that callback function would
  ;; ordinarily be redirected as a continuation message to the Actor.
  ;; The Actor would have to respond to the message, and you will have
  ;; induced a classic deadlock.
  ;;
  ;; You need to surround that action with WITHOUT-ACTOR-STATUS so
  ;; that embedded =CONT calls will become identity operations instead
  ;; of setups to send continuation messages back to the Actor.
  ;;
  ;; The Actor will be hung waiting on the resource, so there is no
  ;; danger of multiple thread access to Actor internals, until the
  ;; resource is released, if code in callback functions access Actor
  ;; internals from a foreign thread prior to that release. When in
  ;; doubt, use a lock.
  ;;
  `(let ((*current-actor* nil))
     ,@body))

;; ------------------------------------------
;; Sends directed to mailboxes, functions, etc.

#+:LISPWORKS
(defmethod send ((mbox mp:mailbox) &rest message)
  (mp:mailbox-send mbox message))

#+:ALLEGRO
(defmethod send ((mbox mp:queue) &rest message)
  (mpcompat:mailbox-send mbox message))

(defmethod send ((mbox prio-mailbox) &rest message)
  (mailbox-send mbox message))

(defmethod send ((fn function) &rest message)
  (apply fn message))

(defmethod send ((sym symbol) &rest message)
  (if-let (actor (find-actor sym))
      (apply 'send actor message)
    (if (fboundp sym)
        (apply sym message)
      (call-next-method))))

(defmethod send ((str string) &rest message)
  (if-let (actor (find-actor str))
      (apply 'send actor message)
    (call-next-method)))

(defun funcallable-p (obj)
  (or (functionp obj)
      (and (symbolp obj)
           (fboundp obj))))

(defmethod send (other-obj &rest message)
  (let ((mfn (car message)))
    (when (funcallable-p mfn)
      (apply mfn other-obj (cdr message))
      )))

;; ------------------------------------------
;; A mailbox repository...
;; ... some things just can't be turned into an Actor service...

#+:LISPWORKS
(let ((queue (list nil)))

  (defun get-mailbox ()
    (or (sys:atomic-pop (car queue))
        (mp:make-mailbox)))

  (defun release-mailbox (mbox)
    (sys:atomic-push mbox (car queue))))

#+:ALLEGRO
(let ((queue (list nil))
      (lock  (mpcompat:make-lock)))

  (defun get-mailbox ()
    (mpcompat:with-lock (lock)
       (or (pop (car queue))
           (mpcompat:make-mailbox))))

  (defun release-mailbox (mbox)
    (mpcompat:with-lock (lock)
       (push mbox (car queue)))))
  
(defun do-with-borrowed-mailbox (fn)
  (let ((mbox (get-mailbox)))
    (unwind-protect
        (funcall fn mbox)
      (release-mailbox mbox))))

(defmacro with-borrowed-mailbox ((mbox) &body body)
  `(do-with-borrowed-mailbox
    (lambda (,mbox)
      ,@body)))

#+:LISPWORKS
(editor:setup-indent "with-borrowed-mailbox" 1)

;; ----------------------------------------------
;; ASK - RPC with an Actor. Any errors incurred during the message
;; handling are reflected back to the caller

(defmethod ask ((actor actor) &rest message)
  ;; Blocking synchronous ASK with mailbox
  (if (eq actor (current-actor))
      (apply 'dispatch-message actor message)
    ;; else - asking someone else
    (apply 'recover-ans-or-exn
           ;; return through mailbox is via SEND which always produces a
           ;; list. Hence the APPLY in the line above.
           (with-borrowed-mailbox (mbox)
             (apply 'send actor :ask-{061B3878-CD81-11E7-9B0D-985AEBDA9C2A} mbox message)
             (mpcompat:mailbox-read mbox)
             ))))

;; ----------------------------------------
;; ASK RPC directed to functions etc.

(defun huh!? ()
  (error "Huh!?"))

(defmethod ask (obj &rest message)
  (let ((mfn (car message)))
    (if (funcallable-p mfn)
        (apply mfn obj (cdr message))
      (Huh!?))))

(defmethod ask ((fn function) &rest message)
  (apply fn message))

(defmethod ask ((sym symbol) &rest message)
  (if-let (actor (find-actor sym))
      (apply 'ask actor message)
    (if (fboundp sym)
        (apply sym message)
      (call-next-method))))

(defmethod ask ((str string) &rest message)
  (if-let (actor (find-actor str))
      (apply 'ask actor message)
    (call-next-method)))

;; ---------------------------------------------
;; SPAWN a new Actor on a function with args
;; This is the way we start all Actors in the system.

(defun spawn (fn &rest args)
  (let ((actor (make-actor fn)))
    (apply 'send actor args)
    actor))

;; --------------------------------------------------------------------
;; Executive Pool - actual system threads dedicated to running Actor code

(defvar *heartbeat-timer*    nil) ;; the system watchdog timer
(defvar *last-heartbeat*     0)   ;; time of last Executive activity
(defvar *executive-counter*  0)   ;; just a serial number on Executive threads
(defvar *heartbeat-interval* 1)   ;; how often the watchdog should check for system stall
(defvar *maximum-age*        3)   ;; how long before watchdog should bark
#|
(defvar *nbr-execs*               ;; should match the number of CPU Cores
  #+(AND :LISPWORKS :MACOSX)
  (load-time-value
   (with-open-stream (s (sys:open-pipe "sysctl -n hw.logicalcpu"))
     (let ((ans (ignore-errors (parse-integer (read-line s nil nil)))))
       (or (and (integerp ans)
                ans)
           4)))
   t)
  #-(AND :LISPWORKS :MACOSX) 4)
|#
(defvar *nbr-execs*   16)            ;; for now while we are testing...

;; ----------------------------------------------------------------
;; Ready Queue

(defconstant +wait-property+ 'waiting-for-actor)

#+:LISPWORKS
(defun pop-ready-queue ()
  ;; while awaiting a function to perform from the Actor ready queue,
  ;; we indicate our waiting with a process property that can be
  ;; queried by the system watchdog timer.
  (prog2
      (setf (mp:process-property +wait-property+) t)
      (mailbox-read *actor-ready-queue*)
    (setf (mp:process-property +wait-property+) nil
          *last-heartbeat* (get-universal-time))))

#+:LISPWORKS
(defun waiting-for-actor-p (proc)
  (mp:process-property +wait-property+ proc))

#+:ALLEGRO
(defun pop-ready-queue ()
  ;; while awaiting a function to perform from the Actor ready queue,
  ;; we indicate our waiting with a process property that can be
  ;; queried by the system watchdog timer.
  (let* ((proc  (mpcompat:current-process)))
    (setf (getf (mp:process-property-list proc) +wait-property+) t)
    (let ((ans (mailbox-read *actor-ready-queue*)))
      (setf (getf (mp:process-property-list proc) +wait-property+) nil
            *last-heartbeat* (get-universal-time))
      ans)))

#+:ALLEGRO
(defun waiting-for-actor-p (proc)
  (getf (mp:process-property-list proc) +wait-property+))

;; ------------------------------------------------------------
;; Executive Actions

(defun executive-loop ()
  ;; the main executive loop
  (unwind-protect
      (loop for fn = (pop-ready-queue)
            do
            (restart-case
                (funcall fn)
              (:terminate-actor ()
                :report "Terminate Actor"
                )))
    (remove-from-pool (mpcompat:current-process))))

(defun exec-terminate-actor (actor)
  ;; an interrupt handler - if the actor is ours, we terminate it
  (when (eq actor (current-actor))
    (throw :terminate-actor nil)))

;; --------------------------------------------------------------

(defun ready-queue-empty-p ()
  (mailbox-empty-p *actor-ready-queue*))

(defun empty-ready-queue ()
  ;; this is a support routine, to be called only from the safe
  ;; Monitor section. So, only one of us is mutating
  ;; *ACTOR-READY-QUEUE*, the pointer.
  (let ((old-mb  (shiftf *actor-ready-queue*
                         (make-prio-mailbox))))
    ;; nudge any Executives waiting on the queue to move over to the
    ;; new one.
    (mapc (lambda (proc)
            (declare (ignore proc))
            (mailbox-send old-mb 'do-nothing))
          *executive-processes*)
    ))

#+:ALLEGRO
(defun #1=allegro-check-sufficient-execs ()
  (loop
    (sleep *heartbeat-interval*)
    (when (check-sufficient-execs)
      (return-from #1#))))

#|
(defun test-stall ()
  (loop repeat (1+ *nbr-execs*) do 
	(spawn (lambda () 
		 (sleep 10) 
		 (pr :hello (current-actor)))
	       )))
|#

(defmonitor
    ;; All under a global lock
    ((terminate-actor (actor)
       (dolist (exec *executive-processes*)
         (mp:process-interrupt exec 'exec-terminate-actor actor)))
     
     (check-sufficient-execs ()
       (let (age)
         (unless (or (null *executive-processes*) ;; not running the Actor system?
		     (ready-queue-empty-p)        ;; nothing to do anyway?
                     (find-waiting-executive)     ;; an available Executive thread?
                     (< (setf age (- (get-universal-time) ;; been stalled long enough?
				     *last-heartbeat*))
			*maximum-age*))
           ;; -------------------------------------------
           ;;
           ;; Why kill the workhorse?
           ;;
           ;; For LW, the timer routine triggers in an arbitrary
           ;; thread with a retriggering timer. This routine runs as
           ;; an interrupt routine and we need to keep it short. We
           ;; also need to prevent retriggering of nuisance
           ;; notifications while we are busy handling the situation.
           ;;
           ;; For ACL, the timer runs in its own dedicated thread and
           ;; won't retrigger until we return from here. But we also
           ;; need to keep this short so that we don't block ongoing
           ;; useful activity that may need something inside this
           ;; monitor section.
           ;;
           ;; So in both cases, just kill off the timer and let a new
           ;; thread handle the notification with the user.
           ;; ----------------------------------------------
	   #+:LISPWORKS
           (mp:unschedule-timer (shiftf *heartbeat-timer* nil))
	   #+:ALLEGRO
	   (setf *heartbeat-timer* nil)
           ;; --------------------------------------------
	   
           (mp:process-run-function
            "Handle Stalling Actors"
            #+:LISPWORKS ()
            (lambda ()
              (restart-case
                  (error "Actor Executives are stalled (blocked waiting or compute bound). ~&Last heartbeat was ~A sec ago."
                         age)
                (:do-nothing-just-wait ()
                  :report "It's okay, just wait"
                  (start-watchdog-timer))
                (:spawn-new-executive ()
                  :report "Spawn another Executive"
                  (push-new-executive))
                (:stop-actor-system ()
                  :report "Stop Actor system"
                  (kill-executives))
                ))
            ))))

     (find-waiting-executive ()
       (some 'waiting-for-actor-p *executive-processes*))

     (remove-from-pool (proc)
       (setf *executive-processes* (delete proc *executive-processes*)))
     
     (push-new-executive ()
       (push (mp:process-run-function
              (format nil "Actor Executive ~D" (incf *executive-counter*))
              #+:LISPWORKS '()
              'executive-loop)
             *executive-processes*)
       (start-watchdog-timer))

     #+:LISPWORKS
     (start-watchdog-timer ()
       (unless *heartbeat-timer*
         (setf *heartbeat-timer*
               (mp:make-timer 'check-sufficient-execs))
         (mp:schedule-timer-relative
          *heartbeat-timer*
          *heartbeat-interval*
          *heartbeat-interval*)))

     #+:ALLEGRO
     (start-watchdog-timer ()
       (unless *heartbeat-timer*
         (setf *heartbeat-timer*
               (mp:process-run-function "Heartbeat Timer"
                                        'allegro-check-sufficient-execs))))

     (ensure-executives ()
       (unless *executive-processes*
         (dotimes (ix *nbr-execs*)
           (push-new-executive))))
     
     (kill-executives ()
       (let ((timer (shiftf *heartbeat-timer* nil)))
         (when timer
           #+:LISPWORKS
           (mp:unschedule-timer timer)
           #+:ALLEGRO
           (mp:process-kill timer)
           (setf *last-heartbeat* 0)))
       (let ((procs (shiftf *executive-processes* nil)))
         (setf *executive-counter* 0)
         (dolist (proc procs)
           (ignore-errors
             #+:LISPWORKS
             (mp:process-terminate proc)
             #+:ALLEGRO
             (mp:process-kill proc)))
         (empty-ready-queue)
         ))))

;; -----------------------------------------------------------------------------
;; Not specifically a part of Actors, but might help when describing
;; non-blocking Actor code...
;;
;; Be careful here... the use of parallel execution could lead to
;; shared access to local state, which might violate single-thread
;; semantics inside of Actor bodies.
;;
;; These macros also game the system by intentional capture of free
;; variable %SK (the current continuation). This is a lexical binding,
;; and must not be made a global special symbol.

(defmacro =lambda (parms &body body)
  `#'(lambda (%sk ,@parms) ,@body))

(defmacro =defun (name parms &body body)
  (let* ((f            (symb '= name))
         (has-rest     (position '&rest parms))
         (prefix-parms (subseq parms 0 has-rest))
         (tail-parm    (when has-rest
                         (nthcdr (1+ has-rest) parms))))
    ;; ooftah...
    `(progn
       (defmacro ,name ,parms
         `(,',f %sk ,,@prefix-parms ,@,@tail-parm))
       (defun ,f (%sk ,@parms) ,@body))))

(defmacro =bind (parms expr &body body)
  `(let ((%sk (=cont #'(lambda ,parms ,@body))))
     ,expr))

(defmacro =values (&rest retvals)
  `(funcall %sk ,@retvals))

(defmacro =funcall (fn &rest args)
  `(funcall ,fn %sk ,@args))

(defmacro =apply (fn &rest args)
  `(apply ,fn %sk ,@args))

(defmacro with-cont (&body body)
  ;; for REPL toplevel call to function defined with =defun
  `(let ((%sk #'values))
     ,@body))

(defmacro with-future ((ans) form &body body)
  ;; alternative for Actors and Processes alike
  `(=bind (,ans)
       (spawn (lambda ()
                (=values ,form)))
     ,@body))

(define-symbol-macro =bind-callback %sk)

#+:LISPWORKS
(editor:setup-indent "with-future"  2)

;; --------------------------------------------------
;; Async ASK

(defmacro aska (args (obj &rest message) &body body)
  ;; Non-blocking asynchronous ASK with callback
  `(do-aska ,obj ,message (lambda ,args
                            ,@body)))

(defmethod do-aska ((obj actor) message cbfn)
  (if (eq obj (current-actor))
      (funcall cbfn (apply 'dispatch-message obj message))
    ;; else
    (apply 'send obj :ask-{061B3878-CD81-11E7-9B0D-985AEBDA9C2A}
           (=cont (lambda (ans)
                    (funcall cbfn (apply 'recover-ans-or-exn ans))))
           message)))

(defmethod do-aska (obj message cbfn)
  (with-future (ans)
      (apply 'ask obj message)
    (funcall cbfn ans)))
  
;; ------------------------------------------------

(defun par (cbfn fns)
  (let* ((cbfn  (=cont cbfn))
         (len   (length fns))
         (count (list len))
         (ansv  (make-array len)))
    (labels ((done (ix ans)
               (setf (aref ansv ix) ans)
               (when (zerop (mpcompat:atomic-decf (car (the cons count))))
                 (apply cbfn (coerce ansv 'list)))))
      (if fns
          (loop for fn in fns
                for ix from 0
                do
                (spawn (lambda (n fn)
                         (done n (funcall fn)))
                       ix fn))
        ;; else - nothing to do
        (funcall cbfn)))
    ))

(defmacro with-futures (args forms &body body)
  ;; Be careful here... Actors are generally incompatible with
  ;; parallel concurrent access to their internal state. But if you
  ;; know what you are doing...
  `(par
     (lambda ,args
       ,@body)
     (list ,@(mapcar #`(lambda () ,a1) forms)) ))

#+:LISPWORKS
(editor:setup-indent "with-futures" 2)

;; ----------------------------------------------------

(defun trn (mat)
  (apply 'mapcar 'list mat))

(defun pmapcar (cbfn fn &rest lists)
  ;; Parallel mapcar - calls cbfn with a result list.
  ;; Use like PAR for indefinite number of parallel forms,
  ;; each of which is the same function against different args.
  (let* ((cbfn   (=cont cbfn))
         (grps   (trn lists))
         (len    (length grps))
         (count  (list len))
         (ansv   (make-array len)))
    (labels ((done (ix ans)
               (setf (aref ansv ix) ans)
               (when (zerop (mpcompat:atomic-decf (car (the cons count))))
                 (funcall cbfn (coerce ansv 'list)))))
      (if grps
          (loop for grp in grps
                for ix from 0
                do
                (spawn (lambda (n grp)
                         (done n (apply fn grp)))
                       ix grp))
        ;; else - empty lists, nothing to do
        (funcall cbfn nil)))
    ))

(defmacro =pmapcar (fn &rest lists)
  `(pmapcar %sk ,fn ,@lists))
       
#+:LISPWORKS
(editor:setup-indent "=pmapcar" 2)

#| ;; for example - query a bunch of remote nodes
(=bind (lst)
    (=pmapcar (lambda (node)
                (send node :query))
        nodes)
  (do-something-with-result lst))
|#

;; -----------------------------------------------------------------------

(=defun apar-map (node-maker-fn &rest lists)
  ;;
  ;; APAR-MAP - an async par-map -- works just like mapcar, but in
  ;; parallel async, suitable for =bind.
  ;;
  ;; The node-maker-fn should take a continuation arg and args
  ;; selected from each list, as if defined by =defun or =lambda.
  ;;
  ;; But rather than passing the =defun name, you *must* use the =name
  ;; as it is an actual function. The node-maker-fn should return a
  ;; closure that will be spawned to perform the actual work.
  ;;
  ;; The worker function, in the returned closure, should return by
  ;; =values.
  ;;
  ;; The final returned value from APAR-MAP is a list returned to its
  ;; caller via =values. This is a different callback from the ones
  ;; fabricated for each worker node, and used for their =values.
  ;;
  ;;   (node = Actor, since node is a spawned function).
  ;;
  (let ((grps (trn lists)))
    (if grps
        (let* ((len   (length grps))
               (count (list len))
               (ansv  (make-array len)))
          (labels ((done (ix ans)
                     (setf (aref ansv ix) ans)
                     (when (zerop (mpcompat:atomic-decf (car (the cons count))))
                       (=values (coerce ansv 'list))))
                   (callback (ix)
                     (lambda (ans)
                       (done ix ans))))
            (loop for grp in grps
                  for ix from 0
                  do
                  (spawn (apply node-maker-fn (callback ix) grp))
                  )))
      ;; else - no nodes
      (=values nil)
      )))

#|
  ;; example...
(=bind (lst)
    (apar-map (=lambda (x y)
                (lambda ()
                  (=values (list x y))))
              '(a b c)
              '(1 2 3))
  (print lst))
==> '((a 1) (b 2) (c 3))
  |#
