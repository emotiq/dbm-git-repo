;; packages.lisp
;; DM/RAL  02/09
;; ------------------------------------------------------------------

(defpackage :mp-compatibility
  (:use #:common-lisp)
  (:nicknames #:mpcompat)
  #+:LISPWORKS
  (:import-from #:mp
   #:process-name
   #:process-run-function
   #:process-kill
   #:process-interrupt
   #:without-preemption
   #:process-wait
   #:process-wait-with-timeout
   #:lock-owner
   #:process-lock
   #:process-unlock
   #:with-lock
   #:make-mailbox
   #:mailbox-empty-p
   )
  #+(OR :LISPWORKS6 :LISPWORKS7)
  (:import-from #:mp
   #:mailbox-send
   #:mailbox-read
   ;; new in Lispworks 6
   #:process-property
   #:process-private-property
   #:process-poke
   #:make-lock
   #:make-condition-variable
   #:condition-variable-wait
   #:condition-variable-signal
   #:lock-owned-by-current-process-p
   #:with-sharing-lock
   #:with-exclusive-lock
   ;; in LW7
   #:process-terminate
   )
  #+(OR :LISPWORKS6 :LISPWORKS7)
  (:import-from #:sys
   #:atomic-incf
   #:atomic-decf
   #:compare-and-swap
   #:ensure-memory-after-store)
  (:export
   #:current-process
   #:process-name
   #:process-property
   #:process-private-property
   #:process-run-function
   #:process-terminate
   #:process-interrupt
   #:without-preemption
   #:process-wait
   #:process-wait-with-timeout
   #:make-lock
   #:lock-owner
   #:process-lock
   #:process-unlock
   #:with-lock
   #:with-spinlock
   #:with-locks
   #:with-spinlocks
   #:with-sharing-lock
   #:with-exclusive-lock
   #:make-mailbox
   #:mailbox-send
   #:mailbox-read
   #:mailbox-empty-p

   ;; new in Lispworks 6
   #:process-poke
   #:make-condition-variable
   #:condition-variable-wait
   #:condition-variable-signal
   #:lock-owned-by-current-process-p
   
   #:atomic-incf
   #:atomic-decf
   #:compare-and-swap
   #:ensure-memory-after-store

   #:generate-uuid
   #:critical
   #:spin-critical
   ))

