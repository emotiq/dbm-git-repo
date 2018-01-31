;; packages.lisp - Actors packages
;;
;; DM/RAL  12/17
;; -------------------------------------------------------------------

(in-package :CL-USER)

#+:ALLEGRO
(defpackage :allegro-timer
  (:nicknames :atimer)
  (:use :common-lisp)
  (:export
   :timer
   :make-timer
   :schedule-timer
   :schedule-timer-relative
   :unschedule-timer
   ))

#+:CLOZURE
(defpackage :clozure-timer
  (:nicknames :ctimer)
  (:use :common-lisp)
  (:export
   :timer
   :make-timer
   :schedule-timer
   :schedule-timer-relative
   :unschedule-timer
   ))

(defpackage #:actors
  (:use #:common-lisp)
  (:nicknames #:ac)
  (:import-from :mpcompat
   :make-lock
   :with-lock
   :CAS)
  (:import-from #:priq
   #:prio-mailbox
   #:make-prio-mailbox
   #:mailbox-read
   #:mailbox-send
   #:mailbox-empty-p
   #:make-unsafe-fifo
   #:make-priq
   #:addq
   #:popq
   #:emptyq-p
   #:contents)
  (:import-from #:trivia
   #:match
   #:ematch
   #:guard
   #:lambda-match
   #:lambda-ematch)
  (:import-from #:useful-macros
   #:curry
   #:rcurry
   #:if-let
   #:when-let
   #:foreach
   #:nlet-tail
   #:dlambda
   #:dcase
   #:defmonitor
   #:capture-ans-or-exn
   #:recover-ans-or-exn
   #:<shared-plist>
   #:get-kv
   #:symb)
  #+:LISPWORKS
  (:import-from :mp
   :make-timer
   :schedule-timer-relative
   :unschedule-timer)
  #+:ALLEGRO
  (:import-from :atimer
   :make-timer
   :schedule-timer-relative
   :unschedule-timer)
  #+:CLOZURE
  (:import-from :ctimer
   :make-timer
   :schedule-timer-relative
   :unschedule-timer)
  (:export
   #:*nbr-execs*
   #:actor
   #:make-actor
   #:send
   #:ask
   #:aska
   #:current-actor
   #:spawn
   #:get-property
   #:set-property
   #:suspend
   #:become
   #:dispatch-message
   #:self-call
   #:register-actor
   #:unregister-actor
   #:pr
   #:find-actor
   #:get-actors
   #:recv
   #:=cont
   #:terminate-actor
   #:without-actor-status
   #:make-proxy

   #:=lambda
   #:=defun
   #:=bind
   #:=bind-callback
   #:=values
   #:=funcall
   #:=apply
   #:with-cont
   #:with-future
   #:par
   #:with-futures
   #:pmapcar
   #:=pmapcar
   #:apar-map
   
   #:with-borrowed-mailbox
   #:do-nothing
   ))

(defpackage #:linda
  (:use #:common-lisp #:ac)
  (:import-from #:um
   #:if-let
   #:when-let
   #:foreach
   #:nlet
   #:nlet-tail
   #:group
   #:dlambda
   #:curry
   #:rcurry
   #:defmacro!
   #:accum)
  (:export
   #:*linda*
   #:make-ts
   #:out
   #:rd
   #:in
   #:rdp
   #:inp
   #:outb
   #:rdb
   #:rdbp
   #:remove-bindings
   #:on-in
   #:on-inp
   #:on-rd
   #:on-rdp
   #:on-rdb
   #:on-rdbp
   #:remove-all-bindings
   #:remove-all-tuples
   #:reset
   #:srdp
   #:sinp
   #:srdbp
   #:remove-tuples
   #:remote-srdp
   #:remote-sinp
   #:remote-out
   #:remote-outb
   #:remote-srdbp
   #:remote-remove-bindings
   ))

