;;; -*- Mode: LISP; Syntax: COMMON-LISP -*-

(defsystem :dbm
  :components ((:file "load")))

(defsystem :dbm/cosi
  :depends-on (cosi))

(defsystem :dbm/ads-clos
  :depends-on (ads-clos))


