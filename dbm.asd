;;; -*- Mode: LISP; Syntax: COMMON-LISP -*-

(defsystem "dbm"
  :version "0.0.1"
  :author "Copyright (c) 2018 Emotiq AG"
  :licence "<https://opensource.org/licenses/MIT>"
  :depends-on (dbm/load))

(defsystem "dbm/load"
  :version "0.0.1"
  :author "Copyright (c) 2018 Emotiq AG"
  :licence "<https://opensource.org/licenses/MIT>"
  :components ((:file "ASDF-Starter.lisp")
               (:file "load")))

(defsystem "dbm/cosi"
  :version "0.0.1"
  :author "Copyright (c) 2018 Emotiq AG"
  :licence "<https://opensource.org/licenses/MIT>"
  :depends-on (cosi))

(defsystem "dbm/ads-clos"
  :version "0.0.1"
  :author "Copyright (c) 2018 Emotiq AG"
  :licence "<https://opensource.org/licenses/MIT>"
  :depends-on (ads-clos))


