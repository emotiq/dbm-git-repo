
(in-package #:user)

(asdf:defsystem "ecc-change-passwd"
  :description "ecc-change-passwd: password management for ECC"
  :version     "1.0"
  :author      "D.McClain <david@acudora.com>"
  :license     "Copyright (c) 2011 by Acudora, Inc. All rights reserved."
  :components  ((:file "change-passwd-intf"))
  :serial       t
  :depends-on   ("ecc-keying"))

