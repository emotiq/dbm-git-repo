
(in-package :cl-user)

(asdf:defsystem "ecc-create-passwd"
  :description "ecc-create-passwd: password management for ECC"
  :version     "1.0"
  :author      "D.McClain <david@acudora.com>"
  :license     "Copyright (c) 2011 by Acudora, Inc. All rights reserved."
  :components  ((:file "create-passwd-intf"))
  :serial       t
  :depends-on   ("ecc-keying"))

