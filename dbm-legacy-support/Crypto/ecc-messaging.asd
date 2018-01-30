
(in-package :cl-user)

(asdf:defsystem "ecc-messaging"
  :description "ecc-messaging: ECC encryption / decryption for messaging"
  :version     "1.0"
  :author      "D.McClain <david@acudora.com>"
  :license     "Copyright (c) 2011 by Acudora, Inc. All rights reserved."
  :components  ((:file "messaging-intf"))
  :serial       t
  :depends-on   ("ecc-keying"))


