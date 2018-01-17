
(in-package #:user)

(asdf:defsystem "ecc-ibe"
  :description "ecc-ibe: ECC Identity Based Encryption / Decryption for Messaging"
  :version     "1.0"
  :author      "D.McClain <david@acudora.com>"
  :license     "Copyright (c) 2011 by Acudora, Inc. All rights reserved."
  :components  ((:file "primes")
                (:file "bilinear-mappings"))
  :serial       t
  :depends-on   ("ecc-keying"))


