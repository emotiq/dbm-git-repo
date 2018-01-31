
(in-package :cl-user)

(asdf:defsystem "alt-decompressor"
  :description "alt-decompressor: decryption based on NIST B-571 Elliptic Curve Cryptography"
  :version     "1.0"
  :author      "D.McClain <david@acudora.com>"
  :license     "Copyright (c) 2011 by Acudora, Inc. All rights reserved."
  :components  ((:file "alt-decompressor"))
  :serial       t
  :depends-on   ())

