
(in-package :cl-user)

(asdf:defsystem "ecc-decrypt"
  :description "ecc-decrypt: decryption based on NIST B-571 Elliptic Curve Cryptography"
  :version     "1.0"
  :author      "D.McClain <david@acudora.com>"
  :license     "Copyright (c) 2011 by Acudora, Inc. All rights reserved."
  :components  ((:file "ctr-hmac-encryption")
                (:file "3ctr-hmac-encryption")
                (:file "decrypt-library"))
  :serial       t
  :depends-on   ("ironclad"
                 "useful-macros"
                 "lisp-object-encoder"
                 "s-base64"
                 "core-crypto"
                 ))

