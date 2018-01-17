
(in-package #:user)

(asdf:defsystem "ecc-keying"
  :description "ecc-keying: encryption based on NIST B-571 Elliptic Curve Cryptography"
  :version     "1.0"
  :author      "D.McClain <david@acudora.com>"
  :license     "Copyright (c) 2011 by Acudora, Inc. All rights reserved."
  :components  ((:file "passwds")
                (:file "ecc-keys")
                (:file "encryption")
                (:file "gfc-encryption")
                (:file "ctr-hmac-encryption")
                (:file "3ctr-hmac-encryption"))
  :serial       t
  :depends-on   ("ironclad"
                 "useful-macros"
                 "lisp-object-encoder"
                 "s-base64"
                 ;; "plotter"
                 ;; "data-objects"
                 "core-crypto"
                 ))

