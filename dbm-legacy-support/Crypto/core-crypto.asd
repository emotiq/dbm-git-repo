
(in-package #:user)

(asdf:defsystem "core-crypto"
  :description "core-crypto: core cryptography functions"
  :version     "1.0"
  :author      "D.McClain <dbm@refined-audiometrics.com>"
  :license     "Copyright (c) 2015 by Refined Audiometrics Laboratory, LLC. All rights reserved."
  :components  ((:file "ecc-package")
                (:file "utilities")
                (:file "ctr-hash-drbg")
                (:file "primes")
                (:file "crypto-le")
                (:file "kdf")
                (:file "gf-571")
                (:file "mod-math")
                (:file "edwards")
                (:file "ecc-B571")
                (:file "curve-gen")
                (:file "crypto-environ")
                (:file "machine-id"))
  :serial       t
  :depends-on   ("ironclad"
                 "useful-macros"
                 "lisp-object-encoder"
                 "s-base64"
                 "sha3"
                 ))

