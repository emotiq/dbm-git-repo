
(in-package #:user)

(asdf:defsystem "ecc-vtuning"
  :description "ecc-vtuning: encryption for VTuning installations"
  :version     "1.0"
  :author      "D.McClain <david@acudora.com>"
  :license     "Copyright (c) 2011 by Acudora, Inc. All rights reserved."
  :components  ((:file "passwds")
                (:file "ecc-keys")
                (:file "encryption")
                (:file "vtuning-crypto")
                (:file "registration"))
  :serial       t
  :depends-on   ("ironclad"
                 "useful-macros"
                 "lisp-object-encoder"
                 "s-base64"
                 "core-crypto"
                 ))


