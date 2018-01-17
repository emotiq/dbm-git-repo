(in-package #:user)

(asdf:defsystem "save-shares"
  :description "save-shares: save supplied user licenses to shares files"
  :version     "1.0"
  :author      "D.McClain <david@acudora.com>"
  :license     "Copyright (c) 2012 by Acudora, Inc. All rights reserved."
  :components  ((:file "save-shares"))
  :serial       t
  :depends-on   ("useful-macros"
                 "lisp-object-encoder"
                 "s-base64"
                 "core-crypto"
                 ))

