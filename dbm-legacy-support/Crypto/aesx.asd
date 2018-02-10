

(in-package :ironclad-system)

(asdf:defsystem "aesx"
  :description "aesx: extended-round AES cipher"
  :version     "1.0"
  :author      "D.McClain <dbm@refined-audiometrics.com>"
  :license     "Copyright (c) 2015 by Refined Audiometrics Laboratory, LLC. All rights reserved."
  :default-component-class ironclad-source-file
  :components  ((:file "aesx"))
  :serial       t
  :depends-on   ("ironclad"
                 ))

