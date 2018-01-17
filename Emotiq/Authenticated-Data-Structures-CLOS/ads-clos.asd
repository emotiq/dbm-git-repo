;; ads.asd -- ASD File for Authenticated Data Structures
;;
;; DM/Emotiq  01/18
;; -----------------------------------------------------------

(in-package :user)

(asdf:defsystem "ads-clos"
  :description "ADS-CLOS: CLOS-Based Authenticated Data Structures"
  :version     "1.0"
  :author      "D.McClain <dbm@emotiq.ch>"
  :license     "Copyright (c) 2018 by Emotiq, A.G. License terms apply."
  :components  ((:file "packages")
                (:file "utilities")
                (:file "basic-boxing")
                (:file "hash")
                (:file "ads")
                (:file "auth-merkle-tree")
                (:file "auth-vector")
                (:file "auth-list")
                )
  :SERIAL T
  :depends-on   (:ironclad
                 :lisp-object-encoder
                 :useful-macros))

