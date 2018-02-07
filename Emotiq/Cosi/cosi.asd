;; ads.asd -- ASD File for Authenticated Data Structures
;;
;; DM/Emotiq  01/18
;; -----------------------------------------------------------

(in-package :cl-user)

#+:ALLEGRO (require :sock)

(asdf:defsystem "cosi"
  :description "Cosi: Authenticated multi-signatures in Lisp"
  :version     "1.0"
  :author      "D.McClain <dbm@emotiq.ch>"
  :license     "Copyright (c) 2018 by Emotiq, A.G. License terms apply."
  :components  (
		(:file "cosi-async")
		;; (:file "cosi")
		;; (:file "tst-cas")
                ;; (:file "cosi-socket")
                (:file :random-partition)
                )
  :SERIAL T
  :depends-on   (:ironclad
		 :actors
                 :core-crypto
                 :lisp-object-encoder
                 :useful-macros
		 :usocket
                 ))

