;; lisp-object-encoder.asd
;; --------------------------------------------------------------------------------------
;; Portable Lisp Object Encoding / Decoding for Network Transport
;;
;; Copyright (C) 2008 by SpectroDynamics, LLC. All rights reserved.
;;
;; DM/SD  08/08
;; --------------------------------------------------------------------------------------

(asdf:defsystem "lisp-object-encoder"
  :description "Lisp-Object-Encoder: Network portable encoding / decoding of Lisp objects"
  :version     "1.0"
  :author      "D.McClain <dbm@spectrodynamics.com>"
  :license     "Copyright (c) 2008 by SpectroDynamics, LLC. All rights reserved."
  :components  ((:file "managed-buffers")
		(:file "ubyte-streams")
                (:file "lisp-object-encoder")
                (:file "persistent-store")
		(:file "prevalent-metaclass")
                (:file "prevalent-objects"))

  :SERIAL T
  :depends-on   ("data-objects"
                 "ironclad"
                 "sdle-store"))

