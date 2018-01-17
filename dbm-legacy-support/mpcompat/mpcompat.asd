
(asdf:defsystem "mpcompat"
  :description "mpcompat: a compatibility API for multiprocessing"
  :version     "1.0"
  :author      "D.McClain <dbm@spectrodynamics.com>"
  :license     "Copyright (c) 2010 by SpectroDynamics, LLC. All rights reserved."
  :components  ((:file "packages")
                #+:LISPWORKS  (:file "mp-compat-lw")
                #+:LISPWORKS5 (:file "mp-compat-lw5")
                #+(OR :LISPWORKS6 :LISPWORKS7) (:file "mp-compat-lw6")
                #+:ALLEGRO    (:file "mp-compat-allegro")
                #+:SBCL       (:file "mp-compat-sbcl")
                #+:CLOZURE    (:file "mp-compat-clozure")
                ;; (:file "mp-compat-critical")
                ;; (:file "multi-locks")
                )
  :serial t
  :depends-on   ("useful-macros"))
