
(asdf:defsystem "useful-macros"
  :description "useful-macros: a collection of widely useful macros and functions"
  :version     "1.0"
  :author      "D.McClain <dbm@refined-audiometrics.com>"
  :license     "Copyright (c) 2008 by Refined Audiometrics Laboratory, LLC. All rights reserved."
  :components  (#+:CLOZURE (:file "clozure-compat")
  	        #+:SBCL    (:file "sbcl-compat")
		#+:ALLEGRO (:file "allegro-compat")
                (:file "packages")
		(:file "comprehensions")
                #+:LISPWORKS (:file "lexb4")
                ;; (:file "freev")
                #+:LISPWORKS (:file "safe-call-system")
                (:file "hierarchical-packages")
                (:file "package-aliases")
                (:file "basic-useful")
                (:file "sharp-quasiquote-reader")
                (:file "bang-macros")
                (:file "ppcre-reader")
                (:file "reader-macros")
                (:file "safe-read-from-string")
                #+:LISPWORKS (:file "ctypes")
                (:file "dlambder")
                (:file "bb")
                (:file "useful-macros")
                ;; (:file "scraps")
                (:file "pandoric")
                (:file "dflet")
                (:file "typed-fun")
                ;; (:file "monads")
                (:file "critical-section")
                ;; (:file "dispatch-queues") ;; what do we need these for?
                ;; (:file "lazy") ;; supplanted by Actors
                #+:LISPWORKS (:file "remembered-filenames")
                ;; (:file "useful-macros-old")
                ;; (:file "match-macro")

                ;; these match-macro-ex were the ones in use before optima
                ;; (:file "match-macro-ex")
                ;; (:file "match-macro-ex-opt")

                ;; (:file "match-macro-ex3")
                ;; (:file "monitor-macros")
                (:file "memoize")
                #-:ALLEGRO (:file "cache")
                #+:WIN32 (:file "exec")
                ;; (:file "lazy") ;; supplanted by a better, simpler, version
                (:file "engfmt")
		(:file "usec")
               	(:file "uuid")
                (:file "computed-metaclass")
                #+(AND :LISPWORKS :MACOSX) (:file "OSX-UUID-Generate")
                #+(AND :ALLEGRO :MACOSX)   (:file "OSX-UUID-Generate-Allegro")
                ;; (:file "xfli")
		;; (:file "rubber-objects")
                )
  :serial       t
  :depends-on   (#| "compiled-ml-matcher" |#
                 "optima"
                 "cl-ppcre"
                 "ironclad"))

