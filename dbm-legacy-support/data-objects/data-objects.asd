
(asdf:defsystem "data-objects"
  :description "data-objects: a collection of widely useful data types"
  :version     "1.0"
  :author      "D.McClain <dbm@refined-audiometrics.com>"
  :license     "Copyright (c) 2008 by Refined Audiometrics Laboratory, LLC. All rights reserved."
  :components  ((:file "packages")
                (:file "ref")
                (:file "prio-queue")
                (:file "locked-resource")
                (:file "resource")
                (:file "ord")
                (:file "bal-binary-trees")
                (:file "bal-binary-tree-maps")
                ;; (:file "mi-rubber-objects-2")
		(:file "rubber-objects-maps")
                #+:LISPWORKS (:file "fstm")
                #+:LISPWORKS (:file "mcas")
                #+:LISPWORKS (:file "lf-bag")
                #+:LISPWORKS (:file "lw-rwgate")
                #+:LISPWORKS (:file "progress-bar")
                #+:LISPWORKS (:file "debug-stream")
                #+:LISPWORKS (:file "collector2")
                (:file "interval-trees")
                (:file "zorder-maps")
                (:file "rpsx")
                (:file "btree-clos")
                (:file "memory-btrees-clos")
		#+:LISPWORKS (:file "protocols")
                )
  :serial t
  :depends-on   ("useful-macros"
                 "mpcompat"
                 "optima"
                 "trivia"
                 ))


