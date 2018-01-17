
(inspect
 (um:filter (lambda (pack)
              (let ((syms (package-shadowing-symbols pack)))
                (when syms
                  (list (package-name pack) syms))))
            (list-all-packages)))

(pprint (package-name :okeanos))


(um:dlambda
  (:a (one two) (doit one two))
  (:b (three)  (doit2 three))
  (t (&rest args) (list args)))
