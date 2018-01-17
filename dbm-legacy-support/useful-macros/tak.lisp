
(in-package :um)

(defun tak (x y z)
  (cond ((not (< y x)) z)
        (t    (tak (tak (1- x) y z)
                   (tak (1- y) z x)
                   (tak (1- z) x y)))
        ))