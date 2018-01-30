
(in-package :cl-user)

(defun make-stats-counter
       (&key (count 0)
             (sum   0)
             (ssum  0))
  (um:plambda (n) (sum count ssum)
    (incf ssum (* n n))
    (incf sum  n)
    (incf count)))

(um:defpan stats-counter-mean (sum count)
  (/ sum count))

(um:defpan stats-counter-variance (ssum sum count)
  (if (< count 2)
      0
    (/ (- ssum
          (* sum (stats-counter-mean self)))
       (- count 1))))

(um:defpan stats-counter-stddev ()
  (sqrt (stats-counter-variance self)))
