
(in-package :um)

(let ()
  (labels ((doit-1 ()
             (print "Starting doit-1")
             (sleep 3)))
    (with-serial-dispatch ()
      (doit-1))
    (print "Just launched doit-1")
    (with-serial-dispatch ()
      (doit-1))
    (print "Just launched doit-1 again...")
    (wait-for-serial-dispatch)
    (print "Finished...")
    (values)))


(let ()
  (labels ((doit-1 ()
             (print "Starting doit-1")
             (sleep 3)))
    (with-parallel-dispatch ()
      (doit-1))
    (print "Just launched doit-1")
    (with-parallel-dispatch ()
      (doit-1))
    (print "Just launched doit-1 again...")
    (wait-for-parallel-dispatch)
    (print "Finished...")
    (values)))


(defun aitken (a b c)
  (declare (double-float a b c))
  #F
  (declare (optimize #+:LISPWORKS (float 0)))
  (declare (:explain :variables :all-calls :all-calls-with-arg-types))
  (let* ((c-b (- c b))
         (den (- c-b (- b a))))
    (declare (double-float c-b den))
    (if (zerop den)
        c
      (- c (/ (* c-b c-b) den)))))
