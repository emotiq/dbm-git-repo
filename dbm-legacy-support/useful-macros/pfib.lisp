#|
(defparameter *fibs* (make-array 33))

(defun direct-fib (n)
  (if (< n 2)
      n
    (um:nlet-tail iter ((f1 0)
                        (f2 1)
                        (ix 2))
      (if (> ix n)
          f2
        (iter f2 (+ f1 f2) (1+ ix)))
      )))

(defun basic-fib (n)
  (if (< n 2)
      n
    (let* ((x (basic-fib (- n 2)))
           (y (basic-fib (1- n))))
      (+ x y))
    ))

(defun fib (n)
  (labels ((basic-fib (n)
             (let* ((x (fib (- n 2)))
                    (y (fib (1- n))))
               (+ x y))))
    (cond ((< n 2) n)
          ((< n 33)
           (or (aref *fibs* n)
               (setf (aref *fibs* n) (basic-fib n))))
          (t (basic-fib n))
          )))

(defun pfib (n)
  (labels ((%pfib (n)
             (if (< n 33)
                 (constantly (basic-fib n))
               (let* ((x (unsafe-future (%pfib (1- n))))
                      (y (%pfib (- n 2))))
                 (lambda ()
                   (+ (funcall (touch x)) (funcall y))))
               )))
    (funcall (%pfib n))
    ))

  |#

