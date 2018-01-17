
(in-package :fac)

(let ((actor (make-actor
              (let ((count 0))
                (dlambda
                  (:test (n)
                   (setf count n)
                   (um:nlet iter ()
                     (with-future ()
                         (sleep 1)
                       (pr :count count)
                       (when (plusp (decf count))
                         (iter))))))))
             ))
  (spawn actor :test 5))

(spawn (let ((count 0))
         (dlambda
           (:test (n)
            (setf count n)
            (um:nlet iter ()
              (with-future ()
                  (sleep 1)
                (pr :count count)
                (when (plusp (decf count))
                  (iter)))))))
       :test 5)

(let ((x (future #'one))
      (y (future #'two)))
  (when-all (list x y)
            body))

(progn
  (with-futures (&rest answers)
      ((progn
         (sleep 1) :ok-1)
       (progn
         (sleep 2) :ok-2)
       (progn
         (sleep 3) :ok-3))
    (pr answers))
  (pr :after-after))

(with-futures (x y z)
    ((sin 0.1)
     (sin 0.2)
     (sin 0.3))
  (pr (list x y z)))

(funcall (dlambda
           (:one () 1)
           (:two () 2)
           (t (&rest args)
              (break)
              args)))
              
(defun tst (&optional (n #N1_000))
  (time
   (loop repeat n do
         (spawn (lambda ()
                  nil)))))

(defun tst2 (&optional (N #N1_000))
  (time
   (loop repeat n do
         (mp:process-run-function "test" () (lambda ()
                                              nil)))))