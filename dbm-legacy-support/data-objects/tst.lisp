
(defun tst (n)
  (labels ((trunc (v)
             (logand (lognot 1)
                     (1+ (truncate v 2))))

           (rnd (v)
             (ash (round v 4) 1)))
    (list (trunc n) (rnd n))))
