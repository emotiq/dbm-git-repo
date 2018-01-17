
(defun get-times ()
  (let* ((t1 (usec:get-universal-time-usec))
         (t2 (bfly:!? #| "eval@10.0.1.4" |#
                      "eval@malachite.local"
                      '(usec:get-universal-time-usec)))
         (t3 (usec:get-universal-time-usec))
         ;; (t4 (bfly:!? "eval@10.0.1.4" '(usec:get-universal-time-usec)))
         ;; (t5 (usec:get-universal-time-usec))
         (t21 (- t2 t1))
         (t23 (- t2 t3)))
    (list :t2-t1 t21
          :t2-t3 t23
          :offs  (* 0.5 (+ t21 t23))
          :dly   (* 0.5 (- t21 t23)))))

(let* ((ans (loop repeat 1000 collect (get-times)))
       (offs (coerce (mapcar (lambda (rec)
                               (getf rec :offs))
                             ans)
                     'vector))
       (dlys  (coerce (mapcar (lambda (rec)
                                (getf rec :dly))
                              ans)
                      'vector))
       (omn   (vm:mean offs))
       (osd   (vm:stdev offs))
       (dmn   (vm:mean dlys))
       (dsd   (vm:stdev dlys)))
  (let* ((offs (remove-if (um:rcurry #'> (+ omn (* 3 osd))) offs)))
    (plt:histogram 'offs offs :clear t :xlog t))
  (let ((dlys  (remove-if (um:rcurry #'> (+ dmn (* 3 dsd))) dlys)))
    (plt:histogram 'dly  dlys :clear t :xlog t))
  (list :omn omn :osd osd
        :dmn dmn :dsd dsd))
       
(plt:fplot 'fn '(1.5 10) (lambda (x)
                         (/ (* x x x x) (exp (* x x 0.5))))
           :clear t
           :xlog t)