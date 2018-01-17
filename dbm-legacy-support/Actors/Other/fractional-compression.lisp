

(defun db20 (x)
  (* 20 (log x 10)))

(defun ampl20 (x)
  (expt 10 (/ x 20)))

(progn
  (plt:fplot 'plt '(-50 20)
             #'identity
             :clear t
             :title "Fractional Compression"
             :xtitle "Input Level [dB]"
             :ytitle "Output Level [dB]")

  (let* ((thr    -20)
         (ratio   3.7)
         (g-big   10)
         (frac    0.2)
         ;; (g-small (db20 (+ 1 (/ (- (ampl20 g-big) 1) frac))))
         (g-small 10)
         (g-out   (- (g-at-zero g-small thr ratio frac))))
    
    (plt:fplot 'plt '(-50 20)
               (lambda (lev-db)
                 (let* ((cmpr  (* (max 0 (- lev-db thr)) (- (/ ratio) 1))))
                   (+ lev-db g-out
                      (db20 (+ 1
                               (* frac
                                  (- (ampl20 (+ g-small cmpr)) 1)))))))
               :color :red)))

(defun g-at-zero (gthr thr ratio frac)
  (db20 (+ 1
           (* frac
              (- (ampl20 (- gthr (* thr (- (/ ratio) 1))))
                 1)))))

#|
  0 dBFS   18 dBVU   95 dBSPL

-18 dBFS    0 dBVU   77 dBSPL

-65 dBFS  -47 dBVU   30 dBSPL
|#


                        