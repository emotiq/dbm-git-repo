
(in-package :um)

(defun tst (&key (nkey 4) (niter #N1_000_000))
  (let* ((keys   (lc ((symb n) (n <.. 1 nkey))))
         (plist  (mapcan #'identity (lc ((list n n) (n <- keys)))))
         (map    (reduce #'(lambda (map n)
                             (maps:add n n map))
                         keys
                         :initial-value (maps:empty)))
         (arr    (coerce keys 'vector)))
    (print "Compute Overhead")
    (time
     (loop repeat niter do
           (let* ((ix (random nkey))
                  (key (aref arr ix)))
             )))
    (print "Compute Map")
    (time
     (loop repeat niter do
           (let* ((ix (random nkey))
                  (key (aref arr ix)))
             (maps:find key map))))
    (print "Compute PList")
    (time
     (loop repeat niter do
           (let* ((ix (random nkey))
                  (key (aref arr ix)))
             (getf plist key))))
    
    ))
         
  