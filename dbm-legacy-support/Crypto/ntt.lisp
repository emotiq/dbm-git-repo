;; ntt.lisp -- number theoretic transform (FFT over finite field, modular arithmetic)
;; DM/Acudora 02/14
;; -----------------------------------------------------

(in-package :ecc-crypto-b571)

;; work in 16-point NTT over field Z_257
;; Primitive 1/256 root of unity = 3
;; successive squares of that primitive root become primitive roots for
;; lower power-of-2 roots of unity, e.g., 9 = prim 1/128 root of unity, etc.

(defun ntt (blk &key inv)
  (let ((lsts (um:group blk 16)))
    (labels ((pad-16 (lst)
               (let ((nel (length lst)))
                 (if (< nel 16)
                     (append lst (zeros (- 16 nel)))
                   lst)))
             (zeros (nel)
               (loop for ix from 0 below nel collect 0)))
      (loop for lst in lsts nconc
            (ntt-16 (pad-16 lst) :inv inv)) )))

(defun ntt-16 (lst &key inv)
  (assert (= 16 (length lst)))
  (let ((twids '(256 241 64 249 136 81 9 3))
        (m     257))
    (labels ((evens (lst)
               (if (endp lst)
                   lst
                 (cons (car lst)
                       (odds (cdr lst)))))

             (odds (lst)
               (if (endp lst)
                   lst
                 (evens (cdr lst))))
             
             (iter (lst twid)
               (if (endp (cdr lst))
                   lst
                 (let* ((twid2 (mult-mod m twid twid))
                        (es (iter (evens lst) twid2))
                        (os (iter (odds lst) twid2))
                        (f  1))
                   (labels ((bumpf ()
                              (shiftf f (mult-mod m f twid)))
                            (ffth ()
                              (mapcar (lambda (ex ox)
                                        (add-mod m ex (mult-mod m (bumpf) ox)))
                                      es os)))
                     (let* ((front (ffth))
                            (back  (ffth)))
                       (append front back))) ))))

      (let ((ans (iter lst (if inv (inv-mod m 249) 249))))
        (if inv
            (mapcar (lambda (x)
                      (div-mod m x 16))
                    ans)
          ans))) ))

                   
