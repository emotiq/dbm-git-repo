;; cosi.lisp -- Cothority signatures in Lisp
;;
;; DM/Emotiq  01/18
;; ------------------------------------------------------------------------

(defpackage :cosi
  (:use :common-lisp :crypto-mod-math)
  (:import-from :edwards-ecc
		:ed-add 
		:ed-sub 
		:ed-mul 
		:ed-div 
		:ed-affine
		:ed-nth-pt
		:*ed-r*
		:*ed-q*
                :ed-pt=
		:with-ed-curve
		:ed-compress-pt
		:ed-decompress-pt
		:ed-validate-point
		:ed-hash
		:ed-random-pair)
  (:import-from :ecc-crypto-b571
		:convert-int-to-nbytesv
		:convert-bytes-to-int
		:sha3-buffers)
  (:export
   :schnorr-signature
   :verify-schnorr-signature
   ))

(in-package :cosi)

(defun hash-pt-msg (pt msg)
  (let* ((nb (ceiling (1+ (integer-length *ed-q*)) 8))
	 (v  (convert-int-to-nbytesv (ed-compress-pt pt) nb))
	 (mv (loenc:encode msg)))
    (convert-bytes-to-int (sha3-buffers v mv))))

(defun schnorr-signature (skey msg &key (curve :curve-1174))
  (with-ed-curve curve
    (multiple-value-bind (v vpt) (ed-random-pair)
    (let* ((c   (hash-pt-msg vpt msg))
	   (r   (sub-mod *ed-r* v 
		       (mult-mod *ed-r* c skey))))
	(list c r)))))

(defun verify-schnorr-signature (pkey msg sig-pair &key (curve :curve-1174))
  (with-ed-curve curve
    (destructuring-bind (c r) sig-pair
      (let ((vpt (ed-add
		  (ed-mul (ed-decompress-pt pkey) c)
		  (ed-nth-pt r))))
	(assert (= c (hash-pt-msg vpt msg)))
	))))

#|
(defvar *skey*) ;; my secret key
(defvar *pkey*) ;; my public key (a compressed ECC point)

(multiple-value-bind (v vpt) (ed-random-pair)
  (let ((cpt (ed-compress-pt vpt)))
    (assert (ed-pt= (ed-decompress-pt cpt) vpt))
    (setf *skey* v
      *pkey* cpt)))

(setf *skey* 1 *pkey* (ed-compress-pt (ed-nth-pt 1)))

(let* ((msg "this is a test")
       (sig (schnorr-signature *skey* msg)))
  (verify-schnorr-signature *pkey* msg sig))


       
|#
