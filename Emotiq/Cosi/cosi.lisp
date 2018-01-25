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

;; -------------------------------------------------------

(in-package :cosi)

;; ------------------------------------------------------- 
;; EC points are transported (stored in memory, sent over networks) as
;; compressed points, a single integer, not as pairs of integers.

(defun hash-pt-msg (pt msg) 
  ;; We hash an ECC point by first converting to compressed form, then
  ;; to a UB8 vector. A message is converted to a UB8 vector by calling
  ;; on LOENC:ENCODE.
  ;;
  ;; Max compressed point size is 1 bit more than the integer-length
  ;; of the underlying curve field prime modulus.
  ;;
  ;; We return the SHA3 hash as a big-integer
  ;;
  ;; This is callled with the *ed-curve* binding in effect. Client
  ;; functions should call this function from within a WITH-ED-CURVE.
  ;;
  (let* ((nb (ceiling (1+ (integer-length *ed-q*)) 8))
	 (v  (convert-int-to-nbytesv (ed-compress-pt pt) nb))
	 (mv (loenc:encode msg)))
    (convert-bytes-to-int (sha3-buffers v mv))))

;; ---------------------------------------------------------
;; Single message Schnorr Signatures.
;; Signatures of this type will make up the collective signature.

(defun schnorr-signature (skey msg &key (curve :curve-1174))
  ;; 
  ;; The Schnorr signature is computed over ECC in a manner analogous
  ;; to prime fields. (In fact we have to use the prime field of the
  ;; curve here.)
  ;;
  ;; Compute a random value v and its curve V = v*G, for EC generator
  ;; G (a point on the curve).  In the prime field of the curve
  ;; itself, (not in the underlying prime field on which the curve is
  ;; computed), we compute r = (v - c*k_s), for secret key k_s, (an integer).
  ;;
  ;; Form the SHA3 hash of the point V concatenated with the message
  ;; msg: c = H(V|msg).
  ;; 
  ;; The Schnorr signature is the integer pair (c r).
  ;;
  (with-ed-curve curve
    (multiple-value-bind (v vpt) (ed-random-pair)
    (let* ((c   (hash-pt-msg vpt msg))
	   (r   (sub-mod *ed-r* v 
		       (mult-mod *ed-r* c skey))))
	(list c r)))))

(defun verify-schnorr-signature (pkey msg sig-pair &key (curve :curve-1174))
  ;;
  ;; To verify a Schnorr signature (c r), a pair of integers, we take
  ;; the provider's public key K_p (an EC point in compressed form)
  ;; and multiply by c and add r*G to derive EC point V' = r*G + c*K_p.
  ;;
  ;; We necessarily have K_p = k_s*G. 
  ;; So V' = r*G + c*k_s*G = (v - c*k_s)*G + c*k_s*G = v*G = V
  ;;
  ;; Then compute the SHA3 hash of the point V' and the message msg: H(V'|msg).
  ;; If that computed hash = c, then the signature is verified.
  ;;
  ;; Only the caller knows the secret key, k_s, needed to form the value of r.
  ;; 
  ;; The math here is done over the EC using point addition and scalar
  ;; multiplication.
  ;;
  (with-ed-curve curve
    (destructuring-bind (c r) sig-pair
      (let ((vpt (ed-add
		  (ed-mul (ed-decompress-pt pkey) c)
		  (ed-nth-pt r))))
	(assert (= c (hash-pt-msg vpt msg)))
	))))

;; ---------------------------------------------------------
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

;; ------------------------------------------------------------
;; Collective Schnorr Signature

(defun collective-commitment (pkeys commits msg &key (curve :curve-1174))
  ;; Given a list of public keys, pkeys, and a list of corresponding
  ;; commitments, commits, form a collective public key, a collective
  ;; commitment, and the collective challenge.
  ;;
  ;; Individual commitments are compressed EC points. Pkeys is also a
  ;; list of compressed EC points.
  ;;
  ;; K_p' = Sum(K_p,i)
  ;; V'   = Sum(V_i)
  ;; c    = Hash(V'|msg)
  ;;
  ;; where Sum is EC point addition.
  ;;
  (with-ed-curve curve
    (let* ((pzero (ec-nth-pt 0))
           (tkey  (reduce 'ed-add pkeys
                          :initial-value pzero))
           (tcomm (reduce 'ed-add commits
                          :initial-value pzero))
           (c     (hash-pt-msg tcomm msg)))
      (values tkey tcomm c))))

(defun collective-signature (c sigs &key (curve :curve-1174))
  ;;
  ;; For collective challenge, c, signed by each participant with sigs,
  ;; form the final collective signature, a pair (c rt).
  ;;
  (with-ed-curve curve
    (let ((rt  (ec-nth-pt 0)))
      (dolist (sig sigs)
        (destructuring-bind (c_i r_i) sig
          (assert (= c_i c)) ;; be sure we are using the same challenge val
          (setf rt (ec-add rt r_i))))
      (values c rt))))

(defun 


