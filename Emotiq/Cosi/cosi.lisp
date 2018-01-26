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
                :ed-neutral-point
                :ed-pt=
		:with-ed-curve
		:ed-compress-pt
		:ed-decompress-pt
		:ed-validate-point
		:ed-hash
		:ed-random-pair)
  (:import-from :ecc-crypto-b571
		:convert-int-to-nbytesv
		:convert-bytes-to-int)
  (:export
   :schnorr-signature
   :verify-schnorr-signature
   ))

;; -------------------------------------------------------

(in-package :cosi)

;; ------------------------------------------------------- 
;; EC points are transported (stored in memory, sent over networks) as
;; compressed points, a single integer, not as pairs of integers.
;;
;; Decompressing a compressed point also checks for point validity:
;;   1. Is point on the EC curve?
;;   2. Does it belong to the correct twist group?
;;   3. It cannot be the neutral point

(defvar *default-curve* :curve-1174)

(defun do-with-curve (curve fn)
  (if curve
      (with-ed-curve curve
        (funcall fn))
    (funcall fn)))

(defmacro with-curve (curve &body body)
  `(do-with-curve ,curve (lambda () ,@body)))

#+:LISPWORKS
(editor:setup-indent "with-curve" 1)

;; --------------------------------------------
;; Hashing with SHA3

(defun select-sha3-hash ()
  (let ((nb  (1+ (integer-length *ed-q*))))
    (cond ((> nb 384) :sha3)
          ((> nb 256) :sha3/384)
          (t          :sha3/256)
          )))

(defun sha3-buffers (&rest bufs)
  ;; assumes all buffers are UB8
  (let ((dig  (ironclad:make-digest (select-sha3-hash))))
    (dolist (buf bufs)
      (ironclad:update-digest dig buf))
    (ironclad:produce-digest dig)))

(defun convert-pt-to-v (pt)
  (let ((nb (ceiling (1+ (integer-length *ed-q*)) 8)))
    (convert-int-to-nbytesv (ed-compress-pt pt) nb)))
  
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
  (let ((v  (convert-pt-to-v pt))
        (mv (loenc:encode msg)))
    (convert-bytes-to-int (sha3-buffers v mv))))

;; ---------------------------------------------------------
;; Single message Schnorr Signatures.
;; Signatures of this type will make up the collective signature.
;;
;; In the following:
;;
;;  v,V = v*G = a commitment value
;;  c = a challenge value, typically c = Hash(V|m),
;;       for message m. This is the Fiat-Shamir challenge.
;;  r = a cloaked signature value = (v - c*k_s) mod r_ec,
;;       for private key k_s, prime modulus r_ec = #|K/h|,
;;       #|K/h| = (the prime order of the curve field),
;;       #|K| = prime order of field in which curve is embedded,
;;       h = cofactor of curve field. r_ec*G = I, the point at infinity.
;;       Points on the EC are computed over prime field q_ec, not r_ec.
;;  sig = a pair = (c r)
;;
;;  Verfied as V' = r*G + c*K_p, c ?= Hash(V'|m),
;;  for public key K_p, message m.
;;
;; When offered a challenge, we compute a commitment, and form the
;; signature from them. The signature caries both a cloaked signature
;; value and the challenge.  Verifiers should be able to use those two
;; values, in combination with our known public key, to verify the
;; signature.
;;
;; NOTE: We must offer a cloaked signature using value r from the
;; isomorphic prime field underlying the EC. If, instead, we were to
;; offer up points along the curve, as in:
;;    sig' = (c R), for R = V - c*K_p,
;; for public key K_p, then anyone could forge a signature knowing
;; only the public key K_p. We gain security through ECDLP and difficulty
;; of inverting the hash.
;;
;; Barriers to attack:
;;    1. get V from c = H(V|m), very difficult for good hash H.
;;    2. knowing V = v*G from inverse hash (!?), ECDLP to find v,
;;       very difficult.
;;    3. knowing (c, r) from sig, v = r + c*k_s can't be solved for v
;;       without knowing k_s secret key. Could be anywhere along a line.
;;       And since prime field is large >2^252, Birthday attack requires
;;       almost >2^128 trials.
;;
;; In distributed cosign the secret v value is communicated to other
;; cosigners. We need to ensure that it is transported securely. It is
;; a random value in the prime field isomorphic to the EC group.
;;
;; Since c = Hash(V|m) this signature also authenticates the message
;; m.  Value c serves as challenge and message authentication. Value r
;; in signature serves as undeniable signature of sender (assuming
;; he/she keeps k_s a secret).

(defun schnorr-signature-from-challenge (c v skey)
  ;; 
  ;; The Schnorr signature is computed over ECC in a manner analogous
  ;; to prime fields. (In fact we have to use the prime field of the
  ;; curve here.)
  ;;
  ;; Value c is a presented challenge value. Value v is a secret
  ;; random commitment seed, from which the challenge was computed.
  ;;
  ;; From the commitment seed v, challenge c, and secret key k_s, we
  ;; compute r = (v - c*k_s), in the prime field isomorphic to the EC.
  ;;
  ;; The Schnorr signature is the integer pair (c r).
  ;;
  ;; Returns secret commitment seed v as second value. Transport it
  ;; securely. Signature pair (c r) is okay to publish.
  (let ((r  (sub-mod *ed-r* v
                     (mult-mod *ed-r* c skey))))
    (list c r))) ;; the signature pair

;; ---------------------------------------------------------------

(defun hash-pt-pt (p1 p2)
  (convert-bytes-to-int
   (sha3-buffers (convert-pt-to-v p1)
                 (convert-pt-to-v p2))))

;; --------------------------------------------------------------------
;; *COSI-PKEYS* - a hashtable directory of public keys assigned to
;; each node. Computing a public key query response, combined with
;; verification takes about 20 ms. So if we have thousands of nodes in
;; the cosi tree this would be a signifcant delay on every signature
;; verification. (10's of sec)
;;
;; Hence at startup, we should query all participating nodes for their
;; public key responses, compute the verification of those responses,
;; and store their public keys in a hashtable for faster verification
;; later.
;;
;; This also means that if any nodes join later, or existing nodes
;; change their keying, then we need special handlers to perfom the
;; public key query and verification of ZKP and insert into this table
;; before we engage those new participants in any Cosi chains.
;;
(defvar *cosi-pkeys* (make-hash-table))

;; --------------------------------------------------------------------
;; Network node simulation
;;
;; For now, nodes are represented by a DLAMBDA closure with private state

;; internal state of each node
(defstruct (node-state
            (:constructor %make-node-state))
  skey       ;; secrect key
  pkey       ;; public key
  v          ;; the most recent commitment seed
  seq        ;; the ID of the current Cosi round
  subs       ;; list of my network subordinates
  parts      ;; list of participant nodes this round
  byz        ;; indicator for Byzantine behavior
  self)

(defun make-node-state (byz)
  (multiple-value-bind (skey pkey) (ed-random-pair)
    (%make-node-state
     :pkey pkey
     :skey skey
     :byz  byz)))

(defmacro with-node-state (state &body body)
  `(with-accessors ((state-pkey  node-state-pkey)
                    (state-skey  node-state-skey)
                    (state-v     node-state-v)
                    (state-seq   node-state-seq)
                    (state-subs  node-state-subs)
                    (state-parts node-state-parts)
                    (state-byz   node-state-byz)
                    (state-self  node-state-self)) ,state
     (declare (ignorable state-pkey state-skey
                         state-v    state-seq
                         state-subs state-parts
                         state-byz  state-self))
     ,@body))

#+:LISPWORKS
(editor:setup-indent "with-node-state" 1)

;; -------------------------------------------------------------
;; Message handlers

(defun msg-ok (msg byz)
  (unless byz
    (or msg t)))
    
(defun node-return-pkey+zkp (state)
  ;; In response to a query about this node's public key, compute a
  ;; ZKP to prove that we know the secret key, and return that proof
  ;; along with the compressed public key EC point.
  (with-node-state state
    (multiple-value-bind (vv vvpt) (ed-random-pair)
      (let* ((c  (hash-pt-pt vvpt state-pkey)) ;; Fiat-Shamir ZKP challenge
             (r  (sub-mod *ed-r* vv
                          (mult-mod *ed-r* state-skey c))))
        (list r c (ed-compress-pt state-pkey)) ;; return ZKP and public key
        ))))

(defun node-validate-public-key (state triple)
  (with-node-state state
    (destructuring-bind (r c pkey) triple
      (let* ((pt   (ed-decompress-pt pkey))
             (vpt  (ed-add (ed-nth-pt r)
                           (ed-mul pt c))))
        (assert (= c (hash-pt-pt vpt pt)))
        pt)) ;; return decompressed EC point
    ))

(defun node-reset (state seq-id)
  ;; maybe we should do sanity checking on seq-id?
  (with-node-state state
    (setf state-v     nil
          state-seq   seq-id
          state-parts nil)))

(defun node-make-cosi-commitment (state seq-id msg)
  ;;
  ;; First phase of Cosi:
  ;;   Decide if msg warrants a commitment. If so return a fresh
  ;;   random value from the prime field isomorphic to the EC.
  ;;   Hold onto that secret seed and return the random EC point.
  ;;   Otherwise return a null value.
  ;;
  ;; We hold that value for the next phase of Cosi.
  ;;
  (with-node-state state
    (funcall state-self :reset seq-id) ;; starting new round of Cosi
    (when (msg-ok msg state-byz)
      (multiple-value-bind (v vpt) (ed-random-pair)
        (setf state-v v) ;; hold secret random seed
        (let ((tparts (list state-self)))
          (dolist (node state-subs)
            (multiple-value-bind (plst pt)
                (funcall node :commitment seq-id msg)
              (when plst
                (push node state-parts)
                (setf tparts (append plst tparts)
                      vpt    (ed-add vpt (ed-decompress-pt pt)))
                )))
          (values tparts
                  (ed-compress-pt vpt))))
      )))

(defun node-compute-signature (state seq-id c)
  ;;
  ;; Second phase of Cosi:
  ;;   Given challenge value c, compute the signature value
  ;;     r = v - c * skey.
  ;;   If we decided against signing in the first phase,
  ;;   then return a null value.
  ;;
  (with-node-state state
    (when (and state-v
               (eql state-seq seq-id))
      (let ((r  (sub-mod *ed-r* state-v (mult-mod *ed-r* c state-skey))))
        (dolist (node state-parts)
          (setf r (add-mod *ed-r* r (funcall node :signing seq-id c))))
        r))))

(defun node-compute-cosi (state seq-id msg)
  ;; top-level entry for Cosi signature creation
  ;; assume for now that leader cannot be corrupted...
  (with-node-state state
    (multiple-value-bind (tparts vpt)
        (funcall state-self :commitment seq-id msg)
      (let* ((c    (hash-pt-msg (ed-decompress-pt vpt) msg)) ;; compute global challenge
             (r    (funcall state-self :signing seq-id c)))
        (list msg
              (list c    ;; cosi signature
                    r
                    tparts))
        ))))

(defun node-validate-cosi (state msg sig)
  ;; toplevel entry for Cosi signature validation checking
  (with-node-state state
    (destructuring-bind (c r tparts) sig
      (let* ((tkey (reduce (lambda (ans node)
                             (let ((pkey (gethash node *cosi-pkeys*)))
                               (ed-add ans pkey)))
                           tparts
                           :initial-value (ed-neutral-point)))
             (vpt  (ed-add (ed-nth-pt r)
                           (ed-mul tkey c)))
             (h    (hash-pt-msg vpt msg)))
        (assert (= h c))
        (list msg sig))
      )))

;; ------------------------------------------------------------

(defun make-node (&key byz)
  (let ((state (make-node-state byz)))
    (with-node-state state
      (setf state-self
            (um:dlambda

              ;; -------------------------------------------
              ;; Cosi entry points
              
              (:public-key ()
               ;; inform the caller of my public key
               (node-return-pkey+zkp state))
              
              (:validate-public-key (triple)
               (node-validate-public-key state triple))
              
              (:reset (seq-id)
               (node-reset state seq-id))

              (:commitment (seq-id msg)
               ;; start of new Cosi signature computation
               (node-make-cosi-commitment state seq-id msg))
            
              (:signing (seq-id c)
               ;; finish of new Cosi signature computation
               (node-compute-signature state seq-id c))

              (:cosi (seq-id msg)
               ;; compute a grand Cosi signature
               (node-compute-cosi state seq-id msg))

              (:validate (msg sig)
               ;; validate a Cosi signature against a message
               (node-validate-cosi state msg sig))

              ;; -----------------------------------------
              ;; for simulation tree construction...
              
              (:setup-tree (nodes)
               (setf state-subs nodes))
              
              (:count-nodes ()
               (1+ (loop for node in state-subs sum
                         (funcall node :count-nodes))))
              )))))

#||#
;; ----------------------------------------------------------------------------
;; Test Code

(defvar *cosi-nodes* nil)
(defvar *top-node*   nil)

(defun build-cosi-directory ()
  (clrhash *cosi-pkeys*)
  (dolist (node *cosi-nodes*)
    (setf (gethash node *cosi-pkeys*)
          (funcall *top-node* :validate-public-key (funcall node :public-key)))))

(defun build-cosi-tree (&optional (n 900))
  ;; default N = 900 produces a tree with 1000 nodes
  ;; good for timings
  (print "Generate nodes")
  (let ((all (time (loop repeat n collect (make-node)))))
    (print "Generate node tree")
    (time
     (um:nlet-tail iter ((nodes all)
                         (tops  nil))
       (if (endp nodes)
           (if (um:single tops)
               (setf *cosi-nodes* all
                     *top-node*   (car tops))
             (progn
              (format t "~%~A Nodes" (length tops))
              (iter tops nil)))
         (let* ((top  (make-node)))
           (push top all)
           (funcall top :setup-tree (um:take 10 nodes))
           (iter (um:drop 10 nodes) (cons top tops)))
         ))))
  (print "Building cosigner public key directory")
  (time (build-cosi-directory)))

#|
(build-cosi-tree 900)

(funcall *top-node* :count-nodes)

(defvar *x*
  (time (funcall *top-node* :cosi 1 "this is a test")))

(time (funcall *top-node* :validate (car *x*) (cadr *x*)))

 |#
;; ---------------------------------------------------------------

(defun schnorr-signature (skey msg &key curve)
  ;;
  ;; Used for computing a single signature, not for Cosi.
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
  ;; msg: c = H(V|msg). This is the "challenge" value, via Fiat-Shamir.
  ;; 
  ;; The Schnorr signature is the integer pair (c r).
  ;;
  (with-curve curve
    ;; get commitment and its seed
    (multiple-value-bind (v vpt) (ed-random-pair)
      (let ((c  (hash-pt-msg vpt msg))) ;; compute challenge
        (schnorr-signature-from-challenge c v skey)))))


(defun verify-schnorr-signature (pkey msg sig-pair &key curve)
  ;;
  ;; Used to verify all signatures: both individual and Cosi.
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
  (with-curve curve
    (destructuring-bind (c r) sig-pair
      (let ((vpt (ed-add
                  (ed-mul (ed-decompress-pt pkey) c)
                  (ed-nth-pt r))))
        (assert (= c (hash-pt-msg vpt msg)))
        ))))

;; ---------------------------------------------------------
;; tests
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
  (verify-schnorr-signature *pkey* msg sig)
  (list msg sig))
|#

;; ------------------------------------------------------------
;; Collective Schnorr Signature

(defun schnorr-commitment ()
  ;; Returns secret random commitment seed v. Transport it securely.
  ;; For Cosi, each participant calls this function to furnish a value
  ;; which will be combined at the top of the Cosi tree.
  (car (multiple-value-list (ed-random-pair))))


(defun collective-commitment (pkeys commits msg &key curve)
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
  ;; Returns the compressed K_p' collective public key, and
  ;; the collective challenge c.
  ;;
  (with-curve curve
    (let* ((pzero (ed-neutral-point))
           (tkey  (reduce (lambda (ans pt)
                            (ed-add ans (ed-decompress-pt pt)))
                          pkeys
                          :initial-value pzero))
           (tcomm (reduce (lambda (ans v)
                            (ed-add ans (ed-nth-pt v)))
                          commits
                          :initial-value pzero))
           (c     (hash-pt-msg tcomm msg)))
      (values (ed-compress-pt tkey) c))))

(defun collective-signature (c sigs &key curve)
  ;;
  ;; For collective challenge, c, signed by each participant with sigs,
  ;; form the final collective signature, a pair (c rt).
  ;;
  (with-curve curve
    (let ((rt  0))
      (dolist (sig sigs)
        (destructuring-bind (c_i r_i) sig
          (assert (= c_i c)) ;; be sure we are using the same challenge val
          (setf rt (add-mod *ed-r* rt r_i))))
      (list c rt))))

;; ---------------------------------------------------------------------
;; tests
#|
(defvar *s-keys* nil)
(defvar *p-keys* nil)
(progn
  (setf *s-keys* nil
        *p-keys* nil)
  (loop repeat 100 do
        (multiple-value-bind (s p) (ed-random-pair)
          (push s *s-keys*)
          (push (ed-compress-pt p) *p-keys*))))

;; show how this all glues together...
(let* ((msg  "this is a test")
       (vs   (time (progn
               (print "Collect commitments")
               (mapcar (lambda (p-key)
                       (declare (ignore p-key))
                       (schnorr-commitment))
                     *p-keys*)))))
  (print "Starting collective commitment")
  (multiple-value-bind (tkey c)
      (time
       (collective-commitment *p-keys* vs msg))
    (print "Starting signature gathering")
    (let* ((sigs (mapcar (lambda (v s-key)
                           (schnorr-signature-from-challenge c v s-key))
                         vs *s-keys*))
           (tsig (collective-signature c sigs)))
      (print "Verify collective signature")
      (destructuring-bind (cc rt) tsig
        (assert (= c cc))
        (verify-schnorr-signature tkey msg tsig)
        (list msg tsig)))))

(defun tst ()
  (let* ((msg  "this is a test")
         (vs   (mapcar (lambda (p-key)
                         (declare (ignore p-key))
                         (schnorr-commitment))
                       *p-keys*)))
    (multiple-value-bind (tkey c)
        (collective-commitment *p-keys* vs msg)
      (let* ((sigs (mapcar (lambda (v s-key)
                             (schnorr-signature-from-challenge c v s-key))
                           vs *s-keys*))
             (tsig (collective-signature c sigs)))
        (destructuring-bind (cc rt) tsig
          (assert (= c cc))
          (verify-schnorr-signature tkey msg tsig)
          (list msg tsig))))))

(time (loop repeat 1 do (tst)))
  
 |#


