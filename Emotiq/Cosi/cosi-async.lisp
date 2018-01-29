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
;; Network node simulation
;;
;; A Cosi network is presumed to be a tree of cooperating nodes, with
;; each node having some numnber of peers under its direction. Each
;; node carries out a portion of the Cosi protocol, and combines its
;; result with those of its directed peers. The peers perform this
;; same action recursively. The top node of the tree submits the final
;; result.
;;
;; Every potential signature verifier node needs to know the public
;; keys of all cosigner nodes. Public keys aren't simply transmitted
;; to requestors, since attackers could substitute forged signature if
;; they desired. Rather, all requests for public keys from nodes must
;; be returned with an accompanying NIZKP to prove that the responding
;; node actually knows the private key associated with the public key.
;;
;; Collecting all these public keys and verifying the NIZKP's takes
;; considerable time for large Cosi networks. It also implies a lot of
;; network traffic.
;;
;; So rather than forcing the verifiers to go out and request public
;; keys from all the cosigners, we include the cosigner public keys
;; with their individual ZKP's as part of the overall message
;; signature. This roughly triples the signature size, to around 114
;; bytes / cosigner key. But that likely pales in comparison to
;; constant network traffic. Verifier nodes can cache fully verified
;; public keys so they don't need to repeat the proof verification.
;;
;; For now, nodes are represented by a DLAMBDA closure with private state

;; internal state of each node
(defstruct (node-state
            (:constructor %make-node-state))
  (id (gensym)) ;; node id
  skey       ;; secrect key
  pkey       ;; public key
  zkp        ;; cached NIZKP of our public key
  v          ;; the most recent commitment seed
  seq        ;; the ID of the current Cosi round
  subs       ;; list of my network subordinates
  parts      ;; list of participant nodes this round
  byz        ;; indicator for Byzantine behavior
  ;; -------------
  (levels  0) ;; used by top node for node insertion
  (session 0) ;; used by top node for session id
  self)

(defun make-node-state (byz)
  (multiple-value-bind (skey pkey) (ed-random-pair)
    (%make-node-state
     :pkey pkey
     :skey skey
     :byz  byz)))

(defmacro with-node-state (state &body body)
  `(with-accessors ((state-id         node-state-id)
                    (state-pkey       node-state-pkey)
                    (state-skey       node-state-skey)
                    (state-zkp        node-state-zkp)
                    (state-v          node-state-v)
                    (state-seq        node-state-seq)
                    (state-subs       node-state-subs)
                    (state-parts      node-state-parts)
                    (state-levels     node-state-levels)
                    (state-session    node-state-session)
                    (state-byz        node-state-byz)
                    (state-self       node-state-self)) ,state
     (declare (ignorable state-id   state-pkey  state-skey state-zkp
                         state-v    state-seq
                         state-subs state-parts state-levels  state-session
                         state-byz  state-self))
     ,@body))

#+:LISPWORKS
(editor:setup-indent "with-node-state" 1)

;; -------------------------------------------------------------
;; Stand-in for Cosi network comms...
;;
;; Nodes have a GENSYM ID, and a DLAMBDA handler closure
;; Each node stores its own private local state

(defstruct cosi-node
  fn
  id)

(defvar *cosi-nodes* (make-hash-table)) ;; a hashtable linking node ID with simulator closure

(defun node-fn (id)
  (cosi-node-fn (gethash id *cosi-nodes*)))

;; -----------------------------------------------------------

(defmethod ask ((node cosi-node) &rest msg)
  (apply 'ac:ask (cosi-node-fn node) msg))

(defmethod ask ((node symbol) &rest msg)
  (apply 'ask (gethash node *cosi-nodes*) msg))

;; -----------------------------------------------------------

(defmethod send (dest &rest msg)
  (apply 'ac:send dest msg))

(defmethod send ((dest symbol) &rest msg)
  (apply 'send (node-fn dest) msg))

(defun self-call (&rest msg)
  (apply 'ac:self-call msg))

;; -----------------------------------------------------------

(defmethod reply ((id symbol) &rest ans)
  (apply 'reply (node-fn id) ans))

(defmethod reply ((id null) &rest ans)
  ans)

(defmethod reply (id &rest ans)
  (apply 'ac:send id ans))

;; -------------------------------------------------------------
;; Message handlers for Cosi nodes

(defvar *subs-per-node*  9)
(defvar *default-timeout-period* 40)

(defun msg-ok (msg byz)
  (unless byz
    (or msg t)))
    
(defun node-return-pkey+zkp (state reply-to)
  ;; In response to a query about this node's public key, compute a
  ;; ZKP to prove that we know the secret key, and return that proof
  ;; along with the compressed public key EC point.
  ;;
  ;; We cache the computation on first request.
  ;;
  (with-node-state state
    (reply reply-to
           (or state-zkp 
               (multiple-value-bind (v vpt) (ed-random-pair)
                 (let* ((c     (hash-pt-pt vpt state-pkey)) ;; Fiat-Shamir NIZKP challenge
                        (r     (sub-mod *ed-r* v
                                        (mult-mod *ed-r* state-skey c)))
                        (pcmpr (ed-compress-pt state-pkey)))
                   (setf state-zkp (list r c pcmpr))) ;; NIZKP and public key
                 )))))

(defvar *cosi-pkeys* (make-hash-table)) ;; previously verified public keys

(defun validate-public-keys ()
  ;; Each verifier node maintains a cache of previously verified
  ;; public keys in *COSI-PKEYS*. If we have already seen the pkey in
  ;; the triple and verified it, we bypass re-verification and use the
  ;; cached value.
  (maphash (lambda (id node)
             (declare (ignore node))
             (destructuring-bind ((r c pcmpr))
                 (ask id :public-key nil)
               (let* ((pt   (ed-decompress-pt pcmpr)) ;; node's public key
                      (vpt  (ed-add (ed-nth-pt r)    ;; validate with NIZKP 
                                    (ed-mul pt c))))
                 (assert (= c (hash-pt-pt vpt pt)))
                 (setf (gethash id *cosi-pkeys*) pt)) ;; return decompressed EC point
               ))
           *cosi-nodes*))

(defun node-reset (state seq-id)
  ;; maybe we should do sanity checking on seq-id?
  (with-node-state state
    (setf state-v     nil
          state-seq   seq-id
          state-parts nil)))

(defun map-nodes (cbfn node-fn nodes)
  (if nodes
      (let* ((len   (length nodes))
             (count (list len))
             (ansv  (make-array len)))
        (labels ((done (ix ans)
                   (setf (aref ansv ix) ans)
                   (when (zerop (mpcompat:atomic-decf (car (the cons count))))
                     (funcall cbfn (coerce ansv 'list))))
                 (callback (ix)
                   (lambda (ans)
                     (done ix ans))))
          (loop for node in nodes
                for ix from 0
                do
                (ac:spawn node-fn node (callback ix)))
          ))
    ;; else - no nodes
    (funcall cbfn nil)
    ))


(defun sub-commitment (cbfn node seq-id msg)
  (let ((self (ac:current-actor)))
    (send node :commitment self seq-id msg)
    (ac:recv
      ((list* :commit ans)
       (funcall cbfn ans))

      :TIMEOUT *default-timeout-period*
      :ON-TIMEOUT (progn
                    (ac:become 'ac:do-nothing)
                    (funcall cbfn nil))
      )))

(defun group-commitment (cbfn nodes seq-id msg)
  (map-nodes cbfn (lambda (node done-fn)
                    (sub-commitment done-fn node seq-id msg))
             nodes))

(defun node-make-cosi-commitment (state reply-to seq-id msg)
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
    (when (msg-ok msg state-byz)
      (node-reset state seq-id) ;; starting new round of Cosi
      (multiple-value-bind (v vpt) (ed-random-pair)
        (setf state-v v) ;; hold secret random seed
        (let ((tparts (list state-id)))
          (ac:=bind (lst)
              (group-commitment ac:=bind-callback state-subs seq-id msg)
            (labels ((fold-answer (ans node)
                       (cond ((null ans)
                              (mark-node-no-response state node))
                             (t
                              (destructuring-bind (pt plst) ans
                                (push node state-parts)
                                (setf tparts (append plst tparts)
                                      vpt    (ed-add vpt (ed-decompress-pt pt)))
                                ))
                             )))
              (mapc #'fold-answer lst state-subs)
              (reply reply-to :commit (ed-compress-pt vpt) tparts)
              ))))
      )))

(defun sub-signing (cbfn node seq-id c)
  (let ((self (ac:current-actor)))
    (send node :signing self seq-id c)
    (ac:recv
      ((list :signed ans)
       (funcall cbfn ans))
      ((list :missing-node)
       (funcall cbfn nil))
      ((list :invalid-commitment)
       (funcall cbfn nil))

      :TIMEOUT *default-timeout-period*
      :ON-TIMEOUT (progn
                    (ac:become 'ac:do-nothing)
                    (funcall cbfn nil))
      )))

(defun group-signing (cbfn nodes seq-id c)
  (map-nodes cbfn (lambda (node done-fn)
                    (sub-signing done-fn node seq-id c))
             nodes))

(defun node-compute-signature (state reply-to seq-id c)
  ;;
  ;; Second phase of Cosi:
  ;;   Given challenge value c, compute the signature value
  ;;     r = v - c * skey.
  ;;   If we decided against signing in the first phase,
  ;;   then return a null value.
  ;;
  (with-node-state state
    (if (and state-v
             (eql state-seq seq-id))
      (let ((r  (sub-mod *ed-r* state-v (mult-mod *ed-r* c state-skey)))
            (missing nil))
        (ac:=bind (lst)
            (group-signing ac:=bind-callback state-parts seq-id c)
          (labels ((fold-answer (ans node)
                     (cond ((null ans)
                            (mark-node-no-response state node)
                            (setf missing t))
                           ((not missing)
                            (setf r (add-mod *ed-r* r ans)))
                           )))
            (mapc #'fold-answer lst state-parts)
            (if missing
                (reply reply-to :missing-node)
              (reply reply-to :signed r))
            )))
      ;; else -- bad state
      (reply reply-to :invalid-commitment) ;; request restart
      )))

(defun node-compute-cosi (state reply-to msg)
  ;; top-level entry for Cosi signature creation
  ;; assume for now that leader cannot be corrupted...
  (let ((self (ac:current-actor)))
    (with-node-state state
      (self-call :commitment self (incf state-session) msg)
      (ac:recv
        ((list :commit vpt tparts)
         (let ((c  (hash-pt-msg (ed-decompress-pt vpt) msg))) ;; compute global challenge
           (self-call :signing self state-seq c)
           (ac:recv
             ((list :signed r)
              (reply reply-to :signature msg (list c    ;; cosi signature
                                                   r
                                                   tparts)))
             ((list :missing-node)
              ;; retry from start
              (node-compute-cosi state reply-to msg))

             ((list :invalid-commitment)
              (error "Invalid commitment"))
             ))))
      )))

;; -------------------------------------------------------------

(defun mark-node-no-response (state node)
  (declare (ignore state node)) ;; for now...
  )

(defun node-try-insert-node (state reply-to node level)
  ;; Simulate adding a fresh node to the tree.
  ;;
  ;; If the insertion can't happen in this node, and level is not yet
  ;; zero, then the request is passed to some peer at decremented
  ;; level. If level reaches zero without success then NIL is returned
  ;; to the caller, and the global level should be incremented for
  ;; another try.
  ;;
  (labels ((fail ()
             (reply reply-to :node-not-inserted))
           (success ()
             (reply reply-to :node-inserted)))
    (with-node-state state
      (let ((capy (- *subs-per-node* (length state-subs))))
        
        (cond ((plusp capy)
               (push node state-subs)
               (success))
              
              ((plusp level)
               (labels ((try (nodes)
                          (if (endp nodes)
                              (fail)
                            (progn
                              (send (car nodes) :try-insert-node state-id node (1- level))
                              (ac:recv

                                ((list :node-inserted)
                                 (success))

                                ((list :node-not-inserted)
                                 (try (cdr nodes)))

                                :TIMEOUT *default-timeout-period*
                                :ON-TIMEOUT
                                (progn
                                  (mark-node-no-response state (car nodes))
                                  (try (cdr nodes)))
                                )))))
                 (try state-subs)))
              
              (t
               (fail))
              )))))
    
(defun node-must-insert-node (state reply-to node)
  ;; this should only be performed on by the top node
  (with-node-state state
    (labels ((ins ()
               (self-call :try-insert-node state-id node state-levels)
               (ac:recv
                 ((list :node-inserted)
                  (reply reply-to :node-inserted))
                 ((list :node-not-inserted)
                  (incf state-levels)
                  (ins))
                 )))
      (ins) )))
      
(defvar *top-node*   nil)

;; --------------------------------------------------------------------
;; Message handlers for verifier nodes

(defun node-validate-cosi (state msg sig)
  ;; toplevel entry for Cosi signature validation checking
  (with-node-state state
    (destructuring-bind (c r ids) sig
      (let* ((tkey (reduce (lambda (ans id)
                             (let ((pkey (gethash id *cosi-pkeys*)))
                               (ed-add ans pkey)))
                           ids
                           :initial-value (ed-neutral-point)))
             (vpt  (ed-add (ed-nth-pt r)
                           (ed-mul tkey c)))
             (h    (hash-pt-msg vpt msg)))
        (assert (= h c))
        (list msg sig))
      )))

;; ------------------------------------------------------------
;; MAKE-NODE -- make a Cosi network node simulation closure

(defun make-node (&key byz)
  ;; assigns a node ID (with GENSYM) and returns a state object after
  ;; registering with the ID-closure cross reference table for ASK's.
  (let ((state (make-node-state byz))) ;; assigns random ECC keying
    (node-return-pkey+zkp state nil) ;; done for pre-caching side effect
    (with-node-state state
      (setf state-self
            (make-cosi-node
             :id state-id
             :fn (ac:make-actor (um:dlambda
                   ;; -------------------------------------------
                   ;; Cosi entry points
                   
                   (:public-key (reply-to)
                    ;; inform the caller of my public key
                    (node-return-pkey+zkp state reply-to))
                   
                   (:commitment (reply-to seq-id msg)
                    ;; start of new Cosi signature computation
                    (node-make-cosi-commitment state reply-to seq-id msg))
                   
                   (:signing (reply-to seq-id c)
                    ;; finish of new Cosi signature computation
                    (node-compute-signature state reply-to seq-id c))
                   
                   (:cosi (reply-to msg)
                    ;; compute a grand Cosi signature
                    (node-compute-cosi state reply-to msg))

                   (:try-insert-node (reply-to node level)
                    ;; try to insert a node somewhere in this subtree
                    (node-try-insert-node state reply-to node level))

                   (:insert-node (reply-to node)
                    ;; must insert a node in the tree somewhere
                    ;; should only be sent to the master node
                    (node-must-insert-node state reply-to node))
                   
                   ;; -----------------------------------------
                   ;; Verifier entry points
                   
                   (:validate (msg sig)
                    ;; validate a Cosi signature against a message
                    (node-validate-cosi state msg sig))
                   
                   ;; -----------------------------------------
                   ;; for simulation tree construction...
                   
                   (:count-nodes ()
                    ;; for manual verification
                    (1+ (loop for node in state-subs sum
                              (ask node :count-nodes))))
                   
                   (:tree-subs ()
                    ;; for tree visualization
                    state-subs)

                   (t (&rest msg)
                      (error "unknown message: ~A" msg))
                   )))
            ;; register ourself with the ASK cross reference table
            (gethash state-id *cosi-nodes*) state-self))))

#||#
;; ----------------------------------------------------------------------------
;; Test Code

(defun organic-build-tree (&optional (n 1000))
  (clrhash *cosi-nodes*)
  (clrhash *cosi-pkeys*)
  (print "Bulding cosigner node tree")
  (let ((all (time (loop repeat n collect (make-node)))))
    (setf *top-node* (cosi-node-id (car all)))
    (time
     (progn
       (dolist (node (cdr all))
         (send *top-node* :insert-node nil (cosi-node-id node)))
       (ask *top-node* :count-nodes)))
    (print "Validating public keys")
    (time
     (validate-public-keys))
    ))

;; --------------------------------------------------------------------
;; for visual debugging...

#+:LISPWORKS
(progn
  (defmethod children ((node symbol) layout)
    (reverse (ask node :tree-subs)))

  (defmethod print-node (x keyfn)
    nil)

  (defmethod print-node ((node symbol) keyfn)
    (format nil "~A" (funcall keyfn node)))
  
  (defmethod view-tree ((tree symbol) &key (key #'identity) (layout :left-right))
    (capi:contain
     (make-instance 'capi:graph-pane
                    :layout-function layout
                    :roots (list tree)
                    :children-function (lambda (node)
                                         (children node layout))
                    :print-function (lambda (node)
                                      (print-node node key))
                    ))))

;; --------------------------------------------------------------------

(defvar *x* nil) ;; saved result for inspection

(defun tst (&optional (n 100))
    (organic-build-tree n)
  
    (format t "~%Nbr Nodes: ~A"
            (ask *top-node* :count-nodes))

    #+:LISPWORKS
    (view-tree *top-node*)

    (let ((msg "this is a test"))
      (ac:with-borrowed-mailbox (mbox)
        (send *top-node* :cosi mbox msg)
        (format t "~%Create ~a node multi-signature" n)
        (time (setf *x* (mpcompat:mailbox-read mbox)))

        (print "Verify signature")
        (time (ask *top-node* :validate msg (third *x*)))
        ))
    )

#|
(ac::kill-executives)

(time (ask *top-node* :validate (second *x*) (third *x*)))

 |#
;; ==================================================================
;; Here and below is cut #0 on the Brooks scale
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


