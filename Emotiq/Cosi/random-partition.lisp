;; random-partition.lisp -- Generate simulation trees of network nodes for Cosi
;;
;; DM/Emotiq 02/18
;; -------------------------------------------------------------------------

(defpackage :cosi-simgen
  (:use :common-lisp :cosi :crypto-mod-math)
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
  (:import-from :actors
   :=bind
   :=values
   :=defun
   :=lambda
   :=funcall
   :=apply
   :pmapcar
   :spawn
   :current-actor
   :recv
   :become
   :do-nothing
   :make-actor
   :set-executive-pool
   :with-borrowed-mailbox)
  (:export
   :generate-tree
   :reconstruct-tree
   :forwarding))

(in-package :cosi-simgen)

(declaim (optimize (debug 3)))

;; --------------------------------------------------------------------
;; Physical network

(defvar *local-nodes*  '(("Malachite.local" . "10.0.1.6")
                         ("Dachshund.local" . "10.0.1.3")
                         ("Rambo"           . "10.0.1.13")))

(defun get-local-ipv4 (node-name)
  (cdr (assoc node-name *local-nodes*
              :test 'string-equal)))
  
(defun get-my-ipv4 ()
  (get-local-ipv4 (machine-instance)))

(defvar *real-nodes*  (mapcar 'cdr *local-nodes*))
(defvar *leader-node* (get-local-ipv4 "Dachshund.local"))

;; ----------------------------------------------------------------------
;; Network Tree Nodes

(defvar *bins-per-node* 9) ;; prolly needs to be >3 for BFT

(defun make-subs ()
  (make-array *bins-per-node*
              :initial-element nil))

(defclass node ()
  ((ip       :accessor node-ip       ;; IPv4 string for this node
             :initarg  :ip)
   (uuid     :accessor node-uuid     ;; UUID for this node
             :initarg  :uuid)
   (pkeyzkp  :accessor node-pkeyzkp  ;; public key + ZKP
             :initarg  :pkeyzkp)
   (skey     :accessor node-skey     ;; private key
             :initarg  :skey)
   (pkey     :accessor node-pkey     ;; cached ECC point for public key
             :initarg  :pkey)
   (realnode :accessor node-realnode ;; t/f - t when this is a real IPv4 node, nil for fake
             :initform nil)
   (parent   :accessor node-parent   ;; points to node of group parent
             :initarg  :parent
             :initform nil)
   (subs     :accessor node-subs     ;; list of group members beyond self
             :initarg  :subs
             :initform (make-subs))
   ;; -------------------------------------
   (real-ip  :accessor node-real-ip  ;; real node for this node
             :initarg  :real-ip)
   (v        :accessor node-v        ;; first round random seed
             :initform nil)
   (seq      :accessor node-seq      ;; first round ID
             :initform nil)
   (byz      :accessor node-byz      ;; Byzantine misbehav type
             :initform nil)
   (parts    :accessor node-parts    ;; group participants in first round commitment
             :initform nil)
   (load     :accessor node-load     ;; cpu loading of group for this node
             :initform 1)
   (self     :accessor node-self     ;; ptr to Actor handler
             :initarg  :self)
   ))

(defmethod iteri-subs ((node node) fn)
  (loop for sub across (node-subs node)
        for ix from 0
        when sub
        do (funcall fn ix sub)))

(defmethod iter-subs ((node node) fn)
  (iteri-subs node (lambda (ix sub)
                     (declare (ignore ix))
                     (funcall fn sub))))

(defmethod set-node-load (node)
  (setf (node-load node)
        (1+ (loop for sub across (node-subs node)
                  when sub
                  sum  (node-load sub)))))

;; --------------------------------------------------------------------
;; For now, 4 different ways to specify a node:
;;   1. node structure pointer
;;   2. IPv4 address (in dotted string notation)
;;   3. UUID (needs uuid-to-integer for keying *uuid-node* table)
;;   4. PKEY (compressed public key ECC point)

;; XREF from IPv4 address to Tree Node
(defvar *ip-node-tbl*   (make-hash-table :test 'equal)) ;; IPv4 string as key
(defvar *uuid-node-tbl* (make-hash-table))              ;; UUID integer as key
(defvar *pkey-node-tbl* (make-hash-table))              ;; compressed ECC pt integer as key

(defvar *pkey-skey-tbl* (make-hash-table))              ;; commpressed ECC pt integer as key

;; -------------------------------------------------------------------

(defvar *comm-ip*  nil)

(defun make-node (ipstr uuid pkeyzkp parent)
  (let* ((cmpr-pkey (third pkeyzkp))
         (node (make-instance 'node
                              :ip      ipstr
                              :uuid    (uuid:uuid-to-integer uuid)
                              :skey    (gethash cmpr-pkey *pkey-skey-tbl*)
                              :pkey    (edec:ed-decompress-pt cmpr-pkey)
                              :pkeyzkp pkeyzkp
                              :parent  parent
                              :real-ip *comm-ip*
                              )))
    (setf (node-self node) (make-node-dispatcher node)
          (gethash ipstr *ip-node-tbl*)              node
          (gethash (node-uuid node) *uuid-node-tbl*) node
          (gethash cmpr-pkey *pkey-node-tbl*)        node)))

(defun need-to-specify (&rest args)
  (declare (ignore args))
  (error "Need to specify..."))

(defun dotted-string-to-integer (string)
  #+:LISPWORKS (comm:string-ip-address string)
  #+:OPENMCL   (ccl::dotted-to-ipaddr string)
  #+:ALLEGRO   (need-to-specify))

(defun integer-to-dotted-string (val)
  #+:LISPWORKS (comm:ip-address-string val)
  #+:OPENMCL   (need-to-specify)
  #+:ALLEGRO   (need-to-specify))

(defun gen-node-id (ip)
  (if (consp ip)
      (values-list ip)
    (multiple-value-bind (skey pkey) (edec:ed-random-pair)
      (let ((zkp (compute-pkey-zkp skey pkey)))
        (setf (gethash (third zkp) *pkey-skey-tbl*) skey)
        (values ip
                (uuid:make-v1-uuid)
                zkp)
        ))))

;; -------------------------------------------------------------------
;; Node construction

(defun partition (node ip-list &key (key 'identity))
  (let* ((bins  (make-subs))
         (nbins (length bins))
         (vnode (dotted-string-to-integer (node-ip node))))
    (mapc (lambda (ip-arg)
            (let* ((vip (dotted-string-to-integer (funcall key ip-arg)))
                   (ix  (mod (logxor vnode vip) nbins)))
              (push ip-arg (aref bins ix))))
          ip-list)
    (setf (node-subs node) bins)))

(defun inner-make-node-tree (ip ip-list &optional parent)
  (multiple-value-bind (ipstr uuid pkeyzkp)
      (gen-node-id ip)
    (let ((node (make-node ipstr uuid pkeyzkp parent)))
      (when ip-list
        (let ((bins (partition node ip-list
                               :key (lambda (ip-arg)
                                      (if (consp ip-arg)
                                          (car ip-arg)
                                        ip-arg)))))
          (iteri-subs node
                      (lambda (ix subs)
                        (setf (aref bins ix)
                              (inner-make-node-tree (car subs)
                                                    (cdr subs)
                                                    node))))
          (set-node-load node)))
      node)))

(defun make-node-tree (ip vlist)
  ;; main entry point - captures IPv4 of arg ip for use as real-ip in
  ;; succeeding nodes
  (multiple-value-bind (ipstr uuid pkeyzp)
      (gen-node-id ip)
    (let ((*comm-ip*  ipstr))
      (inner-make-node-tree (list ipstr uuid pkeyzp) vlist))))

;; --------------------------------------------------------------------
;; for visual debugging...

#+:LISPWORKS
(progn
  (defmethod children (x layout)
    nil)
  
  (defmethod children ((node node) layout)
    (remove nil (coerce (node-subs node) 'list)))

  (defun split-to-octets (val)
    (um:nlet-tail iter ((n   4)
                        (val val)
                        (lst nil))
      (if (zerop n)
          lst
        (iter (1- n) (ash val -8) (cons (ldb (byte 8 0) val) lst)))
      ))

  (defclass red-text (capi:item-pinboard-object)
    ())

  (defclass black-text (capi:item-pinboard-object)
    ())

  (defmethod capi:draw-pinboard-object :around (pinboard (self red-text) &key &allow-other-keys)
    (gp:with-graphics-state (pinboard
                             :foreground :red)
      (call-next-method)))
  
  (defmethod make-node-pane (graphics-port (node node))
    (declare (ignore graphics-port))
    (let ((txt (node-ip node)))
      (make-instance (if (node-realnode node)
                         'red-text
                       'black-text)
                     :text txt)))

  (defmethod view-tree ((tree node) &key (layout :left-right))
    (capi:contain
     (make-instance 'capi:graph-pane
                    :layout-function layout
                    :roots (list tree)
                    :node-pane-function 'make-node-pane
                    :children-function (lambda (node)
                                         (children node layout))
                    ))))

;; --------------------------------------------------------------------
;; Initial Tree Generation and Persistence

(defvar *default-data-file* "cosi-nodes.txt")
(defvar *default-key-file*  "cosi-keying.txt")

(defun generate-ip ()
  ;; generate a unique random IPv4 address
  (let ((ip (integer-to-dotted-string (random #.(expt 2 32)))))
    (if (gethash ip *ip-node-tbl*)
        (generate-ip) ;; should be unlikely, would be 50% at around 2^16 nodes
      (setf (gethash ip *ip-node-tbl*) ip))))

(defmethod pair-ip-uuid ((node node))
  (list (node-ip node)
        (node-uuid node)
        (node-pkeyzkp node)))

(defmethod pair-ip-uuid ((ip string))
  (pair-ip-uuid (gethash ip *ip-node-tbl*)))

(defun gen-main-tree (leader real-nodes grps)
  (let* ((trees     (mapcar 'make-node-tree real-nodes grps))
         (main-tree (find leader trees
                          :test 'string=
                          :key  'node-ip)))
    ;; mark the real nodes as special
    (mapc (lambda (tree)
            (setf (node-realnode tree) t))
          trees)
    ;; attach the non-leader real nodes to the leader node
    (let ((all-but (remove main-tree trees)))
      (setf (node-subs main-tree) (concatenate 'vector
                                               (node-subs main-tree)
                                               (apply 'vector all-but)))
      (dolist (tree all-but)
        (setf (node-parent tree) main-tree)))
    main-tree))

;; --------------------------------------------------------------
;; Generate Tree / Keying and save to startup init files

(defun generate-tree (&key fname (nel 1000))
  (let* ((leader     *leader-node*)
         (real-nodes  (remove-duplicates *real-nodes*
                                         :test 'string=)))
    
    ;; ensure leader is in the real-nodes collection
    (assert (member leader real-nodes :test 'string=))
    
    ;; pre-populate hash table with real-nodes
    (clrhash *ip-node-tbl*)
    (clrhash *uuid-node-tbl*)
    (clrhash *pkey-node-tbl*)
    (clrhash *pkey-skey-tbl*)
    (dolist (ip real-nodes)
      (setf (gethash ip *ip-node-tbl*) ip))

    ;; build the trees
    (let* ((nreal       (length real-nodes))
           (nel/grp     (ceiling nel nreal))
           (grps        (loop repeat nreal collect
                              (loop repeat nel/grp collect
                                    (generate-ip))))
           (main-tree   (gen-main-tree leader real-nodes grps)))

      ;; save nodes as a text file for later
      (with-open-file (f (merge-pathnames
                          #+:LISPWORKS
                          (sys:get-folder-path :documents)
                          #+(OR :ALLEGRO :OPENMCL)
                          "~/Documents/"
                          (or fname *default-data-file*))
                         :direction :output
                         :if-does-not-exist :create
                         :if-exists :rename)
        (with-standard-io-syntax
          (let ((*print-readably*     t) ;; to get readable UUID's
                (*print-right-margin* 128))
            (pprint `(:leader     ,leader
                      :real-nodes ,(mapcar 'pair-ip-uuid real-nodes)
                      :groups     ,(mapcar (lambda (grp)
                                             (mapcar 'pair-ip-uuid grp))
                                           grps))
                    f))))
      
      ;; write the pkey/skey associations
      (with-open-file (f (merge-pathnames
                          #+:LISPWORKS
                          (sys:get-folder-path :documents)
                          #+(OR :ALLEGRO :OPENMCL)
                          "~/Documents/"
                          *default-key-file*)
                         :direction :output
                         :if-does-not-exist :create
                         :if-exists :rename)
        (let ((keys nil))
          (maphash (lambda (k v)
                     (push (cons k v) keys))
                   *pkey-skey-tbl*)
          (with-standard-io-syntax
            (pprint keys f))))
      
      #+:LISPWORKS (view-tree main-tree)
      main-tree)))

;; ---------------------------------------------------------------
;; Tree reconstruction from startup init files

(defvar *top-node*   nil)
(defvar *my-node*    nil)

(defun reconstruct-tree (&key fname)
  ;; read the keying file
  (let* ((keys        (read-from-string
                       (#+:OPENMCL   uiop/stream::read-file-string
                        #+:LISPWORKS hcl:file-string
                        #+:ALLEGRO   need-to-specify
                        (merge-pathnames
                         #+:LISPWORKS
                         (sys:get-folder-path :documents)
                         #+(OR :ALLEGRO :OPENMCL)
                         "~/Documents/"
                         *default-key-file*))))

         (data        (read-from-string
                       (#+:OPENMCL   uiop/stream::read-file-string
                        #+:LISPWORKS hcl:file-string
                        #+:ALLEGRO   need-to-specify
                        (merge-pathnames
                         #+:LISPWORKS
                         (sys:get-folder-path :documents)
                         #+(OR :ALLEGRO :OPENMCL)
                         "~/Documents/"
                         (or fname *default-data-file*)))))
         (leader      (getf data :leader))
         (real-nodes  (getf data :real-nodes))
         (grps        (getf data :groups)))
    
    ;; sanity checking
    (assert (member leader real-nodes
                    :test 'string=
                    :key  'car))
    (labels ((no-dups (lst)
               (dolist (ip lst)
                 (destructuring-bind (ipstr uuid zkp) ip
                   (assert (null (gethash ipstr *ip-node-tbl*)))
                   (setf uuid (uuid:uuid-to-integer uuid))
                   (assert (null (gethash uuid *uuid-node-tbl*)))
                   (destructuring-bind (r c pcmpr) zkp
                     (declare (ignore r c))
                     (assert (null (gethash pcmpr *pkey-node-tbl*)))
                     (check-pkey zkp)
                     (setf (gethash ipstr *ip-node-tbl*)   ip
                           (gethash uuid  *uuid-node-tbl*) ip
                           (gethash pcmpr *pkey-node-tbl*) ip)
                     )))))
      (clrhash *ip-node-tbl*)
      (clrhash *uuid-node-tbl*)
      (clrhash *pkey-node-tbl*)
      (no-dups real-nodes)
      (mapc #'no-dups grps))
    
    ;; reconstruct keying info
    (clrhash *pkey-skey-tbl*)
    (mapc (lambda (pair)
            (destructuring-bind (k . v) pair
              ;; k is integer, compressed pkey ECC pt
              ;; v is skey integer
              ;; decompression checks for valid ECC pt
              (assert (ed-pt= (ed-nth-pt v) (ed-decompress-pt k)))
              (setf (gethash k *pkey-skey-tbl*) v)))
          keys)
    
    ;; reconstruct the trees
    (let ((main-tree  (gen-main-tree leader real-nodes grps)))
      #+:LISPWORKS (view-tree main-tree)
      (setf *top-node* main-tree
            *my-node*  (gethash (get-my-ipv4) *ip-node-tbl*)))))

;; ---------------------------------------------------------------
;; New leader node election... tree rearrangement

(defun notify-real-descendents (node &rest msg)
  (labels ((recurse (sub-node)
             (if (node-realnode sub-node)
                 (apply 'send sub-node msg)
               (iter-subs sub-node #'recurse))))
    (iter-subs node #'recurse)))

(defun all-nodes-except (node)
  (delete node
          (um:accum acc
            (maphash (um:compose #'acc 'um:snd) *ip-node-tbl*))))

(defun node-model-rebuild-tree (parent node nlist)
  (let ((bins (partition node nlist
                         :key 'node-ip)))
    (iteri-subs node
                (lambda (ix subs)
                  (setf (aref bins ix)
                        (node-model-rebuild-tree node
                                                 (car subs)
                                                 (cdr subs)))))
    (setf (node-parent node) parent)
    (set-node-load node)
    node))

(defun node-elect-new-leader (new-leader-ip)
  (let ((new-top-node (gethash new-leader-ip *ip-node-tbl*)))
    ;; Maybe... ready for prime time?
    (cond ((null new-top-node)
           (error "Not a valid leader node: ~A" new-leader-ip))
          ((eq new-top-node *top-node*)
           ;; nothing to do here...
           )
          (t
           (setf *top-node* new-top-node)
           (node-model-rebuild-tree nil new-top-node
                                    (all-nodes-except new-top-node))
           ;;
           ;; The following broadcast will cause us to get another
           ;; notification, but by then the *top-node* will already
           ;; have been set to new-leader-ip, and so no endless loop
           ;; will occur.
           ;;
           (notify-real-descendents new-top-node :election new-leader-ip))
          )))

;; ---------------------------------------------------------
;; Node insertion/change

(defun bin-for-ip (node ip)
  (let ((vnode  (dotted-string-to-integer (node-ip node)))
        (vip    (dotted-string-to-integer ip)))
    (mod (logxor vnode vip) (length (node-subs node)))))

(defun increase-loading (parent-node)
  (when parent-node
    (incf (node-load parent-node))
    (increase-loading (node-parent parent-node))))

(defun node-model-insert-node (node new-node-info)
  ;; info is (ipv4 UUID pkeyzkp)
  (destructuring-bind (ipstr uuid pkeyzkp) new-node-info
    (let* ((ix       (bin-for-ip node ipstr))
           (bins     (node-subs node))
           (sub-node (aref bins ix)))
      (if sub-node
          ;; continue in parallel with our copy of tree
          (node-model-insert-node sub-node new-node-info)
        ;; else
        (let ((new-node (make-node ipstr uuid pkeyzkp node)))
          (setf (node-real-ip new-node)  ipstr
                (node-realnode new-node) t
                (node-skey new-node)     nil
                (aref bins ix)           new-node)
          (incf (node-load node))
          (increase-loading (node-parent node)))
        ))))

(defun node-insert-node (node new-node-info)
  (destructuring-bind (ipstr uuid pkeyzkp) new-node-info
    (let ((new-node (gethash ipstr *ip-node-tbl*)))
      (if new-node ;; already present in tree?
          ;; maybe caller just wants to change UUID or keying
          ;; won't be able to sign unless it know skey
          (multiple-value-bind (pkey ok) (check-pkey pkeyzkp)
            (when ok
              (setf (node-uuid new-node)     uuid
                    (node-pkeyzkp new-node)  pkeyzkp
                    (node-pkey new-node)     pkey ;; cache the decompressed key
                    (node-realnode new-node) t
                    (node-real-ip new-node)  ipstr)))
        ;; else - not already present
        (node-model-insert-node *top-node* new-node-info))))
  (notify-real-descendents node :insert-node new-node-info))

;; ---------------------------------------------------------

(defun node-model-remove-node (gone-node)
  (remhash (node-ip gone-node)   *ip-node-tbl*)
  (remhash (node-uuid gone-node) *uuid-node-tbl*)
  (let ((pcmpr (third (node-pkeyzkp gone-node))))
    (remhash pcmpr *pkey-node-tbl*)
    (remhash pcmpr *pkey-skey-tbl*)))

(defun node-remove-node (node gone-node-ipv4)
  (let ((gone-node (gethash gone-node-ipv4 *ip-node-tbl*)))
    (when gone-node
      (node-model-remove-node gone-node)
      ;; must rebuild tree to absorb node's subnodes
      (node-model-rebuild-tree nil *top-node*
                               (all-nodes-except *top-node*))
      (when (eq node *top-node*)
        (notify-real-descendents node :remove-node gone-node-ipv4)))))
  
;; -----------------------------------------------------------------

(defun NYI (&rest args)
  (error "Not yet implemented: ~A" args))

(defclass node-ref ()
  ;; Used for sending reply-to's across the network. The NODE-REF will
  ;; make it across and SEND/FORWARDING understands real/fake
  ;; distinctions
  ((real-ip :accessor node-ref-real-ip ;; actual IPv4 address of node
            :initarg  :real-ip)
   (ref-ip  :accessor node-ref-ip      ;; dummy fake IPv4 for sim nodes
            :initarg  :ip)))

(defmethod make-node-ref ((node node))
  (make-instance 'node-ref
                 :real-ip  (node-real-ip node)
                 :ip       (node-ip node)))

(defun make-node-dispatcher (node)
  ;; use indirection to node-dispatcher for while we are debugging and
  ;; extending the dispatcher. Saves reconstructing the tree every
  ;; time the dispatching chanages.
  (ac:make-actor
   ;; one of these closures is stored in the SELF slot of every node
   (lambda (&rest msg)
     (apply 'node-dispatcher node msg))))

(defun node-dispatcher (node &rest msg)
   (um:dcase msg
     ;; ----------------------------
     ;; user accessible entry points - directed to leader node
     
     (:cosi (reply-to msg)
      (reply reply-to (node-compute-cosi node msg)))

     (:validate (reply-to msg sig)
      (reply reply-to :validation (node-validate-cosi node msg sig)))
          
     (:public-key (reply-to)
      (reply reply-to :pkey+zkp (node-pkeyzkp node)))

     (:add/change-node (new-node-info)
      (node-insert-node node new-node-info))

     (:remove-node (node-ip)
      (node-remove-node node node-ip))
     
     (:election (new-leader-ip)
      (node-elect-new-leader new-leader-ip))

     ;; -------------------------------
     ;; internal comms between Cosi nodes
     
     (:commitment (msg seq)
      (node-cosi-commitment node msg seq))

     (:signing   (c seq)
      (node-cosi-signing node c seq))

     ;; -----------------------------------
     
     (:answer (&rest msg)
      ;; for round-trip testing
      (ac:pr msg))

     (t (&rest msg)
        (error "Unknown message: ~A" msg))
     ))

;; -----------------------------------------------------------

(defmethod send ((node node) &rest msg)
  (socket-send (node-ip node) (node-real-ip node) msg))

(defmethod send ((ref node-ref) &rest msg)
  (socket-send (node-ref-ip ref) (node-ref-real-ip ref) msg))

(defmethod send ((node null) &rest msg)
  (ac:pr msg)
  msg)

(defmethod send (dest &rest msg)
  (apply 'ac:send dest msg))

(defun reply (reply-to &rest msg)
  (apply 'send reply-to :answer msg))

;; -----------------------------------------------------
;; THE SOCKET INTERFACE...
;; -----------------------------------------------------

(defun socket-send (ip real-ip msg)
  ;; replace this with USOCKETS protocol
  (let* ((quad     (make-hmac msg
                              (node-skey *my-node*)
                              (node-uuid *my-node*)))
         (agent-ip (format nil "eval@~A" real-ip)))
    (bfly:! agent-ip `(forwarding ,ip ',quad))))

(defun forwarding (dest quad)
  (multiple-value-bind (msg t/f) (verify-hmac quad)
    ;; might want to log incomings that fail the HMAC
    ;; just dropping on floor here...
    (when t/f
      (apply 'send (node-self (dest-ip dest)) msg))))

(defmethod dest-ip ((ip string))
  (gethash ip *ip-node-tbl*))

(defmethod dest-ip ((ref node-ref))
  (dest-ip (node-ref-ip ref)))

;; --------------------------------------------------------------

(defun make-hmac (msg skey uuid)
  (multiple-value-bind (v vpt) (ed-random-pair)
    (let* ((c   (hash-pt-msg vpt msg))
           (r   (sub-mod *ed-r* v
                         (mult-mod *ed-r* c skey))))
      (list msg r c uuid))))

(defun verify-hmac (quad)
  (assert (consp quad))
  (assert (= 4 (length quad)))
  (destructuring-bind (msg r c uuid) quad
    (let* ((node (gethash uuid *uuid-node-tbl*))
           (pkey (node-pkey node))
           (vpt  (ed-add (ed-nth-pt r)
                         (ed-mul pkey c)))
           (cc   (hash-pt-msg vpt msg)))
      (values msg (= cc c))
      )))

;; --------------------------------------------------------------------
;; Message handlers for verifier nodes

(defun node-validate-cosi (node msg sig)
  ;; toplevel entry for Cosi signature validation checking
  (declare (ignore node)) ;; not needed here...
  (destructuring-bind (c r ids) sig
    (let* ((tkey (reduce (lambda (ans id)
                           (let ((node (gethash id *uuid-node-tbl*)))
                             (ed-add ans (node-pkey node))))
                         ids
                         :initial-value (ed-neutral-point)))
           (vpt  (ed-add (ed-nth-pt r)
                         (ed-mul tkey c)))
           (h    (hash-pt-msg vpt msg)))
      (= h c))
    ))

(defun node-compute-cosi (node msg)
  (declare (ignore node msg))
  (NYI 'node-compute-cosi))

(defun node-cosi-commitment (node msg seq)
  (declare (ignore node msg seq))
  (NYI 'node-cosi-commitment))

(defun node-cosi-signing (node c seq)
  (declare (ignore node c seq))
  (NYI 'node-cosi-signing))


