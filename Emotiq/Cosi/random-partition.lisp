;; random-partition.lisp -- Generate simulation trees of network nodes for Cosi
;;
;; DM/Emotiq 02/18
;; -------------------------------------------------------------------------

(defpackage :cosi-simgen
  (:use :common-lisp :cosi)
  (:export
   :generate-tree
   :reconstruct-tree))

(in-package :cosi-simgen)

(declaim (optimize (debug 3)))

;; --------------------------------------------------------------------
;; Physical network

(defvar *malachite*  "10.0.1.6")
(defvar *dachshund*  "10.0.1.3")
(defvar *rambo*      "10.0.1.13")

(defvar *real-nodes*  (list *malachite* *dachshund* *rambo*))
(defvar *leader-node* *dachshund*)

;; ----------------------------------------------------------------------
;; Network Tree Nodes

(defvar *bins-per-node* 9) ;; prolly needs to be >3 for BFT

(defclass node ()
  ((ip       :accessor node-ip       ;; IPv4 string for this node
             :initarg  :ip)
   (uuid     :accessor node-uuid     ;; UUID for this node
             :initarg  :uuid)
   (pkeyzkp  :reader    node-pkeyzkp ;; public key + ZKP
             :initarg  :pkeyzkp)
   (skey     :reader   node-skey     ;; private key
             :initarg  :skey)
   (realnode :accessor node-realnode ;; t/f - t when this is a real IPv4 node, nil for fake
             :initform nil)
   (parent   :accessor node-parent   ;; points to node of group parent
             :initarg  :parent
             :initform nil)
   (bins     :accessor node-bins     ;; list of group members beyond self
             :initarg  :bins
             :initform nil)
   ))

;; XREF from IPv4 address to Tree Node
(defvar *ip-node*   (make-hash-table :test 'string=))
(defvar *uuid-node* (make-hash-table :test 'uuid:uuid=))
(defvar *pkey-node* (make-hash-table))

(defvar *pkey-skey* (make-hash-table))

(defun make-node (ipstr uuid pkeyzkp parent)
  (let ((node (make-instance 'node
                             :ip      ipstr
                             :uuid    uuid
                             :skey    (gethash (third pkeyzkp) *pkey-skey*)
                             :pkeyzkp pkeyzkp
                             :parent  parent
                             )))
    (setf (gethash ipstr *ip-node*)   node
          (gethash uuid  *uuid-node*) node
          (gethash (third pkeyzkp) *pkey-node*) node)))

(defun need-to-specify ()
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
        (setf (gethash (third zkp) *pkey-skey*) skey)
        (values ip
                (uuid:make-v1-uuid)
                zkp)
        ))))
    
(defun make-node-tree (ip vlist &optional parent)
  (multiple-value-bind (ipstr uuid pkeyzkp)
      (gen-node-id ip)
    (let ((node (make-node ipstr uuid pkeyzkp parent)))
      (when vlist
        (let* ((ipv   (dotted-string-to-integer ipstr))
               (dists (mapcar (lambda (v)
                                (let* ((vstr (if (consp v)
                                                 (car v)
                                               v))
                                       (vv (dotted-string-to-integer vstr)))
                                  (logxor ipv vv)))
                              vlist))
               (bins  (make-array *bins-per-node* :initial-element nil)))
          (mapc (lambda (dist v)
                  (push v (aref bins (mod dist *bins-per-node*))))
                dists vlist)
          (loop for cell across bins
                for ix from 0
                do
                (when cell
                  (setf (aref bins ix) (make-node-tree (car cell) (cdr cell) node))))
          (setf (node-bins node) (delete nil (coerce bins 'list)))
          ))
      node)))

;; --------------------------------------------------------------------
;; for visual debugging...

#+:LISPWORKS
(progn
  (defmethod children (x layout)
    nil)
  
  (defmethod children ((node node) layout)
    (node-bins node))

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
    (if (gethash ip *ip-node*)
        (generate-ip) ;; should be unlikely, would be 50% at around 2^16 nodes
      (setf (gethash ip *ip-node*) ip))))

(defmethod pair-ip-uuid ((node node))
  (list (node-ip node)
        (node-uuid node)
        (node-pkeyzkp node)))

(defmethod pair-ip-uuid ((ip string))
  (pair-ip-uuid (gethash ip *ip-node*)))

(defun generate-tree (&key fname (nel 1000))
  (let* ((leader     *leader-node*)
         (real-nodes  (remove-duplicates *real-nodes*
                                         :test 'string=)))
    
    ;; ensure leader is in the real-nodes collection
    (assert (member leader real-nodes :test 'string=))
    
    ;; pre-populate hash table with real-nodes
    (clrhash *ip-node*)
    (clrhash *uuid-node*)
    (clrhash *pkey-node*)
    (clrhash *pkey-skey*)
    (dolist (ip real-nodes)
      (setf (gethash ip *ip-node*) ip))

    ;; build the trees
    (let* ((nreal       (length real-nodes))
           (nel/grp     (ceiling nel nreal))
           (grps        (loop repeat nreal collect
                              (loop repeat nel/grp collect
                                    (generate-ip))))
           (trees       (mapcar 'make-node-tree real-nodes grps))
           (main-tree   (find leader trees :test 'string= :key 'node-ip)))

      ;; mark the real nodes as special
      (mapc (lambda (tree)
              (setf (node-realnode tree) t))
            trees)
      
      ;; attach the non-leader real nodes to the leader node
      (dolist (tree (remove main-tree trees))
        (setf (node-parent tree) main-tree)
        (push tree (node-bins main-tree)))

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
                   *pkey-skey*)
          (with-standard-io-syntax
            (pprint keys f))))
      
      #+:LISPWORKS (view-tree main-tree)
      main-tree)))

;; ---------------------------------------------------------------
;; Tree reconstruction

(defun reconstruct-tree (&key fname)
  ;; read the keying file
  (let* ((keys        (read-from-string
                       (#+:OPENMCL   uiop/stream::read-file-string
                        #+:LISPWORKS hcl:file-string
                        #+:ALLEGRO   (need-to-specify)
                        (merge-pathnames
                         #+:LISPWORKS
                         (sys:get-folder-path :documents)
                         #+(OR :ALLEGRO :OPENMCL)
                         "~/Documents/"
                         *default-key-file*))))

         (data        (read-from-string
                       (#+:OPENMCL   uiop/stream::read-file-string
                        #+:LISPWORKS hcl:file-string
                        #+:ALLEGRO   (need-to-specify)
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
                 (let ((ipstr (car ip)))
                   (assert (null (gethash ipstr *ip-node*)))
                   (setf (gethash ipstr *ip-node*) ip)))))
      (clrhash *ip-node*)
      (clrhash *uuid-node*)
      (clrhash *pkey-node*)
      (no-dups real-nodes)
      (mapc #'no-dups grps))
    
    ;; reconstruct keying info
    (clrhash *pkey-skey*)
    (mapc (lambda (pair)
            (destructuring-bind (k . v) pair
              (setf (gethash k *pkey-skey*) v)))
          keys)
    
    ;; reconstruct the trees
    (let* ((trees       (mapcar 'make-node-tree real-nodes grps))
           (main-tree   (find leader trees
                              :test 'string=
                              :key  'node-ip)))

      ;; mark real nodes as special
      (mapc (lambda (tree)
              (setf (node-realnode tree) t))
            trees)

      ;; attach the real nodes to the leader
      (dolist (tree (remove main-tree trees))
        (setf (node-parent tree) main-tree)
        (push tree (node-bins main-tree)))
      
      #+:LISPWORKS (view-tree main-tree)
      main-tree)))
