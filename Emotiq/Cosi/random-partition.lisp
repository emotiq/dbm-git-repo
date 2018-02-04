
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

(defvar *bins-per-node* 9)

(defclass node ()
  ((ip       :accessor node-ip
             :initarg  :ip)
   (uuid     :accessor node-uuid
             :initarg  :uuid
             :initform (uuid:make-v1-uuid))
   (realnode :accessor node-realnode
             :initform nil)
   (parent   :accessor node-parent
             :initarg  :parent
             :initform nil)
   (bins     :accessor node-bins
             :initarg  :bins
             :initform nil)
   ))

;; XREF from IPv4 address to Tree Node
(defvar *ip-node*   (make-hash-table :test 'string=))
(defvar *uuid-node* (make-hash-table :test 'uuid:uuid=))

(defun make-node (ipstr uuid parent)
  (let ((node (make-instance 'node
                             :ip     ipstr
                             :uuid   uuid
                             :parent parent
                             )))
    (setf (gethash ipstr *ip-node*)   node
          (gethash uuid  *uuid-node*) node)))

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

(defun partition (ip vlist &optional parent)
  (let* ((ipstr  (if (consp ip)
                     (car ip)
                   ip))
         (uuid   (if (consp ip)
                     (cdr ip)
                   (uuid:make-v1-uuid)))
         (node (make-node ipstr uuid parent)))
    (when vlist
      (let* ((ipv   (dotted-to-integer ipstr))
             (dists (mapcar (lambda (v)
                              (let ((vstr  (if (consp v)
                                               (car v)
                                             v))
                                    (vuuid (if (consp v)
                                               (cdr v)
                                             (uuid:make-v1-uuid))))
                                (logxor ipv (dotted-to-integer vstr))))
                              vlist))
             (bins  (make-array *bins-per-node* :initial-element nil)))
        (mapc (lambda (dist v)
                (push v (aref bins (mod dist *bins-per-node*))))
              dists vlist)
        (loop for cell across bins
              for ix from 0
              do
              (when cell
                (setf (aref bins ix) (partition (car cell) (cdr cell) node))))
        (setf (node-bins node) (delete nil (coerce bins 'list)))
        ))
    node))

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

(defun generate-ip ()
  ;; generate a unique random IPv4 address
  (let ((ip (integer-to-dotted-string (random #.(expt 2 32)))))
    (if (gethash ip *ip-node*)
        (generate-ip) ;; should be unlikely, would be 50% at around 2^16 nodes
      (setf (gethash ip *ip-node*) ip))))

(defmethod pair-ip-uuid ((node node))
  (cons (node-ip node) (node-uuid node)))

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
    (dolist (ip real-nodes)
      (setf (gethash ip *ip-node*) ip))

    ;; build the trees
    (let* ((nreal       (length real-nodes))
           (nel/grp     (ceiling nel nreal))
           (grps        (loop repeat nreal collect
                              (loop repeat nel/grp collect
                                    (generate-ip))))
           (trees       (mapcar 'partition real-nodes grps))
           (main-tree   (find leader trees :test 'string= :key 'node-ip)))

      ;; mark the real nodes as special
      (mapc (lambda (tree)
              (setf (node-realnode tree) t))
            trees)
      
      ;; attach the non-leader real nodes to the leader node
      (dolist (tree (remove main-tree trees))
        (setf (node-parent tree) main-tree)
        (push tree (node-bins main-tree)))

      ;; save as a text file for later
      (with-open-file (f (merge-pathnames
                          #+:LISPWORKS
                          (sys:get-folder-path :documents)
                          #+:OPENMCL
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
      #+:LISPWORKS (view-tree main-tree)
      main-tree)))

;; ---------------------------------------------------------------
;; Tree reconstruction

(defun reconstruct-tree (&key fname)
  (let* ((data        (read-from-string
                       (#+:OPENMCL uiop/stream::read-file-string
                        #+:LISPWORKS file-string
                        (merge-pathnames
                         #+:LISPWORKS
                         (sys:get-folder-path :documents)
                         #+:OPENMCL
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
      (no-dups real-nodes)
      (mapc #'no-dups grps))

    ;; reconstruct the trees
    (let* ((trees       (mapcar 'partition real-nodes grps))
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
