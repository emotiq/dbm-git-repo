
(declaim (optimize (debug 3)))

(defvar *bins-per-node* 9)

(defclass node ()
  ((v0       :accessor node-v0
             :initarg  :v0)
   (realnode :accessor node-realnode
             :initform nil)
   (bins     :accessor node-bins
             :initarg  :bins
             :initform nil)))

(defun partition (v0 vlist)
  (if vlist
      (let* ((dists (mapcar (um:curry 'logxor v0) vlist))
             (bins  (make-array *bins-per-node* :initial-element nil)))
        (mapc (lambda (dist v)
                (push v (aref bins (mod dist *bins-per-node*))))
              dists vlist)
        (loop for cell across bins
              for ix from 0
              do
              (when cell
                (setf (aref bins ix) (partition (car cell) (cdr cell)))))
        (make-instance 'node
                       :v0   v0
                       :bins (delete nil (coerce bins 'list))))
    ;; else
    (make-instance 'node
                   :v0 v0)))


;; --------------------------------------------------------------------
;; for visual debugging...

(defun dotted-to-integer (string)
  #+:LW (comm:string-ip-address string)
  #+:OPENMCL (ccl::dotted-to-ipaddr string))

(defvar *malachite*  (dotted-to-integer "10.0.1.6"))
(defvar *dachshund*  (dotted-to-integer "10.0.1.3"))
(defvar *rambo*      (dotted-to-integer "10.0.1.13"))

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
    (let ((txt (format nil "~{~d~^.~}" (split-to-octets (node-v0 node)))))
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

(defvar *default-data-file* "cosi-nodes.txt")

(defun generate-tree (&key fname (nel 1000))
  (let* ((leader     *dachshund*)
         (real-nodes  (list *dachshund* *malachite* *rambo*))
         (nreal       (length real-nodes))
         (nel/grp     (ceiling nel nreal))
         (grps        (loop repeat nreal collect
                            (loop repeat nel/grp collect
                                  (random #.(expt 2 32)))))
         (trees       (mapcar 'partition real-nodes grps))
         (main-tree   (find leader trees :key 'node-v0)))
    (mapc (lambda (tree)
            (setf (node-realnode tree) t))
          trees)
    (dolist (tree (remove main-tree trees))
      (push tree (node-bins main-tree)))
    (with-open-file (f (merge-pathnames
                        #+:LW
                         (sys:get-folder-path :documents)
                         #+:OPENMCL
                         "~/Documents/"
                        (or fname *default-data-file*))
                       :direction :output
                       :if-does-not-exist :create
                       :if-exists :supersede)
      (with-standard-io-syntax
        (pprint `(:leader     ,leader
                  :real-nodes ,real-nodes
                  :groups     ,grps)
                f)))
    #+:LW (view-tree main-tree)
    main-tree))

  
(defun reconstruct-tree (&key fname)
  (let* ((data        (read-from-string
                       (#+:OPENMCL uiop/stream::read-file-string
                        #+:LW file-string
                        (merge-pathnames
                         #+:LW
                         (sys:get-folder-path :documents)
                         #+:OPENMCL
                         "~/Documents/"
                         (or fname *default-data-file*)))))
         (leader      (getf data :leader))
         (real-nodes  (getf data :real-nodes))
         (grps        (getf data :groups))
         (trees       (mapcar 'partition real-nodes grps))
         (main-tree   (find leader trees :key 'node-v0)))
    (mapc (lambda (tree)
            (setf (node-realnode tree) t))
          trees)
    (dolist (tree (remove main-tree trees))
      (push tree (node-bins main-tree)))
    #+:LW (view-tree main-tree)
    main-tree))
