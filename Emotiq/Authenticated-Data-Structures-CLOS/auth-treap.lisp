;; auth-treap.lisp -- Authenticated Treaps
;;
;; DM/Emotiq  01/18
;; ------------------------------------------------------------------

(in-package :ads)

(defclass treap ()
  ((prio
    :reader   treap-prio
    :initarg  :prio)
   (key
    :reader   treap-key
    :initarg  :key)
   (val
    :reader   treap-val
    :initarg  :val)
   (left
    :accessor treap-left
    :initarg  :left
    :initform nil)
   (right
    :accessor treap-right
    :initarg  :right
    :initform nil)))

(defmethod shallow ((node treap))
  (make-instance 'treap
                 :prio  (treap-prio node)
                 :key   (treap-key node)
                 :val   (treap-val node)
                 :left  (shallow (treap-left node))
                 :right (shallow (treap-right node)) ))

(defmethod fetch ((node treap) (path cons))
  (fetch (ecase (car path)
           (:L (treap-left node))
           (:R (treap-right node)))
         (cdr path)))

(defmethod fetch ((node treap) (path null))
  (treap-val node))

;; ------------------------------------------------------------

(defmethod treap-prio-for-item (item)
  ;; default method - subclass should override
  (hash item))

(defmethod treap-key-for-item (item)
  ;; default method - subclass should override
  item)

;; ------------------------------------------------------------

(defun clone-node (node &key
                        (left  (treap-left node))
                        (right (treap-right node)))
  (auth
   (make-instance 'treap
                  :prio  (treap-prio node)
                  :key   (treap-key node)
                  :val   (treap-val node)
                  :left  left
                  :right right)))

;; ------------------------------------------------------------
#|
(defun make-authenticated-treap (&rest items)
  ;; construct a fully authenticated treap from a list of items
  (labels ((insert-items (items tree)
             (if (endp items)
                 (auth tree)
               (let* ((item (car items))
                      (key  (treap-key-for-item item)))
                 (insert-items (cdr items)
                               (if tree
                                   ()
                                 ;; else - empty tree
                                 (make-instance 'treap
                                                :key key
                                                :val item))))
               )))
    (insert-items items nil)))
|#

;; ------------------------------------------------------------
;; PROBE -- get past the Auth-Type envelope to the contained item

(defmethod probe (x)
  x)

(defmethod probe ((x prover))
  (probe (prover-val x)))

(defmethod probe ((x digest))
  (error "Can't probe a Digest"))

;; ------------------------------------------------------------
;; INSERT - add/update an item in the Treap. Purely functional, no
;; mutation of original Treap. Can't be usefully applied by Verifiers.

(defmethod insert (item tree)
  (insert
   (make-instance 'treap
                  :prio (treap-prio-for-item item)
                  :key  (treap-key-for-item item)
                  :val  item)
   tree))

(defmethod insert ((node treap) (tree null))
  (auth node))

(defmethod insert (node (tree prover))
  (insert node (probe tree)))

(defmethod insert ((node treap) (tree treap))
  (let* ((node-key (treap-key node))
         (top-key  (treap-key tree))
         (kcmp     (compare node-key top-key)))
    (cond ((zerop kcmp)
           ;; replace existing by node
           (clone-node node
                       :left  (treap-left tree)
                       :right (treap-right tree)))

          ((minusp kcmp)
           (let* ((left-child (insert node (treap-left tree)))
                  (tleft      (probe left-child))
                  (pcmp       (compare (treap-prio tleft) (treap-prio tree))))
             (cond ((plusp pcmp)
                    (let ((new-tree (clone-node tree
                                                :left (treap-right tleft))))
                      (clone-node tleft
                                  :right new-tree)))
                   (t
                    (clone-node tree
                                :left left-child))
                   )))

          ((plusp kcmp)
           (let* ((right-child (insert node (treap-right tree)))
                  (tright      (probe right-child))
                  (pcmp        (compare (treap-prio tright) (treap-prio tree))))
             (cond ((plusp pcmp)
                    (let ((new-tree (clone-node tree
                                                :right (treap-left tright))))
                      (clone-node tright
                                  :left new-tree)))
                   (t
                    (clone-node tree
                                :right right-child))
                   )))
          )))

(defun make-authenticated-treap (&rest items)
  (labels ((iter (items tree)
             (if (endp items)
                 tree
               (iter (cdr items) (insert (car items) tree)))
             ))
    (iter items nil)))

;; --------------------------------------------------------------------
;; for visual debugging...

#+:LISPWORKS
(defmethod treap-children (node layout)
  nil)

#+:LISPWORKS
(defmethod treap-children ((node prover) layout)
  (treap-children (probe node) layout))

#+:LISPWORKS
(defmethod treap-children ((node treap) layout)
  (multiple-value-bind (l r)
      (values (treap-left node)
              (treap-right node))
    (let ((lx (or l
                  (vector)))
          (rx (or r
                  (vector))))
      (cond ((and (null l)
                  (null r))
             nil)
            (t
             (case layout
               ((:left-right :right-left) (list rx lx))
               (t   (list lx rx))))
            ))))

#+:LISPWORKS
(defmethod print-node (x keyfn)
  nil)

#+:LISPWORKS
(defmethod print-node ((node prover) keyfn)
  (print-node (probe node) keyfn))

#+:LISPWORKS
(defmethod print-node ((node treap) keyfn)
  (format nil "~A" (funcall keyfn (treap-key node))))

#+:LISPWORKS
(defmethod view-treap ((tree prover) &key (key #'identity) (layout :left-right))
  (view-treap (probe tree) :key key :layout layout))

#+:LISPWORKS
(defmethod view-treap ((tree treap) &key (key #'identity) (layout :left-right))
  (capi:contain
   (make-instance 'capi:graph-pane
                  :layout-function layout
                  :roots (list tree)
                  :children-function (lambda (node)
                                       (treap-children node layout))
                  :print-function (lambda (node)
                                    (print-node node key))
                  )))

;; --------------------------------------------------------------------

#|
(let* ((items '(:one :two :three :four :five :six :seven))
       (tree  (apply 'make-authenticated-treap items)))
  (inspect tree)
  (view-treap tree))
 |#
