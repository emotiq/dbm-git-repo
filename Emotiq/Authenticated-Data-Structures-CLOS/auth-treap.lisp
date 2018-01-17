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
    :initval  nil)
   (right
    :accessor treap-right
    :initarg  :right
    :initval  nil)))

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

(defmethod insert (item tree)
  (insert
   (make-instance 'treap
                  :prio (treap-prio-for-item item)
                  :key  (treap-key-for-item item)
                  :val  item)
   tree))

(defmethod insert ((node treap) (tree null))
  (auth node))

(defmethod insert (node ((tree auth-type)))
  (insert node (unauth tree)))

(defmethod insert ((node treap) (tree treap))
  (let* ((node-key (treap-key node))
         (top-key  (treap-key tree))
         (kcmp     (compare node-key top-key)))
    (cond ((zerop kcmp)
           (setf (treap-left node) (treap-left tree)
                 (treap-right node) (treap-right tree))
           (auth node))

          ((minusp kcmp)
           (let* ((left-child (insert node (treap-left tree)))
                  (tleft      (probe left-child))
                  (pcmp       (compare (treap-prio tleft) (treap-prio tree))))
             (cond ((plusp pcmp)
                    (setf (treap-left tree) (treap-right tleft)
                          (treap-right tleft) (auth tree))
                    (auth tleft))
                   (t
                    (setf (treap-left tree) left-child)
                    (auth tree))
                   )))

          ((plusp kcmp)
           (let* ((right-child (insert node (treap-right tree)))
                  (tright      (probe right-child))
                  (pcmp        (compare (treap-prio tright) (treap-prio tree))))
             (cond ((plusp pcmp)
                    (setf (treap-right tree) (treap-left tright)
                          (treap-left tright) (auth tree))
                    (auth tright))
                   (t
                    (setf (treap-right tree) right-child)
                    (auth tree))
                   )))
          )))
