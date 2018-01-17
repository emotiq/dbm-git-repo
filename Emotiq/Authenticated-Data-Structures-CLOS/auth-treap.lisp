;; auth-treap.lisp -- Authenticated Treaps
;;
;; DM/Emotiq  01/18
;; ------------------------------------------------------------------

(in-package :ads)

(defclass treap ()
  ((prob
    :reader   treap-prob
    :initarg  :prob)
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
                 :prob  (treap-prob node)
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

(defmethod insert (item tree)
  (insert
   (make-instance 'treap
                  :prob (treap-prob-for-item item)
                  :key  (treap-key-for-item item)
                  :val  item)
   tree))

(defmethod insert ((node treap) (tree null))
  (auth node))

(defmethod insert ((node treap) (tree treap))
  (let* ((node-key (treap-key node))
         (top-key  (treap-key tree))
         (cmp      (compare node-key top-key)))
    (cond ((zerop cmp)
           (setf (treap-left node) (treap-left tree)
                 (treap-right node) (treap-right tree))
           (auth node))

          ((minusp cmp)
           (let ((pcmp (compare (treap-prob node) (treap-prob tree))))
             (cond ((plusp pcmp)
                    
             (insert node (treap-left tree)))

          ((plusp cmp)
           (insert node (treap-right tree)))
                              
