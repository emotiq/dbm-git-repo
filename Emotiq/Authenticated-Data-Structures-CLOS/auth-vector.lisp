;; auth-vector.lisp -- Authenticated vectors
;;
;; DM/Emotiq  01/18
;; -----------------------------------------------------------

(in-package :ads)

(defclass auth-vector ()
  ((vec  :reader  auth-vector-vec
         :initarg :vec)))

(defun make-auth-vector (&rest items)
  (auth (make-instance 'auth-vector
                       :vec (make-array (length items)
                                        :initial-contents (mapcar 'auth items)))))

(defmethod shallow ((av auth-vector))
  (let* ((vec     (auth-vector-vec av))
         (new-vec (make-array (length vec))))
    (make-instance 'auth-vector
                   :vec (map-into new-vec 'shallow vec))))
              
(defmethod fetch ((vec auth-vector) (path cons))
  (fetch (aref (auth-vector-vec vec) (car path)) (cdr path)))

(defmethod fetch ((vec auth-vector) (path null))
  (error "Non-empty path expected"))


#|
(let* ((v    (make-auth-vector :one :two :three))
       (path '(1)))
  (multiple-value-bind (item wlist)
      (prove (fetch v path))
    (verify wlist (fetch (shallow v) path))))
 |#
