;; auth-list.lisp -- Authenticated lists
;;
;; DM/Emotiq  01/18
;; -----------------------------------------------------------

(in-package :ads)

(defclass auth-list ()
  ((lst  :reader  auth-list-lst
         :initarg :lst)))

(defun make-auth-list (&rest items)
  (auth (make-instance 'auth-list
                       :lst (mapcar 'auth items))))

(defmethod shallow ((alst auth-list))
  (make-instance 'auth-list
                 :lst (mapcar 'shallow (auth-list-lst alst))))
              
(defmethod fetch ((alst auth-list) (path cons))
  ;; a path here is a list of (:T :T :T :H), always ending in :H
  (labels ((iter (lst path)
             (ecase (car path)
               (:H  (fetch (car lst) (cdr path)))
               (:T  (iter (cdr lst) (cdr path)))
               )))
    (iter (auth-list-lst alst) path)))

(defmethod fetch ((alst auth-list) (path null))
  (error "Non-empty path expected"))

#|
(let* ((alst (make-auth-list :one :two :three))
       (path '(:T :H)))
  (multiple-value-bind (item wlist)
      (prove (fetch alst path))
    (verify wlist (fetch (shallow alst) path))))
 |#
