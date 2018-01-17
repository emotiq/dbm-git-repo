;; interval-trees.lisp -- RB Trees used for holding intervals (start len)

(in-package #:interval-trees)

(defmethod find-containing (key (map sets:empty) &optional default)
  (values default nil))

(defmethod find-containing (key (map sets:node) &optional default)
  ;; key should be a pair (start len)
  ;; returns the object from the cell that has an interval key
  ;; which contains the interval lookup key
  #f
  (destructuring-bind (kstart klen) key
    (sets:with-node-bindings (l v r) map
      (let ((c (ord:compare key (maps:map-cell-key v))))
        (cond ((zerop c)  (values (maps:map-cell-val v) t))
              ((minusp c)
               ;; key < interval
               ;; can happen if (1) kstart < istart => try left branch
               ;;            or (2) kstart = istart && kend < iend => we found it
               (let ((istart (car (maps:map-cell-key v))))
                 (cond ((= kstart istart)                  ;; K |---|
                        (values (maps:map-cell-val v) t))  ;; I |-----|
                       (t                                  ;; K |----|
                        (find-containing key l default))   ;; I   |---|
                       )))
              (t
               ;; key > interval
               ;; can happen if (1) kstart = istart && kend > iend => try right brancch
               ;;            or (2) kstart > istart 
               ;;
               (destructuring-bind (istart ilen) (maps:map-cell-key v)
                 (cond ((<= (+ kstart klen)                  ;; K    |--|
                            (+ istart ilen))                 ;; I   |-----|
                        (values (maps:map-cell-val v) t))
                       (t                                    ;; K |------| or    |-----|
                        (find-containing key r default))     ;; I |---|       |-----|
                       )))
              )))
    ))

#|
(let ((m (maps:add '(0 4) 'one
              (maps:add '(3 4) 'two
                   (maps:add '(5 10) 'three (maps:empty))))))
  (find-containing '(5 3) m))
|#

;; ---------------------------------------------------------------------------------

