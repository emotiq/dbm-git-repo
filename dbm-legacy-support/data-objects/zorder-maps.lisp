
(defpackage #:zorder-maps
  (:use #:common-lisp)
  (:export
   #:zorder-key
   #:make-zorder-key
   #:zorder-key-p
   #:zorder-key-coord
   ))

(in-package #:zorder-maps)

(defun less-msb (x y)
  (and (< x y)
       (< x (logxor x y))))

(defun cmp-zorder(a b)
  (let ((ndim  (length a)))
    ;; a, b are lists of equal length representing coordinates in N dimensions
    (assert (= ndim (length b)))
    (let ((j  0)
          (x  0))
      (loop for k from 0 below ndim do
            (let ((y (logxor (elt a k) (elt b k))))
              (when (less-msb x y)
                (setf j k
                      x y))
              ))
      (- (elt a j) (elt b j))
      )))

(defstruct zorder-key
  coord)

(defmethod ord:compare ((a zorder-key) (b zorder-key))
  (cmp-zorder (zorder-key-coord a) (zorder-key-coord b)))

#|
(let* ((coords (loop for x from 0 below 8 nconc
                     (loop for y from 0 below 8 collect
                           (list x y))))
       (sorted (sort coords (um:compose #'minusp #'cmp-zorder)))
       (zmap   (maps:empty)))
  (loop for coord in coords do
        (setf zmap (maps:add (make-zorder-key
                              :coord coord)
                             coord zmap)))
  (sets::view-set zmap)
  (plt:plot 'plt
            (mapcar #'first sorted)
            (mapcar #'second sorted)
            :clear t
            :symbol :circle
            :plot-joined t)
  (inspect sorted))
|#
