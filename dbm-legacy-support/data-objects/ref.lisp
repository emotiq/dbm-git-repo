;; ref.lisp -- SMP indirect refs
;;
;; DM/RAL  02/17
;; -------------------------------------------------------------

#|
(defpackage #:ref
  (:use #:common-lisp)
  (:export
   #:ref
   #:car-ref
   #:cdr-ref
   #:ref-value
   #:cas
   #:mref
   #:atomic-incf
   #:atomic-decf
   #:raw-car-ref
   #:raw-cdr-ref
   ))
|#

(in-package #:ref)
   
(declaim (optimize (speed 3) (safety 0) #+:LISPWORKS (float 0)))

;; ====================================================================

(defclass ref-mixin ()
  ((cell :reader ref-cell :initarg :cell)))

;; ----------------------------------------

(defclass car-ref (ref-mixin)
  ())

(defmethod car-ref (x)
  (make-instance 'car-ref
                 :cell (list x)))

(defmethod ref-value (x)
  x)

(defmethod ref-value ((ref car-ref))
  (car (the cons (ref-cell ref))))

(defmethod cas ((ref car-ref) old new)
  (mpcompat:CAS (car (the cons (ref-cell ref))) old new))

(defmethod raw-car-ref ((cell cons))
  (make-instance 'car-ref
                 :cell cell))

;; -----------------------------------------
;; REF - a mostly read-only indirect reference cell
;; can only be mutated through CAS

(defclass ref (car-ref)
  ())

(defun ref (x)
  (make-instance 'ref
                 :cell (list x)))

(defmethod car-ref ((ref ref))
  ref)

;; ------------------------------------------
;; MREF - mutable REF can be SETF directly

(defclass mref (ref)
  ())

(defun mref (x)
  (make-instance 'mref
                 :cell (list x)))

(defmethod set-ref-value ((ref mref) val)
  (setf (car (the cons (ref-cell ref))) val))

(defsetf ref-value set-ref-value)

(defmethod atomic-incf ((ref mref))
  #+:LISPWORKS
  (system:atomic-fixnum-incf (car (the cons (ref-cell ref))))
  #+:ALLEGRO
  (excl:incf-atomic (car (the cons (ref-cell ref)))))

(defmethod atomic-decf ((ref mref))
  #+:LISPWORKS
  (system:atomic-fixnum-decf (car (the cons (ref-cell ref))))
  #+:ALLEGRO
  (excl:decf-atomic (car (the cons (ref-cell ref)))))

;; -----------------------------------------

(defclass cdr-ref (ref-mixin)
  ())

(defmethod cdr-ref ((ref ref-mixin))
  (make-instance 'cdr-ref
                 :cell (ref-cell ref)))

(defmethod raw-cdr-ref ((cell cons))
  (make-instance 'cdr-ref
                 :cell cell))

(defmethod ref-value ((ref cdr-ref))
  (cdr (the cons (ref-cell ref))))

(defmethod cas ((ref cdr-ref) old new)
  (mpcompat:CAS (cdr (the cons (ref-cell ref))) old new))


;; -----------------------------------------

(defmethod um:rmw ((ref ref-mixin) val-fn)
  ;; Generic Read-Modify-Write of Refs. It must be the case that the
  ;; val-fn does not destructively modify the value held in the ref
  ;; object.  And it should not generate non-idempotent side effects,
  ;; since the call to val-fn may occur more than once in the event of
  ;; access collisions.
  (declare (function val-fn))
  (loop for old = (ref-value ref)
        for new = (funcall val-fn old)
        until (cas ref old new)))

