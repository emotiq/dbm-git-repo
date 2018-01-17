;; bb.lisp -- Binding Block, ideas adapted from Ron Garrett (CCL)
;;
;; DM/RAL  04/17
;; ----------------------------------------------------------------

(defpackage #:bb
  (:use #:common-lisp)
  (:export
   #:bb
   #:define-handler
   ))

(in-package #:bb)

;; ---------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun handler (symbol)
    (get symbol 'bb-handler)))

(defmacro define-handler ((symbol clauses-args) &body body)
  `(setf (get ,symbol 'bb-handler)
         (lambda (,clauses-args)
           ,@body)))

#+:LISPWORKS
(editor:setup-indent "define-handler" 1)

(defmacro bb (&rest clauses)
  (when clauses
    (let ((clause (car clauses)))
      (cond ((consp clause)
             (if (cdr clauses)
                 `(progn
                    ,clause
                    (bb ,@(cdr clauses)))
               clause))
            
            ((keywordp clause)
             (let ((fn (handler clause)))
               (if fn
                   (funcall fn clauses)
                 (error "Invalid BB type ~S" clause))))
            
            ((symbolp clause)
             ;; LET* clause
             `(let* ((,clause ,(cadr clauses)))
                (bb ,@(cddr clauses))))
            
            (t
             (error "Invalid BB syntax"))
            ))))

;; ---------------------------------------------------------

(define-handler (:db args)
  (destructuring-bind (kw pat form &rest clauses) args
    (declare (ignore kw))
    `(destructuring-bind ,pat ,form
       (bb ,@clauses))))

(define-handler (:mv args)
  (destructuring-bind (kw pat form &rest clauses) args
    (declare (ignore kw))
    `(multiple-value-bind ,pat ,form
       (bb ,@clauses))))

(define-handler (:ac args)
  (destructuring-bind (kw pat form &rest clauses) args
    (declare (ignore kw))
    `(with-accessors ,pat ,form
       (bb ,@clauses))))

(define-handler (:sl args)
  (destructuring-bind (kw pat form &rest clauses) args
    (declare (ignore kw))
    `(with-slots ,pat ,form
       (bb ,@clauses))))

(define-handler (:fn args)
  (destructuring-bind (kw (name args &body body) &rest clauses) args
    (declare (ignore kw))
    `(flet ((,name ,args
              ,@body))
       (bb ,@clauses))))
    
(define-handler (:rfn args)
  (destructuring-bind (kw (name args &body body) &rest clauses) args
    (declare (ignore kw))
    `(labels ((,name ,args
                ,@body))
       (bb ,@clauses))))

#|
(bb
 x 15
 :ac ((lock thing-lock)
      (val  thing-val)) it
 (print x))
 |#

