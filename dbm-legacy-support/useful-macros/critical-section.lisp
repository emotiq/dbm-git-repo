
(in-package :um)

;; ------------------------------------------
;; A MONITOR is a region of code under control of a lock. Only one
;; thread at a time can be executing any one of the clauses of a
;; MONITOR.
;;
;; The clauses are written, just like a defun, with a name, and
;; arglist, and a body.
;;
;; Each clause will become a defun aimed at the shared critical
;; region.
;;
;; NOTE: rather than using (&rest clauses) we use (clauses) as the
;; argument in order to gain the formatting support of the editor.
;; DEFMONITOR should be written like an FLET with all clauses enclosed
;; in an outer list.

#+:LISPWORKS
(defmacro! defmonitor (clauses)
  `(let* ((,g!lock (mp:make-lock))
          (,g!lam  (lambda (&rest ,g!args)
                     (mp:with-lock (,g!lock)
                       (dcase ,g!args
                         ,@clauses)))
                   ))
     ,@(mapcar (lambda (clause)
                 (let ((fname (first clause)))
                   `(defun ,fname (&rest ,g!fargs)
                      (apply ,g!lam ',fname ,g!fargs))))
               clauses)
     ))

#+:LISPWORKS
(editor:setup-indent "DEFMONITOR" 1 nil nil 'flet)

#+:ALLEGRO
(defmacro! defmonitor (clauses)
  `(let* ((,g!lock (mp:make-process-lock))
          (,g!lam  (lambda (&rest ,g!args)
                     (mp:with-process-lock (,g!lock)
                       (dcase ,g!args
                         ,@clauses)))
                   ))
     ,@(mapcar (lambda (clause)
                 (let ((fname (first clause)))
                   `(defun ,fname (&rest ,g!fargs)
                      (apply ,g!lam ',fname ,g!fargs))))
               clauses)
     ))

;; ----------------------------------------------------------

#+:LISPWORKS
(defun do-with-spinlock (cons fn)
  (unwind-protect
      (progn
        (do ()
            ((and (eq nil (car cons))
                  ;; Spin on fetch until we think we have a chance of
                  ;; succeeding with the CAS. This avoids excessive
                  ;; bus traffic.
                  (sys:compare-and-swap (car cons) nil mp:*current-process*))))
        (funcall fn))
    (sys:compare-and-swap (car cons) mp:*current-process* nil)))

#+:LISPWORKS
(defmacro! with-spinlock (cons &body body)
  `(flet ((,g!body ()
            ,@body))
     (declare (dynamic-extent #',g!body))
     (do-with-spinlock ,cons #',g!body)))

#+:LISPWORKS
(defmacro! defsponitor (clauses)
  `(let* ((,g!lock (list nil))
          (,g!lam  (lambda (&rest ,g!args)
                     (with-spinlock ,g!lock
                       (dcase ,g!args
                         ,@clauses)))
                   ))
     ,@(mapcar (lambda (clause)
                 (let ((fname (first clause)))
                   `(defun ,fname (&rest ,g!fargs)
                      (apply ,g!lam ',fname ,g!fargs))))
               clauses)
     ))

#+:LISPWORKS
(editor:setup-indent "DEFSPONITOR" 1 nil nil 'flet)

;; ----------------------------------------------------------

#|
(defsponitor ()
  ((ensure-access ()
     (doit))
   (diddle-access (arg)
     (doit2 arg))))
==>
(LET* ((#:LOCK46671 (MP:MAKE-LOCK))
       (#:LAM46670  (LAMBDA (&REST #:ARGS46669)
                      (MP:WITH-LOCK (#:LOCK46671)
                        (DCASE #:ARGS46669
                          (ENSURE-ACCESS NIL
                                         (DOIT))
                          (DIDDLE-ACCESS (ARG)
                                         (DOIT2 ARG))
                          )))
                    ))
  (DEFUN ENSURE-ACCESS (&REST #:FARGS46668)
    (APPLY #:LAM46670 'ENSURE-ACCESS #:FARGS46668))
  (DEFUN DIDDLE-ACCESS (&REST #:FARGS46668)
    (APPLY #:LAM46670 'DIDDLE-ACCESS #:FARGS46668)))
|#
