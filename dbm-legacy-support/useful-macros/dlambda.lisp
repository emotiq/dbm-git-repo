;; ----------------------------------------------------------------------

#|
;;
;; This is Doug Hoyte's implementation of DLAMBDA...
;;
(defmacro! dlambda (&rest ds)
  `(lambda (&rest ,g!args)
     (let ((,g!tail (cdr ,g!args)))
       (case (car ,g!args)
         ,@(mapcar
            (lambda (d)
              `(,(if (eq t (car d))
                     t
                   (list (car d))
                   )
                (apply (lambda ,@(cdr d))
                       ,(if (eq t (car d))
                            g!args
                          g!tail)) ))
            ds)))))

(defmacro dcase (args &rest clauses)
  `(apply (dlambda
            ,@clauses)
          ,args))

#+:LISPWORKS
(editor:setup-indent "dcase" 1)
|#

;; ----------------------------------------------------------------------
;; Here is a faster implementation on LispWorks
;;

(defun bad-selector (&rest args)
  (declare (ignore args))
  (error "Invalid selector"))

(defun make-jv-dispatcher (jv &optional default)
  ;; generalized (named) jump vectors
  ;; jv is plist of alternating selector symbols, and functions or closures
  ;; default should be a function or nil, called when no selectors match
  (if default
      (lambda (&rest args)
        (if-let (fn (getf jv (first args) nil))
            (apply fn (rest args))
          (apply default args)))
    ;; else
    (lambda (sel &rest args)
      (apply (getf jv sel 'bad-selector) args))
    ))

;; ----------------------------------------------------------------------

(defmacro! dlambda (&rest ds)
  (let* ((dsels   (mapcar #'first ds))
         has-default
         (dfnames (mapcar (lambda (sel)
                            (if (eq sel t)
                                (setf has-default g!default)
                              (gensym (string sel))))
                          dsels)))
    `(labels
         ,(mapcar (lambda (dfname clause)
                    `(,dfname ,@(rest clause)))
                  dfnames ds)
       (declare (inline ,@dfnames))
       (make-jv-dispatcher (list ,@(mapcan (lambda (dsel dfname)
                                             (unless (eq dsel t)
                                               `(',dsel #',dfname)))
                                           dsels dfnames))
                           ,@(when has-default
                               `(#',g!default)))
       )))

(defmacro dcase (args &rest clauses)
  `(apply (dlambda
            ,@clauses)
          ,args))

#+:LISPWORKS
(editor:setup-indent "DCASE" 1)

;; ----------------------------------------------------------------------

