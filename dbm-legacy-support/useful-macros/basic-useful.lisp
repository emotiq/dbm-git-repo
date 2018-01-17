
(in-package :useful-macros)

;; in ML these are referred to as sections
;; these actually correspond to the Dylan operators
;; secr ::= rcurry, secl ::= curry
(defun curry (fn &rest pref-args)
  (lambda (&rest suf-args)
    ;; using M-V-C we don't need to append the arg lists
    (multiple-value-call fn (values-list pref-args) (values-list suf-args))))

(defun rcurry (fn &rest suf-args)
  (lambda (&rest pref-args)
    (multiple-value-call fn (values-list pref-args) (values-list suf-args))))

(defun make-rubber-vector (&key (length 16) element-type)
  (make-array length
              :fill-pointer 0
              :adjustable   t
              :element-type element-type))

(defmacro named-lambda (name lambda-list &body body)
  `(labels ((,name ,lambda-list ,@body))
     #',name))

  ;; --------------------------------------------

(defun raw-mkstr (&rest args)
  (with-output-to-string (s)
    (dolist (a args) 
      (princ a s))
    ))
  
(defun mkstr (&rest args)
  (with-standard-io-syntax
    (apply 'raw-mkstr args)))

;; ----------------------------------------------
;; Symbology...

(defun correct-for-symbol-character-case (str)
  ;; a portable way to make symbol strings
  ;; Modern Mode vs ANSI
  (if (eql #\a (char (string :a) 0))
      (string-downcase (string str))
    (string-upcase (string str))))

(defun intern-symbol (str &rest package)
  (apply 'intern (correct-for-symbol-character-case str) package))

(defun symb (&rest args)
  (values (intern-symbol (apply #'mkstr args))))

(defun kwsymb (&rest args)
  (values (intern-symbol (apply #'mkstr args) (find-package :keyword))))

(defmethod kwsymbol ((name string))
  (intern-symbol name (find-package :keyword)))
(defmethod kwsymbol ((sym symbol))
  (if (keywordp sym)
      sym
    (intern (symbol-name sym) (find-package :keyword))))
(defmethod kwsymbol ((cons cons))
  (if (eq 'quote (car cons))
      (kwsymbol (cadr cons))
    (call-next-method)))

(defun symbol-gensym (s)
  (gensym (format nil "~A-" (symbol-name s))))
  
(defmacro with-gensyms (syms &body body)
  `(let ,(mapcar (lambda (sym)
                   `(,sym  (symbol-gensym ',sym)))
                 syms)
     ,@body))

(defun gensyms (lst)
  (mapcar (lambda (_)
            (declare (ignore _))
            (gensym))
          lst))

;; ----------------------------------------------

(defun flatten (x)
  ;; this is really a deep flatten
  (labels ((rec (x acc)
             (cond ((null x) acc)
                   ((atom x) (cons x acc))
                   (t  (rec (car x) (rec (cdr x) acc)))
                   )))
    (rec x nil)))

(defmacro perform (name bindings &body body)
  (let ((args (mapcar 'first bindings))
        (vals (mapcar 'second bindings)))
    `(labels ((,name ,args ,@body))
       (,name ,@vals))
    ))
  
(defmacro nlet (name bindings &body body)
  ;; NLET = Named LET
  `(perform ,name ,bindings ,@body))
  
(defun collect-decls (forms)
  (nlet iter ((forms forms)
              (decls nil))
    (let ((form (car forms)))
      (if (or (stringp form)
              (and (consp form)
                   (eq (car form) 'declare)))
          (iter (cdr forms) (cons form decls))
        (values forms (nreverse decls))
        ))))

;; -------------------------------------------

