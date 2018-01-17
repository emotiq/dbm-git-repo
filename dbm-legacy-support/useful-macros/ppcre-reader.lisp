
(in-package :useful-macros)

;; ---------------------------------------
;; This part from Doug Hoyte using Edi's ppcre

(defun segment-reader (stream ch n)
  (if (> n 0)
      (let ((chars))
        (do ((curr (read-char stream)
                   (read-char stream)))
            ((char= ch curr))
          (push curr chars))
        (cons (coerce (nreverse chars) 'string)
              (segment-reader stream ch (- n 1))))))

;; ---------------------------------------

#+cl-ppcre
(defmacro! match-mode-ppcre-lambda-form (o!args)
  ``(lambda (,',g!str)
      (cl-ppcre:scan
       ,(car ,g!args)
       ,',g!str)))

#+cl-ppcre
(defmacro! subst-mode-ppcre-lambda-form (o!args)
  ``(lambda (,',g!str)
      (cl-ppcre:regex-replace-all
       ,(car ,g!args)
       ,',g!str
       ,(cadr ,g!args))))

;; Reader macro for #~ for pattern matching/substitution
;; Produces a function that can be applied to strings
  
#+cl-ppcre
(defun |reader-for-#~| (stream sub-char numarg)
  (declare (ignore sub-char numarg))
  (let* ((mode-char (read-char stream))
         (ans   (cond
                 
                 ((char= mode-char #\m)
                  (match-mode-ppcre-lambda-form
                   (segment-reader stream
                                   (read-char stream)
                                   1)))
                   
                 ((char= mode-char #\s)
                  (subst-mode-ppcre-lambda-form
                   (segment-reader stream
                                   (read-char stream)
                                   2)))
                 
                 (t
                  (unless *read-suppress*
                    (error "Unknown #~~ mode character")))
                 )))
    (unless *read-suppress*
      ans)))

#+cl-ppcre
(set-dispatch-macro-character #\# #\~ '|reader-for-#~|)

#| ;; example
;; pattern matching
(#~m/^[+-]?[0-9][0-9_,]*(\.[0-9_,]*([eEdD][+-]?[0-9]+)?)?/ s)
;; pattern substitution
(#~s/[0-9]/N/ s)
|#

