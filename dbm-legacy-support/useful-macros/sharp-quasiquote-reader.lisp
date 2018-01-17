
(in-package :useful-macros)

;; --------------------------------------------
;; Reader macro for #` for producing parameterized BQ lists
;; Produces a function that can be applied to arguments
  
(defun |reader-for-#`| (stream sub-char numarg)
  (declare (ignore sub-char))
  (let* ((numarg (or numarg 1))
         (a-args (loop for i from 1 to numarg
                       collect (symb 'a i)))
         (ans  `(lambda ,a-args
                  (declare (ignorable ,@a-args))
                  ,(funcall
                    (get-macro-character #\`) stream nil))))
    (unless *read-suppress*
      ans)))

(set-dispatch-macro-character
 #\# #\` '|reader-for-#`|)
  

#| ;; example -- a1 is first parameter
(mapcar #`(,a1 (intern ,(bang-symbol-name a1))) syms)
|#
