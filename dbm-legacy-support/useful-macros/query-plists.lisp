
(defun compile-plist-query (query)
  (labels ((callfun (object)
             (lambda (fun)
               (funcall fun object)))
           (compile-= (keyword value)
             (lambda (plist)
               (equal (getf plist keyword) value)))
           (compile-and (funs)
             (lambda (plist)
               (every (callfun plist) funs)))
           (compile-or (funs)
             (lambda (plist)
               (some (callfun plist) funs)))
           (compile-not (fun)
             (lambda (plist)
               (not (funcall fun plist)))))
    (let ((operator (first query))
          (operands (rest query)))
      (ecase operator
        (:=
         (compile-= (first operands) (second operands)))
        (:and
         (compile-and (mapcar #'compile-plist-query operands)))
        (:or
         (compile-or (mapcar #'compile-plist-query operands)))
        (:not
         (compile-not (compile-plist-query (first operands))))))))

(defun query-plists (query plists)
  (remove-if-not (compile-plist-query query) plists))

#|
(query-plists '(:and (:= :first-name "Zach") (:= :state "ME")
                     (:not (:= :last-name "Beane")))
              *people*)
|#
