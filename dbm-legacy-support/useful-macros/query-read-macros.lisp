
(in-package :useful-macros)

(defmethod pred ((n real))
  (1- n))

(defmethod succ ((n real))
  (1+ n))

(defmethod pred ((ch character))
  (code-char (1- (char-code ch))))

(defmethod succ ((ch character))
  (code-char (1+ (char-code ch))))

(defun symbol-macro-dispatchers ()
  (flet ((get-range (from to)
           (nlet-tail collect ((ix from)
                               (ans nil))
             (if (char<= ix to)
                 (collect (succ ix) (cons (cons ix (get-dispatch-macro-character #\# ix)) ans))
               (nreverse ans)))))
    (inspect (get-range #\A #\Z))
    ))
