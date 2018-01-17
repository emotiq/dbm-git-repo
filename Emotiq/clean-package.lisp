
(in-package :user)

(defun clean-package (package)
  (let ((pkg (find-package package)))
    (do-symbols (sym pkg)
      (when (and (eq pkg (symbol-package sym))
                 (fboundp sym))
        (print sym)
        (fmakunbound sym)))
    ))

