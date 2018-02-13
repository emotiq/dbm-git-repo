;; -*- mode: lisp -*-



;;;; Quicklisp Setup



;; adapted from quicklisp's lines added by ql:add-to-init-file:

(defparameter *quicklisp-base-pathname* "~/quicklisp/")

(defun quicklisp-set-up-p ()
  (member ':quicklisp *features*))

(defun set-up-quicklisp ()
  (let ((p (merge-pathnames "setup.lisp" *quicklisp-base-pathname*)))
    (when (probe-file p)
      (format t "~%Loading Quicklisp setup file: ~a ... " p)
      (load p)
      (format t "DONE.~%"))))

(defun set-up-quicklisp-if-needed ()
  (when (not (quicklisp-set-up-p))
    (set-up-quicklisp)))

(set-up-quicklisp-if-needed)
