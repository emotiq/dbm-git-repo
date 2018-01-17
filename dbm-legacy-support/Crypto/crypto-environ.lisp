
(in-package :ecc-crypto-b571)

;; ------------------------------------------------------------------------------
;; File Locations Environment

(defstruct environ
  home-path
  shared-files-path
  pwd-file
  pubkey-file)

(defun make-crypto-environ ()
  (let* ((home   (probe-file #+:MACOSX "~/"
                             ;; #+:WIN32 (lw:environment-variable "USERPROFILE")
                             #+:WIN32 (sys:get-user-profile-directory)))
         (shared #+:MACOSX (or (probe-file "~/Dropbox/Team Share/")
                               (probe-file "~/Documents/Dropbox/Team Share/"))
                 #+:WIN32  (probe-file (merge-pathnames "Dropbox/Team Share/" home)))
         (pwd    (merge-pathnames "_acudora-ecc-passwds" home))
         (pub    (merge-pathnames "Acudora-public-keys.txt" shared)))
    (make-environ
     :home-path         home
     :shared-files-path shared
     :pwd-file          pwd
     :pubkey-file       pub)))

(def-cached-var crypto-environ (make-crypto-environ))

#|
(defvar *crypto-environ* nil)

(defun crypto-environ ()
  (or *crypto-environ*
      (setf *crypto-environ* (init-crypto-environ))))
|#

(defun shared-acudora-file (fname)
  (merge-pathnames fname (environ-shared-files-path (crypto-environ))))

(defun pwd-file ()
  (environ-pwd-file (crypto-environ)))

(defun pubkey-file ()
  (environ-pubkey-file (crypto-environ)))

(defun home-path ()
  (environ-home-path (crypto-environ)))

;; -------------------------------------------------------------------------------------------
