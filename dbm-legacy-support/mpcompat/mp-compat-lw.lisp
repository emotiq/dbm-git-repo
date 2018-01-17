;; mp-compat-lw.lisp
;; --------------------------------------------------------------------------------------
;; Compatibility layer for Lispworks, Allegro, OS X, and Win32, Mulit-Processing Primitives
;;
;; Copyright (C) 2008 by SpectroDynamics, LLC. All rights reserved.
;;
;; DM/SD  08/08
;; --------------------------------------------------------------------------------------

;; --------------------------------------------------
(in-package #:mp-compatibility)
;; --------------------------------------------------
;; Compatibility Layer

#|
(defun current-process ()
  "Get the current Lisp process."
  (mp:get-current-process))
|#

(defun current-process ()
  "Get the current Lisp process."
  mp:*current-process*)

;; --------------------------------------------------------------------------

;; --------------------------------------------------------------------------

(defun generate-uuid ()
  #+:MACOSX  (uuid:byte-array-to-uuid (uuidgen:generate))
  #+:WIN32   (uuid:make-v1-uuid))

;; --------------------------------------------------------------------------

