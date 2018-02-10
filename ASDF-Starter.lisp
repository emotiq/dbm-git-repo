;; ASDF-Starter.lisp -- Edi Weitz's stuff to get ASDF moving better
;;
;; ----------------------------------------------
;; ASDF

;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/lw-add-ons/.lispworks,v 1.20 2007/01/22 15:15:21 edi Exp $

;;; Copyright (c) 2005-2006, Dr. Edmund Weitz.  All rights reserved.

;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:

;;;   * Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.

;;;   * Redistributions in binary form must reproduce the above
;;;     copyright notice, this list of conditions and the following
;;;     disclaimer in the documentation and/or other materials
;;;     provided with the distribution.

;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(in-package :cl-user)

#+(and :lispworks5.0 :win32)
(win32:dismiss-splash-screen t)

#|
#-:CLOZURE
(defvar *asdf-pathname* #+:win32 "z:/usr/local/lisp/source/asdf"
                        #+(or :linux :macosx :Darwin :allegro)
			"/usr/local/lisp/source/asdf"
  "Where ASDF can be found.  This pathname should not have a type.")
|#

(defvar *asdf-base-dirs* #+:win32 `("z:/usr/local/lisp/source/"
				    ;; "c:/emacs/site-lisp/"
				    "y:/projects/Lispworks/")
                         #+(or :linux :macosx :darwin)
			 `("/usr/local/lisp/source/"
			   #-:SBCL ,(translate-logical-pathname "PROJECTS:LISP;") ;; "~/projects/lispworks/"
			   #+:SBCL ,(format nil "~A/~A"
					    (posix-getenv "HOME")
					    "projects/lispworks/")
			   )
  "A list of directories \(note trailing slashes) which contain
directories that contain ASDF system definitions.

Example: If you have, say, c:/home/lisp/cl-ppcre/cl-ppcre.asd and
c:/home/lisp/tbnl/tbnl.asd, then \"c:/home/lisp/\" should be in
this list, and NOT \"c:/home/lisp/cl-ppcre/\".")

#-:SBCL
(defvar *working-dir* #+:win32 "y:/Projects/lispworks"
                      #+(or :linux :macosx :darwin) (translate-logical-pathname "PROJECTS:LISP;") ;; "~/projects/lispworks"
  "The working directory LW is supposed to switch to after loading
this initialization file.")

#+:SBCL
(defvar *working-dir* #+:win32 "y:/Projects/lispworks"
                      #+(or :linux :macosx :darwin)
		      (format nil "~A/~A"
			      (posix-getenv "HOME")
			      (translate-logical-pathname "PROJECTS:LISP;") ;; "projects/lispworks"
                              )
  "The working directory LW is supposed to switch to after loading
this initialization file.")

#+(AND :lispworks :win32)
;; to "fix" USER-HOMEDIR-PATHNAME
;; see <http://support.microsoft.com/default.aspx?scid=kb;en-us;101507>
(setf (lw:environment-variable "HOMEPATH") "\\Projects"
      (lw:environment-variable "HOMEDRIVE") "y:")

;; loads (and compiles, if needed) ASDF unless it's already in the
;; image
#-:asdf
(progn
  #+(OR :SBCL :CLOZURE :LISPWORKS7 :ALLEGRO)
  (require "asdf")
  #-(OR :SBCL :CLOZURE :LISPWORKS7 :ALLEGRO)
  (handler-case
      (when *asdf-pathname*
	(load
	 #+:LISPWORKS
	 (or (compile-file-if-needed *asdf-pathname*)
	     *asdf-pathname*)
	 #+:SBCL
	 *asdf-pathname*))
    (#+:LISPWORKS conditions:fasl-error
      #+:SBCL sb-ext:invalid-fasl
      ()
      (load (compile-file *asdf-pathname*)))))

#+:LISPWORKS
(defun walk-directory-for-asdf (dir)
  "Looks into the directory DIR and all subdirectories and adds
all directories which contains files of type \"asd\" to
ASDF:*CENTRAL-REGISTRY*."
  (dolist (dir-candidate (directory ;; (merge-pathnames (make-pathname :name :wild) dir)
                                    (lw:pathname-location dir)))
    (when (lw:file-directory-p dir-candidate)
      (walk-directory-for-asdf dir-candidate)
      (let ((asd-candidate (merge-pathnames "*.asd" dir-candidate)))
        (when (directory asd-candidate)
          (pushnew dir-candidate asdf:*central-registry* :test #'equal))))))

;; ---------------------------------------------------------------
;; DM/Emotiq 01/18 -- blame me for this #+:ALLEGRO stuff...

#+:ALLEGRO
(defun walk-directory-for-asdf (dir)
  "Looks into the directory DIR and all subdirectories and adds
all directories which contains files of type \"asd\" to
ASDF:*CENTRAL-REGISTRY*."
  (labels ((directoryp (pathname)
	     (and (null (pathname-name pathname))
		  (null (pathname-type pathname)))))
    (dolist (dir-candidate (directory
			    (merge-pathnames
			     (make-pathname :name :wild
					    :type :wild)
			     dir)
			    :directories-are-files nil
			    :follow-symbolic-links nil))
      (when (directoryp dir-candidate)
	(format t "~&ASDF Walking: ~A" dir-candidate)
	(walk-directory-for-asdf dir-candidate)
	(let ((asd-candidate (merge-pathnames "*.asd" dir-candidate)))
	  (when (directory asd-candidate)
	    (pushnew dir-candidate asdf:*central-registry* :test #'equal)))))))
;; ---------------------------------------------------------------

#+:CLOZURE
(defun walk-directory-for-asdf (dir)
  "Looks into the directory DIR and all subdirectories and adds
all directories which contains files of type \"asd\" to
ASDF:*CENTRAL-REGISTRY*."
  (dolist (dir-candidate (directory
			  (merge-pathnames
			   (make-pathname :name :wild
					  :type :wild)
			   dir)
			  :directories t))
    (when (directoryp dir-candidate)
      (walk-directory-for-asdf dir-candidate)
      (let ((asd-candidate (merge-pathnames "*.asd" dir-candidate)))
        (when (directory asd-candidate)
          (pushnew dir-candidate asdf:*central-registry* :test #'equal))))))

#+:SBCL
(defun component-present-p (value)
  (and value
       (not (eql value :unspecific))))

#+:SBCL
(defun directory-pathname-p (p)
  (and
   (not (component-present-p (pathname-name p)))
   (not (component-present-p (pathname-type p)))))

#+:SBCL
(defun walk-directory-for-asdf (dir)
  "Looks into the directory DIR and all subdirectories and adds
all directories which contains files of type \"asd\" to
ASDF:*CENTRAL-REGISTRY*."
  (dolist (dir-candidate (directory
			  (merge-pathnames
			   (make-pathname :name :wild
					  :type :wild)
			   dir)))
    (when (directory-pathname-p dir-candidate)
      (walk-directory-for-asdf dir-candidate)
      (let ((asd-candidate (merge-pathnames "*.asd" dir-candidate)))
        (when (directory asd-candidate)
          (pushnew dir-candidate asdf:*central-registry* :test #'equal))))))

(defun update-asdf-central-registry ()
  "Loops through *ASDF-BASE-DIRS* recursively and adds all
directories containing system definitions to ASDF's central
registry."
  (dolist (base-dir *asdf-base-dirs*)
    (walk-directory-for-asdf base-dir)))

(update-asdf-central-registry)

(defmethod asdf:perform :around ((o asdf:load-op) (c asdf:cl-source-file))
  "When trying to load a Lisp source file with ASDF that has a wrong
FASL version recompiles it."
  ;; from Bill Clementson's blog
  (handler-case
    (call-next-method o c)
    (#+:LISPWORKS conditions:fasl-error
     #+:CLOZURE error
     #+:ALLEGRO error
     #+:SBCL    sb-ext:invalid-fasl
     ()
      (asdf:perform (make-instance 'asdf:compile-op) c)
      (call-next-method))))

(defun asdf (lib &rest args &key &allow-other-keys)
  "Shortcut for ASDF."
  (apply 'asdf:oos 'asdf:load-op lib args))

