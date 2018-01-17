;; Acudora Change Password delivery script
;;
#|;; To use:

;; For Mac-64
pushd /Applications/LispWorks\ 6.0\ \(64-bit\)/LispWorks\ \(64-bit\).app/Contents/MacOS
./Lispworks-6-0-0-macos64-universal -init ~/projects/lispworks/VTuning/crypto/tools/deliver-change-passwd.lisp
popd

;; For Mac
pushd /Applications/LispWorks\ 6.0/LispWorks.app/Contents/MacOS
./Lispworks-6-0-0-macos-universal -init ~/projects/lispworks/Godzilla/deliver.lisp
popd

;; for Windows (Dawson)
pushd h:/projects/Lispworks
"d:/program Files/Lispworks/lispworks-5-1-0-x86-win32.exe" -init ./Godzilla/deliver.lisp

;; for Vista (Slate)
pushd c:/Users/Public/projects/Lispworks
"c:/program Files/Lispworks/lispworks-5-1-0-x86-win32.exe" -init ./Godzilla/deliver.lisp

;; for Vista (Citrine-Vista & Topaz-Vista)
pushd c:/projects/Lispworks
"c:/program Files/Lispworks/lispworks-6-0-0-x64-windows.exe" -init ./VTuning/crypto/tools/deliver-change-passwd.lisp
popd

|#

(load-all-patches)
(load-logical-pathname-translations "PROJECTS")
(change-directory (translate-logical-pathname "PROJECTS:LISP;"))

(let ((mi (machine-instance)))
  (cond ((string-equal "CITRINE-VISTA" mi) (load "Win32-Citrine-ASDF-Starter"))
	((string-equal "SLATE"         mi) (load "Win32-Citrine-ASDF-Starter"))
        ((string-equal "TOPAZ-VISTA"   mi) (load "Win32-Topaz-ASDF-Starter"))
        ((string-equal "DAWSON"        mi) (load "Win32-Dawson-ASDF-Starter"))
        ((string-equal "RAMBO"         mi) (load "Win32-Citrine-ASDF-Starter"))
        (t                                 (load "ASDF-Starter"))
        ))

;; ------------------------------------------------------------------------------
;; Get the bundle maker for OS X

#+:MACOSX
(compile-file-if-needed "macos-application-bundle" :load t)

;; ------------------------------------------------------------------------------
;; Get the components we need in the base Lisp

#+:WIN32 (require "ole")
(require "mt-random")

;; ------------------------------------------------------------------------------
;; Compile the application

;; (asdf:operate 'asdf:load-op :godzilla :force t) ;; force full recompile
(load "dongle")
(asdf "butterfly")
(asdf "ecc-change-passwd")

;; ------------------------------------------------------------------------------
;; Change to the resources folder

;; (change-directory (translate-logical-pathname "PROJECTS:LISP;godzilla;"))

(deliver 'ecc-crypto-b571::make-change-passwd-intf

         #+:MACOSX
         (let ((this-dir (translate-logical-pathname "PROJECTS:LISP;VTuning;crypto;tools;")))
           (write-macos-application-bundle
            "/Applications/Acudora Change Password.app"
            :signature  "ACUD"
            :identifier "com.acudora.change-ecc-password"
            :application-icns (merge-pathnames "calculator.icns" this-dir)
            :document-types nil))

	 #+:WIN32 "Acudora Change Password.exe"
         
         4 ;; delivery level
         
         ;; #+:WIN32 :icon-file #+:WIN32 "Resources/Godzilla.png"

         :interface :capi
         :keep-lisp-reader t
         ;; :keep-conditions :all
         :quit-when-no-windows t
         :kill-dspec-table nil
         ;; :keep-pretty-printer t

         ;; :keep-editor t
         ;; :editor-style :emacs
         ;; :keep-complex-numbers t
         ;; :keep-eval t
         ;; :keep-load-function t
         ;; :keep-modules t
         ;; :keep-package-manipulation t
         ;; :keep-trans-numbers t
         ;; :redefine-compiler-p nil
         
         ;; :keep-foreign-symbols t
	 ;; :keep-gc-cursor t

	 :versioninfo
         (list
          :binary-version #x0001000100000010
	  :version-string "Version 1.01 build 16"
	  :company-name   "Acudora, Inc."
	  :product-name   "Acudora Change Password"
	  :file-description "Use to change an Acudora PKI Password"
          :legal-copyright  "Copyright (c) 2011 by Acudora, Inc. All rights reserved.")
	 ;; :keep-debug-mode t
	 ;; :packages-to-keep :all

         ;; :startup-bitmap-file "Resources/Godzilla.bmp"
         )
(quit)
