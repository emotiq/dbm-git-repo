#|
;; For Mac-64
pushd /Applications/LispWorks\ 6.1\ \(64-bit\)/LispWorks\ \(64-bit\).app/Contents/MacOS
./Lispworks-6-1-0-macos64-universal -init /Volumes/My\ Passport\ for\ Mac/projects/lispworks/VTuning/crypto/tools/deliver-gfc.lisp
popd
|#

(in-package "CL-USER")
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

(require "mt-random")
(load "dongle")
(asdf "butterfly")
(asdf "ecc-messaging")

(deliver 'ecc-crypto-b571::delivered-gfc-encrypt
         "gfc-crypt"
         4
         :multiprocessing t
         :console t
         :kill-dspec-table nil
         :keep-clos :method-dynamic-definition
         )
(quit)

#|
gfc-crypt enc VTuning/crypto/tools/encryption.lisp VTuning/crypto/tools/junkx HowdyDave\!
gfc-crypt dec VTuning/crypto/tools/junkx VTuning/crypto/tools/junkxx HowdyDave\!
|#
