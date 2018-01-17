#|
;; For Mac-64
pushd /Applications/LispWorks\ 6.1\ \(64-bit\)/LispWorks\ \(64-bit\).app/Contents/MacOS
./Lispworks-6-1-0-macos64-universal -init /Volumes/My\ Passport\ for\ Mac/projects/lispworks/VTuning/crypto/tools/deliver-ctr-hmac.lisp
popd
|#

(in-package "CL-USER")
(load-all-patches)

(let ((prjdir "/Volumes/My Passport for Mac/projects"))
  (setf (environment-variable "PROJECTS")
	(if (probe-file prjdir)
	    prjdir
	    #P"~/projects")))

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

(pushnew :DECRYPT-DELIVERY *features*)
(require "mt-random")
(load "dongle")
(asdf "butterfly")
(asdf "ecc-messaging")

(deliver 'ecc-crypto-b571::delivered-3ctr-hmac-encrypt
         "ctr-hmac-crypt"
         4
         :multiprocessing t
         :console t
         :kill-dspec-table nil
         :keep-clos :method-dynamic-definition
         )
(quit)

#|
ctr-hmac-crypt enc VTuning/crypto/tools/encryption.lisp VTuning/crypto/tools/junkx HowdyDave\!
ctr-hmac-crypt dec VTuning/crypto/tools/junkx VTuning/crypto/tools/junkxx HowdyDave\!
|#
