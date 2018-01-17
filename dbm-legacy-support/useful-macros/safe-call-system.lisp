
(in-package :um)

;; SAFE-CALL-SYSTEM - fixes a bug in LWM that prevents proper
;; character set translation of extended chars in filenames, making a
;; straightforward CALL-SYSTEM fail when it should otherwise succeed.
;;
;; DM/RAL 02/17 (LWM 7.0)

(defmethod safe-call-system ((v vector) &rest args &key &allow-other-keys)
  (apply #'safe-call-system (coerce v 'list) args))

(defmethod safe-call-system ((lst list) &rest args &key &allow-other-keys)
  (apply #'safe-call-system (format nil "~{~A ~}" lst) args))

(defmethod safe-call-system ((cmd string) &rest args &key showing &allow-other-keys)
  (let ((syscmd (if showing
                    #'sys:call-system-showing-output
                  #'sys:call-system)))
    (remf args :showing)
    (if (some (lambda (c)
                (> (char-code c) 127))
              cmd)
        (let ((scrfname (hcl:create-temp-file :directory (or (probe-file "/Volumes/ramdisk/")
                                                             "/tmp/"))))
          (unwind-protect
              (progn
                (with-open-file (s scrfname
                                   :direction :output
                                   :if-exists :supersede
                                   :external-format '(:UTF-8 :eol-style :lf))
                  (write-line cmd s))
                (apply syscmd (list "/bin/sh" (namestring scrfname)) args))
          (delete-file scrfname)))
      ;; else
      (apply syscmd cmd args))
    ))

