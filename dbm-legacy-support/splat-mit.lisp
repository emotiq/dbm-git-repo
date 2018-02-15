;; splat-mit.lisp -- Stuff an MIT License into source files
;;
;; DM/Emotiq  02/18
;; ------------------------------------------------------------------------
#|
The MIT License

Copyright (c) 2018 Emotiq AG

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
|#

(defvar *mit-license*
#>.end
#|
The MIT License

~A

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
|#
.end)


(defvar *emotiq-cright*
  "Copyright (c) 2018 Emotiq AG")

(defvar *ral-cright*
  "Copyright (c) 2017-2018 Refined Audiometrics Laboratory, LLC")

(defun gen-license (cright)
  (format nil *mit-license* cright))

(defun add-license-to-file (cright &optional file)
  (um.remfn:with-remembered-prompting (fname :mit-license-update file)
    (when fname
      (let ((fstring  (file-string fname)))
        (um:nlet-tail iter ((pos 0))
          (if (char= #\newline (char fstring pos))
              (setf fstring (concatenate 'string
                                         (subseq fstring 0 pos)
                                         (gen-license cright)
                                         (subseq fstring pos)))
            (iter (1+ (position #\newline fstring :start (1+ pos))))
            ))
        (with-open-file (f fname
                           :direction :output
                           :if-does-not-exist :error
                           :if-exists :overwrite)
          (write-sequence fstring f))
        ))))

(defun walk-folder (dir fn)
  (dolist (asd-candidate (directory (merge-pathnames dir "*.asd")))
    (print asd-candidate)
    (funcall fn asd-candidate))
  (dolist (lisp-candidate (directory (merge-pathnames dir "*.lisp")))
    (print lisp-candidate)
    (funcall fn lisp-candidate))
  (dolist (dir-candidate (directory ;; (merge-pathnames (make-pathname :name :wild) dir)
                                    (lw:pathname-location dir)))
    (when (lw:file-directory-p dir-candidate)
      (walk-folder dir-candidate fn))))

(defun add-license-to-files (cright)
  (let ((dir (capi:prompt-for-directory "Pick a folder")))
    (when dir
      (walk-folder dir (um:curry 'add-license-to-file cright)))
    ))
    