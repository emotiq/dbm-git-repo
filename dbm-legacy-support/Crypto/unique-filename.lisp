
(in-package ecc-crypto-b571)

(defun ufilename (filename)
  (format nil "~64,'0X"
          (convert-bytes-to-int
           (sha3:sha3-digest-file filename
                                  :output-bit-length 256))))

