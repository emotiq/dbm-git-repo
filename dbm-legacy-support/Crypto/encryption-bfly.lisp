
(in-package :ecc-crypto-b571)

#|
;; --------------------------------------------------------------------
;; This portion belongs on the central Acudora key server

(defvar *member-ids*
  (make-hash-table :test 'equalp))

(defun get-ibe-decryption-key (ckeys my-id)
  (handler-case
      (let ((kpub  (get-public-key my-id)))
        (destructuring-bind (ks to-id)
            (dh-decrypt ckeys *ecc-acudora-private-key*)
          (with-sensitive-objects (ks)
            (if (or (equalp to-id my-id)
                    (member my-id (gethash to-id *member-ids*)
                            :test 'equalp))
                (values (dh-encrypt ks kpub) t)
              ;; else
              (error "Not intended recipient"))) ))
    (error (err)
      (values nil err))))

(defun start-key-server ()
  ;; (com.sd.butterfly.int::initialize-api)
  (com.sd.butterfly.int:lw-start-butterfly)
  (bfly.name-server::start :acudora-key-server))
  
;; --------------------------------------------------------------------
|#
(defun decrypt-ibe (cmsg my-id)
  (let ((kpriv (get-private-key my-id))
        ;; (srv   :acudora-key-server)
        #||#
        (srv   (bfly:remote-service
                ;; "granite.local"
                ;; "roadrunner"
                ;; "10.0.1.200"
                ;; "artemis.local"
                "localhost"
                :name :acudora-key-server))
        #||#
        )
    (when kpriv
      (destructuring-bind (etype sig ckeys cmsg)
          (decode-object-from-base64 cmsg)
        (assert (uuid:uuid= etype *ibe-encryption*))
        (multiple-value-bind (uuid kpub)
            (check-signature-with-cmsg cmsg sig)
          (multiple-value-bind (cks err)
              (com.sd.butterfly.bb:with-rpc-timeout 60
                (bfly:call-sync srv :get-decryption-key ckeys my-id))
            (unless cks
              (error err))
            (let* ((ks  (dh-decrypt cks kpriv)))
              (with-sensitive-objects (ks)
                (values (decrypt-msg cmsg ks)
                        (uuid:when-created uuid)
                        kpub))) ))) )))

