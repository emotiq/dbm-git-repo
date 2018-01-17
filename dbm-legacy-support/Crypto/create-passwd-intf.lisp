
(in-package :ecc-crypto-b571)

(capi:define-interface create-passwd-intf ()
  ()
  (:panes
   (pwd-id-pane capi:text-input-pane
                  :text ""
                  :title "E-Mail"
                  :title-args '(:visible-min-width (:character 13)
                                :visible-max-width t)
                  :visible-min-width '(:character 30)
                  :visible-max-width t
                  :accessor pwd-id-pane)
   (pwd-pwd1-pane capi:password-pane
                     :text ""
                     :title "Password"
                     :title-args '(:visible-min-width (:character 13)
                                   :visible-max-width t)
                     :visible-min-width '(:character 30)
                     :visible-max-width t
                     :accessor pwd-pwd1-pane)
   (pwd-pwd2-pane capi:password-pane
                  :text ""
                  :title "Verify"
                  :title-args '(:visible-min-width (:character 13)
                                :visible-max-width t)
                  :visible-min-width '(:character 30)
                  :visible-max-width t
                  :accessor pwd-pwd2-pane)

   (generate-passwd-button capi:push-button
                       :text "Generate"
                       :callback 'generate-passwd)

   (cancel-button capi:push-button
                  :text "Cancel"
                  :callback (lambda (x intf)
                              (declare (ignore x))
                              (capi:destroy intf)))
   )
  
  (:layouts

   (row-layout1
    capi:row-layout
    '(generate-passwd-button
      nil
      cancel-button))

   (column-layout1
    capi:column-layout
    '(pwd-id-pane
      pwd-pwd1-pane
      pwd-pwd2-pane
      row-layout1))
   )

  (:default-initargs
   :layout 'column-layout1
   :title "Acudora Create PKI Password"
   :best-width 400
   :window-styles '( #+:COCOA :textured-background
                              :movable-by-window-background)
   ))

(defun make-create-passwd-intf ()
  (let ((intf (capi:display (make-instance 'create-passwd-intf))))
    ;; (init-crypto)
    ;; (assert *passwds*)
    ;; (assert *public-keys*)
    intf))

(defun get-create-pwd-fields (intf)
  (labels ((get-text (accessor)
             (string-trim '(#\space #\tab #\newline #\return)
                          (capi:text-input-pane-text (funcall accessor intf)))))
    (let ((ans (loop for pair in '((:id    pwd-id-pane)
                                   (:pwd1  pwd-pwd1-pane)
                                   (:pwd2  pwd-pwd2-pane))
                     collect (car pair)
                     collect (get-text (cadr pair)) )))
      (labels ((empty-p (name)
                 (zerop (length (getf ans name)))))
        (dolist (pair '((:id   "E-Mail")
                        (:pwd1 "Password")
                        (:pwd2 "Verify")))
          (destructuring-bind (keyw txt) pair
            (when (empty-p keyw)
              (progn
                (capi:display-message "~A required" txt)
                (return-from get-create-pwd-fields))) ))
        (if (string= (getf ans :pwd1) (getf ans :pwd2))
            ans
          ;; else
          (progn
            (capi:display-message "Password does not match Verify")
            (return-from get-create-pwd-fields)) )))))

(defun generate-passwd (x intf)
  (declare (ignore x))
  (handler-case
      (let ((ans (get-create-pwd-fields intf)))
        (when ans
          (with-progress-bar ()
            (make-passwd-entry (getf ans :id) (getf ans :pwd1))
            (update-pwd-files))
          (capi:display-message "Finished")
          (capi:destroy intf)))
    (error (err)
      (capi:display-message "Error: ~A" err))) )
             
      
