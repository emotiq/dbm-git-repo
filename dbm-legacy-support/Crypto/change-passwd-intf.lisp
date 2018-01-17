
(in-package :ecc-crypto-b571)

(capi:define-interface change-passwd-intf ()
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
   (pwd-old-pane capi:password-pane
                 :text ""
                 :title "Old Password"
                 :title-args '(:visible-min-width (:character 13)
                               :visible-max-width t)
                 :visible-min-width '(:character 30)
                 :visible-max-width t
                 :accessor pwd-old-pane)
   (pwd-pwd1-pane capi:password-pane
                     :text ""
                     :title "New Password"
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
                       :text "Change"
                       :callback 'change-pwd)

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
      pwd-old-pane
      pwd-pwd1-pane
      pwd-pwd2-pane
      row-layout1))
   )

  (:default-initargs
   :layout 'column-layout1
   :title "Acudora Change PKI Password"
   :best-width 400
   ;; :window-styles '(:textured-background :movable-by-window-background)
   ))

(defun make-change-passwd-intf ()
  (let ((intf (capi:display (make-instance 'change-passwd-intf))))
    ;; (init-crypto)
    ;; (assert *passwds*)
    ;; (assert *public-keys*)
    intf))

(defun get-change-pwd-fields (intf)
  (labels ((get-text (accessor)
             (string-trim '(#\space #\tab #\newline #\return)
                          (capi:text-input-pane-text (funcall accessor intf)))))
    (let ((ans (loop for pair in '((:id      pwd-id-pane)
                                   (:pwd-old pwd-old-pane)
                                   (:pwd1    pwd-pwd1-pane)
                                   (:pwd2    pwd-pwd2-pane))
                     collect (car pair)
                     collect (get-text (cadr pair)) )))
      (labels ((empty-p (name)
                 (zerop (length (getf ans name)))))
        (dolist (pair '((:id      "E-Mail")
                        (:pwd-old "Old Password")
                        (:pwd1    "New Password")
                        (:pwd2    "Verify")))
          (destructuring-bind (keyw txt) pair
            (when (empty-p keyw)
              (progn
                (capi:display-message "~A required" txt)
                (return-from get-change-pwd-fields))) ))
        (if (string= (getf ans :pwd1) (getf ans :pwd2))
            ans
          ;; else
          (progn
            (capi:display-message "New Password does not match Verify")
            (return-from get-change-pwd-fields)) )))))

(defun change-pwd (x intf)
  (declare (ignore x))
  (handler-case
      (let ((ans (get-change-pwd-fields intf)))
        (when ans
          (with-progress-bar ()
            (change-passwd (getf ans :id)
                           (getf ans :pwd-old)
                           (getf ans :pwd1)))
          (capi:display-message "Finished")
          (capi:destroy intf)))
    (error (err)
      (capi:display-message "Error: ~A" err))) )
             
      
