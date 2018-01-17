;; machine-id.lisp -- Get Mac Machine Identification String
;; DM/Acudora  08/11
;; --------------------------------------------------------------------------------

(defpackage :machid
  (:use :common-lisp)
  (:export
   #:get-machine-id
   #:get-machine-id-digest
   ))

(in-package #:machid)

(fli:define-foreign-type mach-port-t         () `(:unsigned :int))
(fli:define-foreign-type io-registry-entry-t () `(:unsigned :int))
(fli:define-foreign-type CFTypeRef           () `(:pointer :void))
(fli:define-foreign-type CFStringRef         () `(:pointer :void))
(fli:define-foreign-type CFAllocatorRef      () `(:pointer :void))
(fli:define-foreign-type CFStringEncoding    () :uint32)

(defconstant *kCFAllocatorDefault*  fli:*null-pointer*)

;; ----------------------------------------------

(defconstant *kIORegistryIterateRecursively* #x00000001)

(fli:define-foreign-function (IORegistryGetRootEntry "IORegistryGetRootEntry" :source)
    ((masterPort mach-port-t))
  :result-type io-registry-entry-t
  :language :ansi-c)

(fli:define-foreign-function (IORegistryEntrySearchCFProperty "IORegistryEntrySearchCFProperty" :source)
    ((root  io-registry-entry-t)
     (plane (:reference-pass (:ef-mb-string
                              :external-format :ascii)))
     (key   CFStringRef)
     (:constant *kCFAllocatorDefault* CFAllocatorRef)
     (action :uint32))
  :result-type CFTypeRef
  :language :ansi-c)

(fli:define-foreign-function (IOObjectRelease "IOObjectRelease" :source)
    ((handle io-registry-entry-t))
  :result-type :int
  :language :ansi-c)

;; ----------------------------------------------

(defconstant *kCFStringEncodingUTF8*  #x08000100)

(fli:define-foreign-function (CFStringCreateWithCString "CFStringCreateWithCString" :source)
    ((:constant *kCFAllocatorDefault* CFAllocatorRef)
     (str (:reference-pass (:ef-mb-string
                            :external-format :ascii)))
     (:constant *kCFStringEncodingUTF8* CFStringEncoding))
  :result-type CFStringRef
  :language :ansi-c)

(fli:define-foreign-function (CFStringGetCString "CFStringGetCString" :source)
    ((str-ref CFStringRef)
     (buf     (:pointer (:unsigned :char)))
     (buflen  :uint32)
     (:constant *kCFStringEncodingUTF8* CFStringEncoding))
  :result-type :int
  :language :ansi-c)

(fli:define-foreign-function (CFRelease "CFRelease" :source)
    ((ptr CFTypeRef))
  :result-type :int
  :language :ansi-c)
  
;; ----------------------------------------------

(defun get-ioregistry-text (key)
  #F
  (fli:with-dynamic-foreign-objects ()
    (let* ((root   (IORegistryGetRootEntry 0))
           (cfkey  (CFStringCreateWithCString key)))
      (unwind-protect
          (let* ((buflen 128)
                 (buf    (fli:allocate-dynamic-foreign-object
                          :type '(:unsigned :char) :nelems buflen))
                 (val    (IORegistryEntrySearchCFProperty root "IOService" cfkey
                                                          *kIORegistryIterateRecursively*)))
            (when (fli:null-pointer-p val)
              (error "Invalid key"))
            (CFStringGetCString val buf buflen)
            (cfrelease val)
            (fli:convert-from-foreign-string buf
                                             :external-format :ascii))
        (CFRelease cfkey)
        (IOObjectRelease root)
        )) ))

#|
(defun get-machine-id ()
  (concatenate 'string 
               (get-ioregistry-text "IOPlatformUUID")
               "/"
               (get-ioregistry-text "IOPlatformSerialNumber")))
|#

(defun get-machine-id ()
  #F
  (get-ioregistry-text "IOPlatformUUID"))

(defun get-machine-id-digest (&key (niter 8192))
  (let ((machid (get-machine-id))
        (digest (make-array 32 :element-type '(unsigned-byte 8))))
    (let ((digester (ironclad:make-digest :sha256)))
      (ironclad:update-digest digester machid)
      (ironclad:produce-digest digester :digest digest))
    (loop repeat niter
          for digester = (ironclad:make-digest :sha256)
          then (reinitialize-instance digester)
          do
          (progn
            (ironclad:update-digest digester machid)
            (ironclad:update-digest digester digest)
            (ironclad:produce-digest digester :digest digest)))
    (ironclad:byte-array-to-hex-string digest)))
