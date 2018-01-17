
(defpackage #:resource
  (:use #:common-lisp)
  (:export
   #:release-resource
   ))

(in-package #:resource)

(defmethod release-resource (obj &key &allow-other-keys)
  obj)
  
