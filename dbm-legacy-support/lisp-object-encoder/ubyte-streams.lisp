
(defpackage #:ubyte-streams
  (:use #:common-lisp)
  (:nicknames #:ubstream)
  #+:LISPWORKS
  (:import-from #:stream
   #:stream-file-position)
  (:export
   #:ubyte-stream
   #:ubyte-input-stream
   #:ubyte-output-stream
   #:make-ubyte-input-stream
   #:make-ubyte-output-stream
   #:stream-bytes
   #:with-input-from-ubyte-stream
   #:with-output-to-ubyte-stream
   ;; Allegro does not export stream:stream-file-position
   #:stream-file-position
   ))

;; -----------------------------------------------------
(in-package #:ubyte-streams)
;; -----------------------------------------------------

(defclass ubyte-stream ()
  ())

(defmethod cl:stream-element-type ((stream ubyte-stream))
  '(unsigned-byte 8))

;; ------------------------------------------------------

#+:LISPWORKS
(defclass ubyte-output-stream (stream:fundamental-binary-output-stream
                               ubyte-stream)
  ((arr  :accessor uos-arr
         :initarg  :buffer
         :initform (mgdbuf:make-buffer 1024))))

#+:ALLEGRO
(defclass ubyte-output-stream (excl:fundamental-binary-output-stream
                               ubyte-stream)
  ((arr  :accessor uos-arr
         :initarg  :buffer
         :initform (mgdbuf:make-buffer 1024))))

(defun make-ubyte-output-stream (&optional use-buffer)
  (if use-buffer
      (make-instance 'ubyte-output-stream
                     :buffer use-buffer)
    (make-instance 'ubyte-output-stream)))

#+:LISPWORKS
(defmethod stream:stream-write-byte ((stream ubyte-output-stream) val)
  (vector-push-extend val (uos-arr stream))
  val)

#+:ALLEGRO
(defmethod excl:stream-write-byte ((stream ubyte-output-stream) val)
  (vector-push-extend val (uos-arr stream))
  val)

(defmethod stream-bytes ((stream ubyte-output-stream))
  (uos-arr stream))

(defun do-with-output-to-ubyte-stream (fn use-buffer)
  (let ((s (make-ubyte-output-stream use-buffer)))
    (funcall fn s)
    (copy-seq (uos-arr s))))

(defmacro with-output-to-ubyte-stream ((stream-name &optional use-buffer) &body body)
  `(do-with-output-to-ubyte-stream (lambda (,stream-name) ,@body) ,use-buffer))

(defmethod stream-file-position ((stream ubyte-output-stream))
  (fill-pointer (uos-arr stream)))

(defmethod (setf stream-file-position) (pos (stream ubyte-output-stream))
  (let ((arr (uos-arr stream)))
    (if (array-in-bounds-p arr pos)
        (setf (fill-pointer arr) pos)
      (adjust-array arr (max (* 2 (array-total-size arr))
                             (+ pos 128))
                    :fill-pointer pos))))

;; ------------------------------------------------------

#+:LISPWORKS
(defclass ubyte-input-stream (stream:fundamental-binary-input-stream 
                              ubyte-stream)
  ((arr    :reader   uis-arr    :initarg :arr)
   (ix     :accessor uis-ix     :initarg :start)
   (end    :reader   uis-end    :initarg :end)
   (reader :reader   uis-reader :initarg :reader :initform 'aref)
   ))

#+:ALLEGRO
(defclass ubyte-input-stream (excl:fundamental-binary-input-stream 
                              ubyte-stream)
  ((arr    :reader   uis-arr    :initarg :arr)
   (ix     :accessor uis-ix     :initarg :start)
   (end    :reader   uis-end    :initarg :end)
   (reader :reader   uis-reader :initarg :reader :initform 'aref)
   ))

(defun make-ubyte-input-stream (arr &key (start 0) end (reader 'aref))
  (make-instance 'ubyte-input-stream
                 :arr    arr
                 :start  start
                 :end    end
                 :reader reader))

#+:LISPWORKS
(defmethod stream:stream-read-byte ((stream ubyte-input-stream))
  (with-accessors ((arr    uis-arr)
                   (ix     uis-ix )
                   (end    uis-end)
                   (reader uis-reader)) stream
    (if (or (and end
                 (>= ix end))
            (not (array-in-bounds-p arr ix)))
        stream ;; return stream on EOF
      (prog1
          (funcall reader arr ix)
        (incf ix))
      )))

#+:ALLEGRO
(defmethod excl:stream-read-byte ((stream ubyte-input-stream))
  (with-accessors ((arr    uis-arr)
                   (ix     uis-ix )
                   (end    uis-end)
                   (reader uis-reader)) stream
    (if (or (and end
                 (>= ix end))
            (not (array-in-bounds-p arr ix)))
        stream ;; return stream on EOF
      (prog1
          (funcall reader arr ix)
        (incf ix))
      )))

(defun do-with-input-from-ubyte-stream (fn arr &rest args)
  (let ((s (apply 'make-ubyte-input-stream arr args)))
    (funcall fn s)))

(defmacro with-input-from-ubyte-stream ((stream-name arr &rest args) &body body)
  `(do-with-input-from-ubyte-stream (lambda (,stream-name) ,@body) ,arr ,@args))

(defmethod stream-file-position ((stream ubyte-input-stream))
  (uis-ix stream))

(defmethod (setf stream-file-position) (pos (stream ubyte-input-stream))
  (setf (uis-ix stream) pos))
