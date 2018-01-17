;; utilities.lisp -- a collection of useful routines
;;
;; DM/Emotiq  01/18
;; -----------------------------------------------------------------

(in-package :ads)

;; ---------------------------------------------------------------

(deftype vector-ub8 ()
  `(simple-array (unsigned-byte 8) (*)))

;; ---------------------------------------------------------------

(defmacro def-cached-var (name creator &optional cache-name)
  (let ((cname (or cache-name (intern (format nil "*~A*" (string name))))))
    `(progn
       (defvar ,cname nil)
       (defun ,name ()
         (or ,cname
             (setf ,cname ,creator)))) ))

(editor:setup-indent "def-cached-var" 1)

;; -----------------------------------------------------------------------------
;; PRNG for system

(def-cached-var ctr-hash-prng
  #-:OS-WINDOWS (ironclad:make-prng :fortuna :seed :urandom)
  #+:OS-WINDOWS (lw:make-mt-random-state t))

(um:defmonitor
    ;; protected by a global lock
    ;; ctr-hash-prng is a shared mutable state
    ((rand (limit)
       #-:OS-WINDOWS (ironclad:strong-random limit (ctr-hash-prng))
       #+:OS-WINDOWS (lw:mt-random limit (ctr-hash-prng)))))

(defun rand-between (lower upper)
  (declare (integer lower upper))
  ;; generate random (lower <= n < upper)
  (+ lower (rand (- upper lower))))

;; --------------------------------------------------
;; Low level helper functions

(defvar *nibbles-to-chars* "0123456789abcdef")

(defun encode-bytes-to-string (bytes)
  (with-output-to-string (s)
    (map nil (lambda (b)
               (labels ((princ-nibble (bit-pos)
                          (princ (char *nibbles-to-chars*
                                       (ldb (byte 4 bit-pos) b))
                                 s)))
                 (princ-nibble 4)
                 (princ-nibble 0)))
         bytes)))

(defun decode-string-to-bytes (str)
  (labels ((nibble-val (c)
             (cond ((char<= #\0 c #\9)
                    (- (char-code c) #.(char-code #\0)))
                   ((char<= #\a c #\f)
                    (+ 10 (- (char-code c) #.(char-code #\a))))
                   ((char<= #\A c #\F)
                    (+ 10 (- (char-code c) #.(char-code #\A))))
                   (t
                    (error "Invalid hex-string ~A" str))
                   )))
    (let (tmp)
      (prog1
          (coerce
           (um:accum acc
             (map nil (lambda (c)
                        (cond (tmp
                               (acc (logior (ash tmp 4)
                                            (nibble-val c)))
                               (setf tmp nil))
                              
                              (t
                               (setf tmp (nibble-val c)))
                              ))
                  str))
           'vector-ub8)
        (assert (null tmp)))) ;; check that we had an even number of nibble chars
    ))

;; -----------------------------------------------------------------------------

(defun invalid-arg (x)
  (error "Invalid argument type ~A" x))

(defun singleton? (lst)
  ;; is this a list with only one item?
  (null (cdr lst)))

;; -----------------------------------------------------------------------------

(defclass queue ()
  ((hd  :accessor queue-hd)
   (tl  :accessor queue-tl)))

(defmethod initialize-instance :after ((q queue) &key initial-contents &allow-other-keys)
  (let ((lst (cons nil (copy-list initial-contents))))
    (setf (queue-hd q) lst
          (queue-tl q) (last lst))))

(defmethod queue-add ((q queue) item)
  ;; add item to tail of queue
  (setf (queue-tl q)
        (setf (cdr (queue-tl q)) (list item))))

(defmethod queue-empty-p ((q queue))
  (eq (queue-hd q) (queue-tl q)))

(defmethod queue-pop ((q queue))
  ;; pop first item from queue
  (when (queue-empty-p q)
    (error "Empty queue"))
  (pop (queue-hd q))
  (shiftf (car (queue-hd q)) nil)) ;; help out GC

(defmethod queue-contents ((q queue))
  ;; return the list of queue contents, emptying the queue
  (setf (queue-tl q) (queue-hd q))
  (shiftf (cdr (queue-hd q)) nil))

;; -----------------------------------------------------------------------------


