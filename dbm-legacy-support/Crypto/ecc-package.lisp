;; ecc-package.lisp
;; -----------------------------------

(in-package :user)

(defpackage :ecc-crypto-b571
  (:use #:common-lisp)
  (:export
   #:ctr-hash-prng
   #:basic-random
   #:basic-random-between
   ))

(defpackage :primes
  (:use #:common-lisp)
  (:export
   #:divides?
   #:expt-mod
   #:random-between
   #:make-prime
   #:is-prime?
   #:extended-gcd
   #:compute-modulo-inverse
   #:provably-prime?
   #:factors-of
   #:generate-strong-prime
   #:generate-rsa-base
   #:add-mod
   #:sub-mod
   #:mult-mod
   #:inv-mod
   #:div-mod
   #:expt-mod
   #:decompose
   ))


#|
(defpackage :ecc-crypto-b128
  (:use #:common-lisp)
  (:export
   ))
|#
