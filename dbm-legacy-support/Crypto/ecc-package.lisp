;; ecc-package.lisp
;; -----------------------------------

(in-package :user)

(defpackage :crypto-mod-math
  (:use :common-lisp)
  (:export
   :reset-blinders
   :expt-mod
   :sqrt-mod
   :mult-mod
   :add-mod
   :sub-mod
   :inv-mod
   :div-mod
   :quadratic-residue-p
   ))

(defpackage :ecc-crypto-b571
  (:use :common-lisp :crypto-mod-math)
  (:export
   :ctr-hash-prng
   :basic-random
   :basic-random-between
   :random-between
   
   :convert-int-to-nbytes
   :convert-int-to-nbytesv
   :convert-bytes-to-int
   :ctr-drbg-int
   :sha3-buffers
   
   :def-cached-var
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

(defpackage :edwards-ecc
  (:nicknames :edec)
  (:use :common-lisp :ecc-crypto-b571 :crypto-mod-math)
  (:import-from :ecc-crypto-b571
		:convert-int-to-nbytes
		:convert-int-to-nbytesv
		:convert-bytes-to-int
		:ctr-drbg-int
		:sha3-buffers
		:random-between)
  (:export
   :ed-curve
   :with-ed-curve
   :ed-curves
   :*ed-gen*
   :*ed-r*
   :*ed-h*
   :*ed-q*
   :ed-affine
   :ed-pt=
   :ed-neutral-point-p
   :ed-satisfies-curve
   :ed-add
   :ed-negate
   :ed-sub
   :ed-mul
   :ed-div
   :ed-nth-pt
   :ed-compress-pt
   :ed-decompress-pt
   :ed-validate-point
   :ed-hash
   :ed-random-pair
   :ed-random-generator
   
   :elligator-random-pt
   :elligator-tau-vector
   :elligator-encode
   :elligator-decode
   :elligator-limit
   :elligator-nbits

   :ed-schnorr-sig
   :ed-schnorr-sig-verify
   
   ;; field numeric operators mod *ed-q*
   :ed+
   :ed-
   :ed*
   :ed/
   :ed-sqrt
   :ed-expt
   ))

(defpackage :lagrange-4-square
  (:use :common-lisp)
  (:import-from :primes
   :is-prime?
   :expt-mod)
  (:import-from :useful-macros
   :curry
   :nlet-tail)
  (:export
   :decompose-integer
   ))
   
#|
(defpackage :ecc-crypto-b128
  (:use #:common-lisp)
  (:export
   ))
|#
