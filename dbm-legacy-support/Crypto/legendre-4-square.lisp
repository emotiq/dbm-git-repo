
(in-package :ecc-crypto-b571)

;; equiv to #F
(declaim  (OPTIMIZE (SPEED 3) (SAFETY 0) (FLOAT 0))
          (inline empty singleton create))

(defun #1=stupid-compute-4-coffs (n)
  ;; basically, brute force
  (if (zerop n)
      (list 0 0 0 0)
    (loop for a from (isqrt n) downto (isqrt (truncate n 4)) do
          (let ((r1 (- n (* a a ))))
            (if (zerop r1)
                (return-from #1# (list a 0 0 0))
              (loop for b from (isqrt r1) downto (isqrt (truncate r1 3)) do
                    (let ((r2 (- r1 (* b b))))
                      (if (zerop r2)
                          (return-from #1# (list a b 0 0))
                        (loop for c from (isqrt r2) downto (isqrt (truncate r2 2)) do
                              (let* ((r3 (- r2 (* c c)))
                                     (d  (isqrt r3)))
                                (when (= r3 (* d d))
                                  (return-from #1# (list a b c d)))
                                ))))))))))


(declaim (inline mod4 mod8 trunc2 trunc4 expt2))

(defun mod4 (n)
  (declare (integer n))
  (ldb (byte 2 0) n))

(defun mod8 (n)
  (declare (integer n))
  (ldb (byte 3 0) n))

(defun trunc2 (n)
  (declare (integer n))
  (ash n -1))

(defun trunc4 (n)
  (declare (integer n))
  (ash n -2))

(defun expt2 (n)
  (declare (fixnum n))
  (ash 1 n))

#|
 ;; This DECOMPOSE-INTEGER-BIG version is not such a good idea. About
 ;; half as fast as using the D=1 option in DECOMPOSE-INTEGER
 
(defun cut-4^v (n)
  ;; compute common factor in n of (2^v)^2 = 4^v
  (declare (integer n))
  (check-type n (integer 1))
  (labels ((iter (v)
             (declare (fixnum v)) ;; for all practical purposes...
             ;; check successive pairs of LSB bits = mod 4
             (if (zerop (ldb (byte 2 v) n))
                 (iter (+ 2 v))
               ;; else - termination
               (if (zerop v)
                   (values n 0)
                 (values (ash n (- v))
                         (trunc2 v)))
               )))
    (iter 0)))

(defun #1=decompose-integer-big (n)
  ;; slightly smarter version
  (declare (integer n))
  (check-type n (integer 0))
  (labels ((iter (d)
             ;; remove largest square that leaves rem /= (7 mod 8)
             (declare (integer d))
             (let ((r (- n (* d d))))
               (declare (integer r))
               (if (zerop r) ;; perfect square
                   (values 0 0 d)
                 ;; else
                 (multiple-value-bind (rr v) (cut-4^v r)
                   (declare (integer rr)
                            (fixnum  v))
                   (if (= 7 (mod8 rr))
                       (iter (1- d))
                     ;; else - termination
                     (values rr v d)))
                 ))))
    (multiple-value-bind (r v d) (iter (isqrt n))
      (declare (integer r d)
               (fixnum v))
      
      (labels ((answer (a b c)
                 (declare (integer a b c))
                 (let ((ans (cons d (mapcar
                                     (um:curry '* (expt2 v))
                                     (list a b c)))))
                   (declare (cons ans))
                   (assert (= n (reduce '+
                                        (mapcar '* ans ans))))
                   (return-from #1# ans))))
        
        (let ((sqrt   (if (member (mod4 r) '(0 1)) ;; a possible square?
                          (isqrt r)
                        0))
              (decomp (cdr (assoc r '((   1 . ( 1  0  0)) ;; special cases
                                      (   2 . ( 1  1  0))
                                      (   3 . ( 1  1  1))
                                      (  10 . ( 1  3  0))
                                      (  34 . ( 3  3  4))
                                      (  58 . ( 3  7  0))
                                      (  85 . ( 6  7  0))
                                      ( 130 . ( 3 11  0))
                                      ( 214 . ( 3  6 13))
                                      ( 226 . ( 8  9  9))
                                      ( 370 . ( 8  9 15))
                                      ( 526 . ( 6  7 21))
                                      ( 706 . (15 15 16))
                                      ( 730 . ( 1 27  0))
                                      (1414 . ( 6 17 33))
                                      (1906 . (13 21 36))
                                      (2986 . (21 32 39))
                                      (9634 . (56 57 57))) ))))
          (declare (integer sqrt)
                   (list decomp))
          
      
          ;; at this point we have removed 4^v common factor, N = (2^v)^2 * nn
          ;; so nn can only be 1,2,3 mod 4 = 1,2,3,5,6,7 mod 8
          ;;
          ;; when nn /= (7 mod 8) we have sum of 3 squares, nn = (a^2 + b^2 + c^2)
          ;;  so choose d = 0 for fourth one, N = (2^v)^2 * (a^2 + b^2 + c^2 + d^2).
          ;;
          ;; when nn = (7 mod 8), choose d = 1, and now (nn-1) = (6 mod 8) as sum of 3 squares
          ;;
          ;; at this point rr not (7 mod 8) so we can represent
          ;; as sum of 3 squares
          (cond ((= r (* sqrt sqrt))
                 (answer sqrt 0 0))
                
                (decomp
                 ;; special case by table lookup
                 (apply #'answer decomp))
                
                ((= 3 (mod8 r)) ;; r = (3 mod 8)
                 ;; find odd x s.t. prime p = (r - x^2)/2
                 ;; for odd x, (r - x^2) is always even
                 (multiple-value-bind (x p) (find-prime-nx/2 r)
                   (declare (integer x p))
                   (multiple-value-bind (y z) (decompose-prime p)
                     (declare (integer y z))
                     (answer x (+ y z) (abs (- y z))))))
                    
                (t
                 ;; r is 1,2,5,6 mod 8 = 1,2 mod 4
                 ;; can't be 0,4 mod 8 since we removed common factors of 4
                 ;; can't be 7 mod 8 since we removed that case in the LET above to form rr
                 ;; can't be 3 mod 8 since we took care of that in the previous COND clause
                 ;;
                 ;; for r = 1 mod 4, it could be a square already
                 ;; but we took care of that in a previous COND
                 ;; clause
                 ;; 
                 (multiple-value-bind (x p is-prime) (find-prime-nx r)
                   (declare (integer x p))
                   (cond (is-prime
                          (multiple-value-bind (y z) (decompose-prime p)
                            (answer x y z)))
                         (t
                          ;; (nn-d^2 - x^2) was exact square, p is that square root
                          (answer x p 0))
                         )))
                )))) ))
|#

        
(defun #1=decompose-integer (n)
  ;; slightly smarter version
  (declare (integer n))
  (check-type n (integer 0))
  (if (zerop n)
      (list 0 0 0 0)
    ;; else - compute common factor of (2^v)^2 = 4^v
    (labels ((iter (v)
               (declare (fixnum v)) ;; for all practical purposes...
               ;; check successive pairs of LSB bits = mod 4
               (if (zerop (ldb (byte 2 v) n))
                   (iter (+ 2 v))
                 ;; else - termination
                 (if (zerop v)
                     (values n 0)
                   (values (ash n (- v))
                           (trunc2 v)))
                 )))
      (multiple-value-bind (nn v) (iter 0)
        (declare (integer nn)
                 (fixnum  v))
        ;; at this point we have removed 4^v common factor, N = (2^v)^2 * nn
        ;; so nn can only be 1,2,3 mod 4 = 1,2,3,5,6,7 mod 8
        ;;
        ;; when nn /= (7 mod 8) we have sum of 3 squares, nn = (a^2 + b^2 + c^2)
        ;;  so choose d = 0 for fourth one, N = (2^v)^2 * (a^2 + b^2 + c^2 + d^2).
        ;;
        ;; when nn = (7 mod 8), choose d = 1, and now (nn-1) = (6 mod 8) as sum of 3 squares
        ;;
        (let* ((d      (if (= 7 (mod8 nn)) 1 0))
               (nn-d   (- nn d))
               (sqrt   (if (= 1 (mod4 nn-d))
                           ;; squares can only be (0 mod 4) or (1 mod 4)
                           ;; we already removed all 4^v so can only be (1 mod 4)
                           (isqrt nn-d)
                         0))
               ;; these exceptions are cases where we cannot find a prime p = 1 mod 4
               ;; where n /= x^2 + p, for n = 1,2 mod 4
               ;; or where n /= x^2 + 2*p, for n = 3 mod 8
               ;; or where n /= x^2 + y^2 for n = 1,2 mod 4
               ;;
               ;; The Rabin & Shallit conjecture is that for
               ;; sufficiently large n, an appropriate prime can
               ;; always be found
               (decomp (cdr (assoc nn-d '((   3 . ( 1  1  1)) ;; 3 mod 8
                                          ( 214 . ( 3  6 13)) ;; 2 mod 4 = 6 mod 8
                                          ( 526 . ( 6  7 21)) ;; 2 mod 4 = 6 mod 8
                                          (1414 . ( 6 17 33)) ;; 2 mod 4 = 6 mod 8
                                          ) ))))
          (declare (fixnum d)
                   (integer sqrt nn-d)
                   (list decomp))
          
          (labels ((answer (a b c)
                     (declare (integer a b c))
                     (let ((ans (mapcar
                                 (um:curry '* (expt2 v))
                                 (list a b c d))))
                       (declare (cons ans))
                       (assert (= n (reduce '+
                                            (mapcar '* ans ans))))
                       (return-from #1# ans))))

            (cond ((= nn-d (* sqrt sqrt)) ;; already a square?
                   (answer sqrt 0 0))

                  (decomp
                   ;; special case by table lookup
                   (apply #'answer decomp))
                  
                  ((= 3 (mod8 nn-d)) ;; nn-d = (3 mod 8)
                   ;; find odd x s.t. prime p = (nn-d - x^2)/2
                   ;; for odd x, (nn-d - x^2) is always even
                   (multiple-value-bind (x p) (find-prime-nx/2 nn-d)
                     (declare (integer x p))
                     (multiple-value-bind (y z) (decompose-prime p)
                       (declare (integer y z))
                       (answer x (+ y z) (abs (- y z))))))
                  
                  (t
                   ;; nn-d is 1,2,5,6 mod 8 = 1,2 mod 4
                   ;; can't be 0,4 mod 8 since we removed common factors of 4
                   ;; can't be 7 mod 8 since we removed that case in the LET above to form nn-d
                   ;; can't be 3 mod 8 since we took care of that in the previous COND clause
                   ;;
                   ;; for nn-d = 1 mod 4, it could be a square already
                   ;; but we took care of that in a previous COND
                   ;; clause
                   ;; 
                   (multiple-value-bind (x p is-prime) (find-prime-nx nn-d)
                     (declare (integer x p))
                     (cond (is-prime
                            (multiple-value-bind (y z) (decompose-prime p)
                              (answer x y z)))
                           (t
                            ;; (nn-d - x^2) was exact square, p is that square root
                            (answer x p 0))
                           )))
                  )))))))

(defun #1=find-prime-nx (n)
  ;; starting with largest possible x
  ;; find x such that prime p = n - x^2
  ;; return x and p
  ;;
  ;; n = 1,2,5,6 mod 8 = 1,2 mod 4
  ;;
  ;; we could also have p not prime and n = x^2 + p^2
  ;;
  (declare (integer n))
  (check-type n (integer 1))
  (assert (member (mod4 n) '(1 2)))
  (let ((m  (mod4 n)))
    (declare (fixnum m))
    (cond ((= 1 m)
           ;; check every x, since r = n - x^2 will always be 0,1 mod 4
           (loop for x of-type integer from (the integer (isqrt n)) downto 1 do
                 (let* ((r    (- n (* x x)))
                        (sqrt (isqrt r)))
                   (declare (integer r sqrt))
                   (cond ((= r (* sqrt sqrt))
                          (return-from #1# (values x sqrt nil)))
                         ((and (= 1 (mod4 r))
                               (primes:is-prime? r))
                          (return-from #1# (values x r t)))
                         ))))
          (t ;; (= 2 m)
             ;; check every other x, since r = n - x^2 would bounce between 1,2 mod 4
             ;; and we only need 1 mod 4.
             ;; n can't be perfect square in this case. And we need x odd.
             (let ((start (isqrt n)))
               (declare (integer start))
               (when (evenp start)
                 (decf start))
               (loop for x of-type integer from start downto 1 by 2 do
                     (let* ((r    (- n (* x x))) ;; r always 1 mod 4
                            (sqrt (isqrt r)))
                     (declare (integer r sqrt))
                     (cond ((= r (* sqrt sqrt))
                            (return-from #1# (values x sqrt nil)))
                           ((primes:is-prime? r)
                            (return-from #1# (values x r t)))
                           )))))
          )))
            
(defun #1=find-prime-nx/2 (n)
  ;; for case of n = 3 mod 8
  ;; starting with largest possible odd x
  ;; find x such that prime p = (n - x^2)/2
  ;; return x and p
  ;;
  ;; n = 3 mod 8 = 3 + 8*m, always odd
  ;; x odd = 2*k + 1
  ;; x^2 = 4*k^2 + 4*k + 1 = 1 mod 4, always odd
  ;; n - x^2 = 8*m - 4*k^2 - 4*k + 2 = 2 mod 4, always even
  ;; (n - x^2)/2 = 1 mod 4
  ;;
  (declare (integer n))
  (check-type n (integer 3))
  (assert (= 3 (mod8 n)))
  (let ((start (isqrt n)))
    (declare (integer start))
    (when (evenp start)
      (decf start))
    (loop for x of-type integer from start downto 1 by 2 do
          (let* ((r    (- n (* x x))) ;; always 2 mod 4, never a perfect square
                 (p    (trunc2 r)))   ;; r always even, p always 1 mod 4
            (declare (integer r p))
            (when (primes:is-prime? p)
              (return-from #1# (values x p))
              ))) ))

(defun decompose-prime (p)
  ;; p = 1 mod 4
  (declare (integer p))
  (check-type p (integer 1))
  (assert (= 1 (mod4 p)))
  (let* ((b (if (= 5 (ldb (byte 3 0) p)) ;; p = 5 mod 8
                2
              (labels ((iter (b)
                         (if (= 1 (expt-mod p b (trunc2 (1- p)))) ;; Fermat's theorem
                             (iter (next-prime b)) ;; try next smallest prime > b
                           b)))
                (iter 3))
              ))
         (bb (expt-mod p b (trunc4 (1- p))))) ;; bb now imag unit, i.e., b^2 = -1 mod p
    (declare (integer b bb))
    (labels ((iter (a b)
               (declare (integer a b))
               ;; hmmm... this looks like Chinese Remainder Theorem...
               (if (> (* b b) p)
                   (iter b (mod a b))
                 (values b (mod a b)))))
      (iter p bb))
    ))

(defun next-prime (b)
  ;; assumes b > 2, all higher primes are odd
  (declare (integer b))
  (check-type b (integer 3))
  (assert (and (oddp b)
               (> b 2)))
  (labels ((iter (p)
             (declare (integer p))
             (if (primes:is-prime? p)
                 p
               (iter (+ p 2)))))
    (iter (+ b 2))))
               
#|
(let* ((n  512))
  (stupid-compute-4-coffs n))

(let* ((n  512))
  (mapcar (lambda (x)
            (expt-mod *ed-q* x (truncate (1- *ed-q*) 2)))
          (stupid-compute-4-coffs n)))

(time (loop repeat 1000000 do (stupid-compute-4-coffs 512))) ;; 17 sec
(time (loop repeat 1000000 do (decompose-integer 512)))      ;;  3.5 sec, more than 4x faster
(time (loop repeat 1000000 do (decompose-integer-big 512)))  ;;  5.1 sec, more than 3x faster
 |#

#|
 -- Confidentiality Protocol --
 
ECC curve C, generator G, widely known
Purchaser Bob, pubkey B a point on curve C
Vendor Alice, pubkey A a point on curve C
All UTXOs are stored as encrypted points u*G

Alice and Bob set up DH key k

Bob requests price from Alice with encrypted query,

Alice sends Bob encrypted price E(p,tid_A,k) with authentication signature

Bob verifies authentication, extracts tid_A, and price p
Bob computes UTXOs u sufficient to cover cost = p + fee
Bob computes Lagrange 4-squares x_i, i = 1..4 for difference (u - p - fee)
    u = p + fee + Sum(x_i^2)
Bob encrypts u, p with G:  u*G, p*G
Bob hashes H = Hash(u*G, p*G, fee, x_i, UTXO ids,tid_A,tid_B)

Bob publishes transaction:
     T = H, u*G, p*G, fee, x_i,A,B,tid_A,tid_B,UTXO_id's
  Bob authorizes destruction of input UTXO's, creation of p*G UTXO for Alice,
    and return change creation of UTXO = Sum(x_i^2)*G for Bob
    
Miner verifies:
     Hash H = Hash(u*G, p*G, fee, x_i, UTXO ids, tid_A, tid_B)
     u*G = p*G + fee*G + Sum(x_i^2)*G

Miner records u UTXO's as spent
Miner keeps fee
Miner records new UTXO p*G forwards to Alice along with tid_A
  Alice verifies UTXO from cost p and generator G
Miner returns new UTXO = Sum(x_i^2)*G along with tid_B to Bob, unless sum is zero
  Bob verifies UTXO from spent u, cost p, fee, and x_i

Only fee and x_i are visible
Only Bob knows amount paid, only Bob and Alice know cost of item.

 |#

#|
(defun tst (n)
  ;; for n = 1,2 mod 4 try to express as n = x^2 + p, for p prime 1 mod 4,
  ;; or for which n = x^2 + p^2
  ;;
  ;; only exceptions are '(214 526 1414) tested to n = 1,000,000 => n2 = 4,000,002
  (let (ans)
    (loop for ix from 0 to n do
          (let ((n1 (1+ (* 4 ix))))
            (multiple-value-bind (x p is-prime) (find-prime-nx n1)
              (unless x
                (push n1 ans))))
          (let ((n2 (+ 2 (* 4 ix))))
            (multiple-value-bind (x p is-prime) (find-prime-nx n2)
              (unless x
                (push n2 ans)))) )
    (nreverse ans)))

(defun tst2 (n)
  ;; for n = 3 mod 8, try to express as n = x^2 + 2*p, for p prime 1 mod 4
  ;; only exception is 3, tested to n = 1,000,000 => n3 = 8,000,003
  (let (ans)
    (loop for ix from 0 to n do
          (let ((n3 (+ 3 (* 8 ix))))
            (multiple-value-bind (x p) (find-prime-nx/2 n3)
              (unless x
                (push n3 ans)))))
    (nreverse ans)))
|#
