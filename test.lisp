(cl:in-package "https://github.com/g000001/srfi-70#internals")
(in-readtable :srfi-70)

(def-suite* srfi-70)


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun nanp (obj)
    #+sbcl (sb-ext:float-nan-p obj)
    #+lispworks (sys::nan-p obj)
    #-(or sbcl lispworks) (not-implemented obj)))


(defmacro ==> (&body body)
  `(progn
     ,@(do ((c body (cdddr c))
            (ans '()
                 (destructuring-bind (x ==> y)
                                     (subseq c 0 3)
                   (declare (ignore ==>))
                   (cons `(is (equal ,x ,y))
                         ans)) ))
           ((endp c)
            (nreverse ans)))))

;; procedure: number? obj
;; procedure: complex? obj
(test complex?
  (is-true (complex? #c(3 4)))
  (is-true (complex? 3))
  (is-true (complex? +inf.0)))
;; procedure: real? obj
(test real?
  (is-true (real? 3))
  (is-true (real? #c(-2.5 0.0)))
  (is-true (real? 1e10))
  (is-true (real? -inf.0)))
;; procedure: rational? obj
(test rational?
  (is-true (rational? 6/10))
  (is-true (rational? 6/3))
  (is-false (rational? +inf.0)))
;; procedure: integer? obj
(test integer?
  (is-true (integer? #c(3 0)))
  (is-true (integer? 3.0))
  (is-true (integer? 8/4))
  (is-false (integer? -inf.0)))
;; procedure: exact? z
(test exact?
  (is-true (exact? 5)))
;; procedure: inexact? z
(test inexact?
  (is-true (inexact? +inf.0)))

;; procedure: = z1 z2 z3 ...
;; procedure: < x1 x2 x3 ...
;; procedure: > x1 x2 x3 ...
;; procedure: <= x1 x2 x3 ...
;; procedure: >= x1 x2 x3 ...
(test =
  (is-true (= (+ 1 +inf.0) +inf.0))
  (is-true (= +inf.0 +inf.0))
  (is-false (= -inf.0 +inf.0))
  (is-true (= -inf.0 -inf.0))
  (is-false (= -inf.0 (- (random most-positive-fixnum))))
  (is-false (= +inf.0 (random most-positive-fixnum))))
(test <->
  (is-true (< -inf.0 (- (random most-positive-fixnum)) +inf.0))
  (is-true (> +inf.0 (random most-positive-fixnum) -inf.0)))
(test exp
  (is (= 0.0 (exp (* 1 -inf.0))))
  (is (= (exp (* -1 -inf.0)) +inf.0)))
;; library procedure: finite? z
(test finite?
  (is-false (finite? -inf.0)))
;; library procedure: infinite? z
(test infinite?
  (is-true (infinite? +inf.0)))
;; library procedure: zero? z
;; library procedure: positive? x
(test positive?
  (is-true (positive? +inf.0)))
;; library procedure: negative? x
(test  negagive?
  (is-true (negative? -inf.0)))
;; library procedure: odd? n
;; library procedure: even? n

;; library procedure: max x1 x2 ...
;; library procedure: min x1 x2 ...
(test max-min
  (is (= 4 (max 3 4)))
  (is (inexact? (max 3.9 4)))
  (is (= (max +inf.0 8) +inf.0))
  (is (= (min -inf.0 8) -inf.0)))

;; procedure: + z1 ...
;; procedure: * z1 ...
(test +-*
  (==>
    (+ 3 4)                                ==>  7
    (+ 3)                                  ==>  3
    (+)                                    ==>  0
    (+ +inf.0 +inf.0)                      ==>  +inf.0
    (* 4)                                  ==>  4
    (*)                                    ==>  1
    (* 5 +inf.0)                           ==>  +inf.0
    (* -5 +inf.0)                          ==>  -inf.0
    (* +inf.0 +inf.0)                      ==>  +inf.0
    (* +inf.0 -inf.0)                      ==>  -inf.0)
  (is-true (nanp (+ +inf.0 -inf.0)))
  (is-true (nanp (* 0 +inf.0))))

;; procedure: - z1 z2
;; procedure: - z
;; optional procedure: - z1 z2 ...
;; procedure: / z1 z2
;; procedure: / z
;; optional procedure: / z1 z2 ...
(test /--
  (==>
    (- 3 4)                                ==>  -1
    (- 3 4 5)                              ==>  -6
    (- 3)                                  ==>  -3
    (/ 3 4 5)                              ==>  3/20
    (/ 3)                                  ==>  1/3
    (/ 0.0)                                ==>  +inf.0
    (/ 1.0 0)                              ==>  +inf.0
    (/ -1 0.0)                             ==>  -inf.0
    (/ +inf.0)                             ==>  0.0d0)
  (is-true (nanp (- +inf.0 +inf.0)))
  (is-true (nanp (/ 0 0.0)))
  (is-true (nanp (/ 0.0 0)))
  (is-true (nanp (/ 0.0 0.0))))

;;; library procedure: abs x
(test abs
  (==>
    (abs -7)                               ==>  7
    (abs -inf.0)                           ==>  +inf.0))

;; procedure: quotient n1 n2 x1 x2
;; procedure: remainder n1 n2 x1 x2
;; procedure: modulo n1 n2 x1 x2
(test quotient-remainder-modulo
  (==>
    (quotient 4 2)                   ==> 4/2
    (remainder 4 2)                  ==> 0
    (modulo 4 2)                     ==> 0
    (modulo 13 4)                          ==>  1
    (remainder 13 4)                       ==>  1

    (modulo -13 4)                         ==>  3
    (remainder -13 4)                      ==>  -1

    (modulo 13 -4)                         ==>  -3
    (remainder 13 -4)                      ==>  1

    (modulo -13 -4)                        ==>  -1
    (remainder -13 -4)                     ==>  -1

    (remainder -13 -4.0)                   ==>  -1.0  ; inexact

    (quotient 2/3 1/5)                     ==>  3
    (modulo 2/3 1/5)                       ==>  1/15

    ;;>> todo (quotient .666d0 1/5)                    ==>  3
    (modulo .666d0 1/5)                      ==>  0.06599999999999995d0
    ))

;; library procedure: gcd n1 r1 ...
;; library procedure: lcm n1 r1 ...
(test gcd-lcm
  (==>
    (gcd 32 -36)                           ==>  4
    (gcd)                                  ==>  0
    (lcm 32 -36)                           ==>  288
    (lcm)                                  ==>  1
    (gcd 1/6 1/4)                          ==>  1/12
    (lcm 1/6 1/4)                          ==>  1/2
    (gcd 1/6 5/4)                          ==>  1/12
    (lcm 1/6 5/4)                          ==>  5/2))

;; procedure: numerator q
;; procedure: denominator q
(test numerator-denominator
  (==>
    (numerator (/ 6 4))                    ==>  3
    (denominator (/ 6 4))                  ==>  2
  ;; todo (denominator (exact->inexact (/ 6 4)))            ==> 2.0
    ))

;; procedure: floor x
;; procedure: ceiling x
;; procedure: truncate x
;; procedure: round x
(test floor-ceiling-truncate-round
  (==>
    (floor -4.3)                           ==>  -5.0
    (ceiling -4.3)                         ==>  -4.0
    (truncate -4.3)                        ==>  -4.0
    (round -4.3)                           ==>  -4.0

    (floor 3.5)                            ==>  3.0
    (ceiling 3.5)                          ==>  4.0
    (truncate 3.5)                         ==>  3.0
    (round 3.5)                            ==>  4.0  ; inexact

    (round 7/2)                            ==>  4    ; exact
    (round 7)                              ==>  7))

;; library procedure: exact-floor x
;; library procedure: exact-ceiling x
;; library procedure: exact-truncate x
;; library procedure: exact-round x

;; library procedure: rationalize x y

;; procedure: exp z
;; procedure: log z
;; procedure: sin z
;; procedure: cos z
;; procedure: tan z
;; procedure: asin z
;; procedure: acos z
;; procedure: atan z
;; procedure: atan y x

;; procedure: sqrt z

;; procedure: expt z1 z2
(test expt
  (==>
    (expt 5 3)                  ==>  125
    (expt 5 -3)                 ==>  1/125
    (expt 5 0)                  ==>  1
    (expt 0 5)                  ==>  0
    (expt 0 0)                  ==>  1
    ;; (expt 0 #c(5 .0000312))        ==>  0.0
    ;; (expt 0 -5)                 ==>  +inf.0
    ;; (expt 0 -5+.0000312i)       ==>  +inf.0
    (expt 0 0.0)                ==>  1.0
    (expt 5 +inf.0)             ==>  +inf.0
    (expt 5 -inf.0)             ==>  0.0d0))

;; procedure: make-rectangular x1 x2
;; procedure: make-polar x3 x4
;; procedure: real-part z
;; procedure: imag-part z
;; procedure: magnitude z
;; procedure: angle z
(test angle
  (==>
    (angle +inf.0)              ==> 0.0d0
    (angle -inf.0)              ==> 3.141592653589793d0))
;; procedure: exact->inexact z
;; procedure: inexact->exact z
(test inexact->exact
  (==>
    (inexact->exact .33333d0) ==>  3002369727582815/9007199254740992))
;; procedure: number->string z
;; procedure: number->string z radix
(test number->string
  (==>
    (number->string #x10) ==> "16"
    (number->string #b10) ==> "2"
    (number->string #o10) ==> "8"
    (number->string #e.10) ==> "1/10")
  (for-all ((number (gen-float))
            (radix (gen-integer :max 16 :min 2)) )
    (is-true (= number
                (string->number (number->string number
                                                radix )
                                radix )))))

;; procedure: string->number string
;; procedure: string->number string radix
(test string->number
  (==>
    (string->number "100")                 ==>  100
    (string->number "100" 16)              ==>  256
    (string->number "1e2")                 ==>  100.0
    ;;>> TODO (string->number "15##")                ==>  1500.0
    #.(string->number "+inf.0")              ==>  +inf.0
    #.(string->number "-inf.0")              ==>  -inf.0))


;; *EOF*
