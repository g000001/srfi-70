(cl:in-package :srfi-70.internal)
(in-readtable :srfi-70)

(def-suite srfi-70)

(in-suite srfi-70)

;;; ???
(defconstant |+INF.0| +inf.0)
(defconstant |-INF.0| -inf.0)

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

(test inf
  (is-true (= (+ 1 +inf.0) +inf.0))
  (is-true (= +inf.0 +inf.0))
  (is-false (= -inf.0 +inf.0))
  (is-true (= -inf.0 -inf.0))
  (is-false (= -inf.0 (- (random most-positive-fixnum))))
  (is-false (= +inf.0 (random most-positive-fixnum)))
  (is-true (< -inf.0 (- (random most-positive-fixnum)) +inf.0))
  (is-true (> +inf.0 (random most-positive-fixnum) -inf.0))
  (is (= 0.0 (exp (* 1 -inf.0))))
  (is (= (exp (* -1 -inf.0)) +inf.0))
  (is-true (complex? #c(3 4)))
  (is-true (complex? 3))
  (is-true (real? 3))
  (is-true (real? #c(-2.5 0.0)))
  (is-true (real? 1e10))
  (is-true (rational? 6/10))
  (is-true (rational? 6/3))
  (is-true (integer? #c(3 0)))
  (is-true (integer? 3.0))
  (is-true (integer? 8/4))
  (is-true (complex? +inf.0))
  (is-true (real? -inf.0))
  (is-false (rational? +inf.0))
  (is-false (integer? -inf.0))
  (is-true (exact? 5))
  (is-true (inexact? +inf.0))
  (is-true (positive? +inf.0))
  (is-true (negative? -inf.0))
  (is-false (finite? -inf.0))
  (is-true (infinite? +inf.0))
  (is (= 4 (max 3 4)))
  (is (inexact? (max 3.9 4)))
  (is (= (max +inf.0 8) +inf.0))
  (is (= (min -inf.0 8) -inf.0))
  (==>
    (+ 3 4)                                ==>  7
    (+ 3)                                  ==>  3
    (+)                                    ==>  0
    (+ +inf.0 +inf.0)                      ==>  +inf.0
    ;;  (+ +inf.0 -inf.0)                      ==>  0/0
    (* 4)                                  ==>  4
    (*)                                    ==>  1
    (* 5 +inf.0)                           ==>  +inf.0
    (* -5 +inf.0)                          ==>  -inf.0
    (* +inf.0 +inf.0)                      ==>  +inf.0
    (* +inf.0 -inf.0)                      ==>  -inf.0
    ;;  (* 0 +inf.0)                           ==>  0/0
    )
  (==>
    (- 3 4)                                ==>  -1
    (- 3 4 5)                              ==>  -6
    (- 3)                                  ==>  -3
    ;; (- +inf.0 +inf.0)                      ==>  0/0
    (/ 3 4 5)                              ==>  3/20
    (/ 3)                                  ==>  1/3
    (/ 0.0)                                ==>  (/ 0.0)
    (/ 1.0 0)                              ==>  +inf.0
    (/ -1 0.0)                             ==>  -inf.0
    ;; (/ +inf.0)                             ==>  0.0
    ;; (/ 0 0.0)                              ==>  0/0
    ;; (/ 0.0 0)                              ==>  0/0
    ;; (/ 0.0 0.0)                            ==>  0/0
    )
  (==>
    (abs -7)                               ==>  7
    (abs -inf.0)                           ==>  +inf.0)

  (==>
    (quotient 4 2)                   ==> 4/2
    (remainder 4 2)                  ==> 0
    (modulo 4 2)                     ==> 0)
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
    (expt 5 -inf.0)             ==>  0.0))
