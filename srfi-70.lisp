;;;; srfi-70.lisp

(cl:in-package :srfi-70.internal)
;; (in-readtable :srfi-70)

(def-suite srfi-70)

(in-suite srfi-70)

(define-function (infinite? z) (and (= z (* 2 z)) (not (zero? z))))
(define-function (finite? z) (not (infinite? z)))

(define-function (ipow-by-squaring x n acc proc)
  (cond ((zero? n) acc)
	((eqv? 1 n) (funcall proc acc x))
        ((negative? n)
         (ipow-by-squaring (funcall proc (/ x) (/ x))
                           (- (quotient n 2))
                           (if (even? n) (/ acc) (funcall proc (/ acc) (/ x)))
                           proc ))
	(:else
         (ipow-by-squaring (funcall proc x x)
                           (quotient n 2)
                           (if (even? n) acc (funcall proc acc x))
                           proc ))))

(define-function (integer-expt x n)
  (ipow-by-squaring x n (if (exact? x) 1 1.0) #'*) )

(define-function (expt z1 z2)
  (cond ((and (exact? z2)
              (not (and (zero? z1) (negative? z2))) )
	 ;; (integer-expt z1 z2)
         (cl:expt z1 z2)
         )
	((zero? z2) (+ 1 (* z1 z2)))
	(:else (exp (* (if (zero? z1) (real-part z2) z2) (log z1))))))

(defun truncate (number &optional (divisor 1))
  (funcall (if (exact? number)
               #'cl:truncate
               #'cl:ftruncate)
           number divisor))

(defun ceiling (number &optional (divisor 1))
  (funcall (if (exact? number)
               #'cl:ceiling
               #'cl:fceiling)
           number divisor))

(defun floor (number &optional (divisor 1))
  (funcall (if (exact? number)
               #'cl:floor
               #'cl:ffloor)
           number divisor))

(defun round (number &optional (divisor 1))
  (funcall (if (exact? number)
               #'cl:round
               #'cl:fround)
           number divisor))

(define-function integer-quotient #'cl:truncate)
(define-function integer-remainder #'cl:rem)
(define-function integer-modulo #'cl:mod)

(define-function (quotient x1 x2)
  (if (and (integer? x1) (integer? x2))
      (integer-quotient x1 x2)
      (truncate (/ x1 x2))))

(define-function (remainder x1 x2)
  (if (and (integer? x1) (integer? x2))
      (integer-remainder x1 x2)
      (- x1 (* x2 (quotient x1 x2)))))

(define-function (modulo x1 x2)
  (if (and (integer? x1) (integer? x2))
      (integer-modulo x1 x2)
      (- x1 (* x2 (floor (/ x1 x2))))))

(define-function (lcm . args)
  (if (null args)
      1
      (/ (apply #'integer-lcm (map #'numerator args))
         (apply #'integer-gcd (map #'denominator args)) )))

(define-function (gcd . args)
  (/ (apply #'integer-gcd (map #'numerator args))
     (apply #'integer-lcm (map #'denominator args))))

(define-function integer-lcm #'cl:lcm)
(define-function integer-gcd #'cl:gcd)

(defun inexact->exact (n)
  (rational n))

(define-function (exact-round x) (inexact->exact (round x)))
(define-function (exact-floor x) (inexact->exact (floor x)))
(define-function (exact-ceiling x) (inexact->exact (ceiling x)))
(define-function (exact-truncate x) (inexact->exact (truncate x)))

(defun max (&rest args)
  (if (every #'exact? args)
      (apply #'cl:max args)
      (exact->inexact (apply #'cl:max args))))

(defun min (&rest args)
  (if (every #'exact? args)
      (apply #'cl:min args)
      (exact->inexact (apply #'cl:min args))))

(defun rational? (n)
  (and (not (infinite? n))
       (or (rationalp n)
           (zerop (- n (cl:rationalize n))))))

(defun / (number &rest more-numbers)
  (sb-int:with-float-traps-masked (:invalid)
    (if (and (member-if #'zerop more-numbers)
             (not (zerop number)))
        (if (plusp number)
            +inf.0
            -inf.0)
        (if (and (zerop number)
                 (null more-numbers))
            +inf.0
            (apply #'cl:/ number more-numbers)))))

(defun + (&rest args)
  (sb-int:with-float-traps-masked (:invalid)
    (apply #'cl:+ args)))

(defun * (&rest args)
  (sb-int:with-float-traps-masked (:invalid)
    (apply #'cl:* args)))

(defun - (number &rest more-numbers)
  (sb-int:with-float-traps-masked (:invalid)
    (apply #'cl:- number more-numbers)))


(declaim (inline number->string))
(defun number->string (num &optional (radix 10.))
  (cond ((= +inf.0 num) "+inf.0")
        ((= -inf.0 num) "-inf.0")
        (T (cl:write-to-string num :base radix))))

(defun string->number (str &optional (radix 10.))
  ;;>> TODO (string->number "15##")                ==>  1500.0
  (let ((*read-base* radix))
    (values (read-from-string str))))

;;; eof
