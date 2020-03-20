;;;; package.lisp

(cl:in-package :cl-user)


(defpackage "https://github.com/g000001/srfi-70"
  (:use)
  (:export 
   finite? infinite? zero? positive? negative? odd?  even? max min abs
   gcd lcm exact-floor exact-ceiling exact-truncate exact-round
   rationalize expt number?  complex?  real?  rational?  integer?  exact?
   inexact?  / = < > <= >= + * - quotient remainder modulo floor ceiling
   truncate round exp log sin cos tan asin acos atan sqrt expt
   make-rectangular make-polar real-part imag-part magnitude angle
   exact->inexact inexact->exact number->string string->number numerator
   denominator))


(defpackage "https://github.com/g000001/srfi-70#internals"
  (:use
   "https://github.com/g000001/srfi-70"
   cl 
   named-readtables 
   fiveam)
  (:shadow lambda member map assoc)
  (:shadowing-import-from
   "https://github.com/g000001/srfi-70"
   lcm gcd expt rationalize max min abs gcd lcm / = < > <= >= + * -
   floor ceiling truncate round exp log sin cos tan asin acos atan atan
   sqrt expt numerator denominator))


;;; *EOF*
