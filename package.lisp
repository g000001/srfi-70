;;;; package.lisp

(cl:in-package :cl-user)

(defpackage :srfi-70
  (:use)
  (:export :finite? :infinite? :zero? :positive? :negative? :odd?
           :even? :max :min :abs :gcd :lcm :exact-floor
           :exact-ceiling :exact-truncate :exact-round :rationalize
           :expt
           :number?
           :complex?
           :real?
           :rational?
           :integer?
           :/
           ))

(defpackage :srfi-70.internal
  (:use :srfi-70 :cl :named-readtables :fiveam)
  (:shadow :lambda :member :map :assoc)
  (:shadowing-import-from :srfi-70 :lcm :gcd :expt
                          :rationalize :max :min :abs
                          :gcd :lcm :/))
