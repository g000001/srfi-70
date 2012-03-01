;;;; srfi-70.asd -*- Mode: Lisp;-*-

(cl:in-package :asdf)

(defsystem :srfi-70
  :serial t
  :depends-on (:fiveam
               :named-readtables)
  :components ((:file "package")
               (:file "readtable")
               (:file "util")
               (:file "srfi-70")
               (:file "test")))

(defmethod perform ((o test-op) (c (eql (find-system :srfi-70))))
  (load-system :srfi-70)
  (or (flet ((_ (pkg sym)
               (intern (symbol-name sym) (find-package pkg))))
         (let ((result (funcall (_ :fiveam :run) (_ :srfi-70.internal :srfi-70))))
           (funcall (_ :fiveam :explain!) result)
           (funcall (_ :fiveam :results-status) result)))
      (error "test-op failed") ))
