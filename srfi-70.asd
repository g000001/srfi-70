;;;; srfi-70.asd -*- Mode: Lisp;-*-

(cl:in-package :asdf)


(defsystem :srfi-70
  :version "20200321"
  :description "SRFI 70 for CL: Numbers"
  :long-description "SRFI 70 for CL: Numbers
https://srfi.schemers.org/srfi-70"
  :author "Aubrey Jaffer"
  :maintainer "CHIBA Masaomi"
  :license "MIT"
  :serial t
  :depends-on (:fiveam
               :named-readtables)
  :components ((:file "package")
               (:file "readtable")
               (:file "util")
               (:file "srfi-70")
               (:file "test")))


(defmethod perform :after ((o load-op) (c (eql (find-system :srfi-70))))
  (let ((name "https://github.com/g000001/srfi-70")
        (nickname :srfi-70))
    (if (and (find-package nickname)
             (not (eq (find-package nickname)
                      (find-package name))))
        (warn "~A: A package with name ~A already exists." name nickname)
        (rename-package name name `(,nickname)))))


(defmethod perform ((o test-op) (c (eql (find-system :srfi-70))))
  (let ((*package*
         (find-package
          "https://github.com/g000001/srfi-70#internals")))
    (eval
     (read-from-string
      "
      (or (let ((result (run 'srfi-70)))
            (explain! result)
            (results-status result))
          (error \"test-op failed\") )"))))


;;; *EOF*
