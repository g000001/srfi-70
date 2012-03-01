;;;; readtable.lisp

(cl:in-package :srfi-70.internal)
(in-readtable :common-lisp)

(defun cl-read (&optional (stream *standard-input*)
                          eof-error-p
                          (eof-value) recursive-p)
  (let ((*readtable* (copy-readtable nil)))
    (cl:read stream eof-error-p eof-value recursive-p)) )

(defun cl-reread (char &optional (stream *standard-input*)
                                 eof-error-p
                                 (eof-value) recursive-p)
  (unread-char char stream)
  (cl-read stream eof-error-p eof-value recursive-p))

(defun +-reader (stream char)
  (let ((peek (peek-char nil stream nil :eof t)))
    (case peek
      ((#\I #\i)
       (let ((expr (read stream t nil t)))
         (if (string-equal "inf.0" expr)
             (ecase char
               (#\+ #+sbcl sb-ext:single-float-positive-infinity
                    #-sbcl '+inf.0)
               (#\- #+sbcl sb-ext:single-float-negative-infinity
                    #-sbcl '-inf.0))
             (intern (format nil "~A~A" char expr)) )))
      (otherwise
       (cl-reread char stream)))))

(let ((*readtable* (copy-readtable nil)))
  (set-macro-character #\+ #'+-reader t)
  (set-macro-character #\- #'+-reader t)
  (read-from-string "(+ +inf.0)"))

(let ((*readtable* (copy-readtable nil)))
  (set-macro-character #\+ #'+-reader t)
  (set-macro-character #\- #'+-reader t)
  (read-from-string "()"))

(defreadtable :srfi-70
  (:merge :standard)
  (:macro-char #\+ #'+-reader t)
  (:macro-char #\- #'+-reader t)
  (:case :upcase))