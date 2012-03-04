;;;; readtable.lisp

(cl:in-package :srfi-70.internal)
(in-readtable :common-lisp)


;;; TODO: nnEnn notation

(defun remove-dot (str)
  (let ((cnt (count #\. str)))
    (cond ((zerop cnt)
           (values str 0) )
          ((< 1 cnt)
           (error "~A: too many dots." str) )
          (T (if (zerop (position #\. str))
                 (values (format nil "0~A" (subseq str 1)) 1)
                 (values (remove #\. (remove #\. str))
                         (position #\. str) ))))))

(defun read-digitchars (stream)
  (cl:loop
     :for c := (read-char stream nil)
     :while (and c
                 (or (digit-char-p c)
                     (char= #\. c) ))
     :collect c :into cs
     :finally (progn
                (and (sb-impl::token-delimiterp c)
                     (unread-char c stream))
                (return (coerce cs 'string))) ))

(defun |#e-reader| (stream subchar arg)
  (declare (ignore subchar arg))
  (let ((n (read-digitchars stream)))
    (multiple-value-bind (n pos)
                         (remove-dot n)
      (multiple-value-bind (n len)
                           (parse-integer n)
        (if (zerop pos)
            n
            (/ n (expt 10. (- len pos))) )))))


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
               (#\+ #+sbcl sb-ext:double-float-positive-infinity
                    #-sbcl '+inf.0)
               (#\- #+sbcl sb-ext:double-float-negative-infinity
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
  (:dispatch-macro-char #\# #\e #'|#e-reader|)
  (:case :upcase))
