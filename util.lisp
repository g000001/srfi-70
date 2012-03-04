(cl:in-package :srfi-70.internal)

(defconstant +eof+
  (if (boundp '+eof+)
      (symbol-value '+eof+)
      (list nil)))

(defmacro defsynonymfun (name fcn)
  `(setf (fdefinition ',name) ,fcn))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (progn
    (defsynonymfun = #'cl:=)
    (defsynonymfun < #'cl:<)
    (defsynonymfun > #'cl:>)
    (defsynonymfun <= #'cl:<=)
    (defsynonymfun >= #'cl:>=)
    (defsynonymfun exp  #'cl:exp)
    (defsynonymfun log  #'cl:log)
    (defsynonymfun sin  #'cl:sin)
    (defsynonymfun cos  #'cl:cos)
    (defsynonymfun tan  #'cl:tan)
    (defsynonymfun asin #'cl:asin)
    (defsynonymfun acos #'cl:acos)
    (defsynonymfun atan #'cl:atan)
    (defsynonymfun sqrt #'cl:sqrt)
    (defsynonymfun angle #'cl:phase)
    (defsynonymfun magnitude #'cl:abs)
    (defsynonymfun make-rectangular #'cl:complex)
    (defsynonymfun numerator #'cl:numerator)
    (defsynonymfun denominator #'cl:denominator)
    (setf (fdefinition 'abs) #'cl:abs)
    (setf (fdefinition 'eq?) #'cl:eq)
    (setf (fdefinition 'negative?) #'cl:minusp)
    (setf (fdefinition 'null?) #'cl:null)
    (setf (fdefinition 'pair?) #'cl:consp)
    (setf (fdefinition 'positive?) #'cl:plusp)
    (setf (fdefinition 'zero?) #'cl:zerop)
    (setf (fdefinition 'vector-length) #'cl:length)
    (setf (fdefinition 'vector?) #'cl:vectorp)
    (setf (fdefinition 'procedure?) #'cl:functionp)
    (setf (fdefinition 'even?) #'cl:evenp)
    (setf (fdefinition 'newline) #'cl:terpri)
    (setf (fdefinition 'display) #'cl:princ)
    (setf (fdefinition 'string-length)  #'cl:length)
    (setf (fdefinition 'char->integer)  #'cl:char-code)
    (setf (fdefinition 'string-ref) #'cl:char)
    (setf (fdefinition 'symbol->string) #'cl:string)
    (setf (fdefinition 'string?) #'cl:stringp)
    (setf (fdefinition 'symbol?) #'cl:symbolp)
    (setf (fdefinition 'number?) #'cl:numberp)
    (setf (fdefinition 'char?) #'cl:characterp)
    (setf (fdefinition 'real-part) #'cl:realpart)
    (setf (fdefinition 'imag-part) #'cl:imagpart)
    (setf (fdefinition 'string=?) #'cl:string=)
    (setf (fdefinition 'string-ci=?) #'cl:string-equal)
    (setf (fdefinition 'map) #'cl:mapcar)
    (setf (fdefinition 'char=?) #'cl:char=)
    (setf (fdefinition 'char<?) #'cl:char<)
    (setf (fdefinition 'char-ci=?) #'cl:char-equal)
    (setf (fdefinition 'char-ci<?) #'cl:char-lessp)
    (setf (fdefinition 'string<?) #'cl:string<)
    (setf (fdefinition 'string-ci<?) #'cl:string-lessp)
    (setf (fdefinition 'real?) #'cl:realp)
    (setf (fdefinition 'exists) #'cl:some)
    ))

(defun make-polar (r th)
  (* r (cis th)))

(defun integer? (n)
  (or (zerop n)
      (integerp n)
      (and (not (= (* 2 n) n))
           (= (truncate n) n)) ))

(defun real? (n)
  (and (numberp n)
       (or (realp n)
           (zerop (cl:imagpart n)))))

(defun complex? (n)
  (numberp n))

(defun exact->inexact (n)
  (float n 0d0))

(defun exact? (n)
  (rationalp n))

(defun inexact? (n)
  (floatp n))

(defun list? (obj)
  (and (cl:listp obj)
       (cl:tailp '() obj)))

(defmacro set! (var val)
  `(setq ,var ,val))

(declaim (cl:inline list-tail vector-set! list-ref vector->list list->vector
                    quotient set-car! set-cdr! eqv?
                    assq assv assoc for-each memq))

(defun eqv? (x y)
  (cl:eql x y))

(defun member (item list)
  (cl:do ((e list (cdr e)))
       ((cl:atom e))
    (cl:when (cl:eql item (car e))
      (cl:return e))))

(defun memq (item list)
  (cl:do ((e list (cdr e)))
       ((cl:atom e))
    (cl:when (cl:eq item (car e))
      (cl:return e))))


(defun for-each (fn cl:&rest lists)
  (cl:apply #'cl:mapc fn lists)
  nil)

(defun assq (item alist)
  (cl:assoc item alist :test #'eq?))

(defun assv (item alist)
  (cl:assoc item alist :test #'eqv?))

(defun assoc (item alist)
  (cl:assoc item alist :test #'equal?))

(defun equal? (x y)
  (cl:equal x y))

(defun set-car! (list obj)
  (cl:rplaca list obj))

(defun set-cdr! (cons x)
  (cl:rplacd cons x))

(defun list-tail (list k)
  (cl:nthcdr k list))

(defun list-ref (list k)
  (cl:nth k list))

(defun vector-set! (vec index val)
  (setf (cl:aref vec index) val))

(defun string-set! (str index val)
  (setf (cl:char str index) val))

(defun vector->list (vec)
  (cl:coerce vec 'list))

(defun list->vector (list)
  (cl:coerce list 'cl:vector))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun to-proper-lambda-list (list)
    (cl:typecase list
      (cl:list (if (cl:tailp () list)
                   list
                   (cl:let ((last (cl:last list)))
                     `(,@(cl:butlast list)
                         ,(car last)
                         cl:&rest
                         ,(cdr last)))))
      (cl:symbol `(cl:&rest ,list)))))

(defmacro lambda (args &rest body)
  `(cl:lambda ,(to-proper-lambda-list args)
     ,@body))

(defmacro letrec ((&rest binds) &body body)
  `(let (,@(cl:mapcar (cl:lambda (x)
                        `(,(car x) #'values) )
             binds ))
     (declare (optimize (space 3)))
     (labels (,@(cl:remove nil
                  (cl:mapcar (cl:lambda (x &aux (name (car x)))
                               `(,name
                                 (&rest args)
                                 (apply ,name args) ))
                             binds )))
       (declare (optimize (debug 0) (space 3)))
       (psetq ,@(cl:apply #'cl:append binds))
       ,@body )))

(defmacro define-function (name-args &body body)
  (if (cl:consp name-args)
      (cl:destructuring-bind (name . args)
                             name-args
        `(defun ,name ,(to-proper-lambda-list args)
           ,@body))
      `(progn
         (eval-when (:compile-toplevel :load-toplevel :execute)
           (setf (fdefinition ',name-args)
                 (function cl:values) ))
         (setf (fdefinition ',name-args)
               ,(car body) ))))

(declaim (inline vector-ref))
(defun vector-ref (vec k)
  (cl:svref vec k))

(defmacro begin (&body body)
  `(progn ,@body))

(declaim (inline make-vector))
(defun make-vector (size &optional (init 0))
  (cl:make-array size                   ;***
                 :initial-element init
                 :adjustable nil
                 :fill-pointer nil))

(declaim (inline string-append))
(defun string-append (&rest strings)
  (cl:format nil "~{~A~}" strings))

(defmacro dolex ((&rest varlist) endlist &body body)
  (let* ((vars (cl:mapcar (lambda (v)
                            (if (cl:consp v) (car v) v) )
                          varlist ))
         (binds (cl:mapcar (lambda (b)
                             (if (cl:consp b)
                                 (cl:destructuring-bind (var &optional init next)
                                      b
                                   (if next
                                       `(,var ,init
                                              (let (,@(cl:mapcar (lambda (x)
                                                                   (list x x) )
                                                        vars ))
                                                (declare (ignorable ,@vars))
                                                ,next ))
                                       `(,var ,init) ))
                                 (list b nil) ))
                           varlist )))
    `(cl:do ,binds ,endlist ,@body) ))


(defmacro with-local-define-function (&body defines-body)
  (or (cl:member :in defines-body) (error "no body"))
  (let* ((body-pos (cl:position :in defines-body))
         (defines  (cl:subseq defines-body 0 body-pos))
         (body     (cl:subseq defines-body (cl:1+ body-pos))) )
    (cl:loop
       :for (nil name-arg . bo) :in defines
       :collect (cl:let ((name-arg (to-proper-lambda-list name-arg)))
                  `(,(car name-arg) ,(cdr name-arg) ,@bo) )
       :into defs
       :finally (cl:return
                  `(labels (,@defs)
                     ,@body )))))

(defmacro with-local-define-variable (&body defines-body)
  (or (cl:member :in defines-body) (error "no body"))
  (let* ((body-pos (cl:position :in defines-body))
         (defines  (cl:subseq defines-body 0 body-pos))
         (body     (cl:subseq defines-body (cl:1+ body-pos))) )
    (cl:loop
       :for (nil v bo) :in defines
       :collect v :into vars
       :collect v :into setqs
       :collect bo :into setqs
       :finally (cl:return
                  `(cl:let (,@vars)
                     (cl:psetq ,@setqs)
                     ,@body )))))

(defun boolean? (obj)
  (cl:typep obj '(cl:member cl:t cl:nil)))


(defun eof-object? (obj)
  (eq obj +eof+))

(defmacro iterate (tag specs &body body)
  (let ((vars  (mapcar #'car specs))
        (vals  (mapcar #'cadr specs))
	(id    (gensym))
        (dvars (map-into (make-list (length specs)) #'gensym)))
    `(block ,id
       (let ,(mapcar #'list dvars vals)
         (macrolet ((,tag ,vars
                      `(progn (psetq ,@(list ,@(mapcan #'(cl:lambda (dvar var)
                                                           `(',dvar ,var))
                                                       dvars
                                                       vars)))
                              (go ,',id))))
           (tagbody
             ,id
             (let ,(mapcar #'list vars dvars)
               (return-from ,id (progn ,@body)))))))))


;;; eof
