(in-package :cl-user)

(defpackage :query-split (:use :cl))

(in-package :query-split)

;; (((v1 & (v2 | v3)) & !w1) | T)

(defparameter *example* "(((v1 & (v2 | v3)) & !w1) | T)")

(defvar *cheap* ())
(defvar *expensive* ())

(defun parse (line)
  (let ((*cheap* ())
        (*expensive* ()))
    (multiple-value-bind (exp end) (parse-expression line 0)
      (if (and end (skip-whitespace line end))
          (error "Trailing garbage: '~a'" (subseq line (skip-whitespace line end))))
      (values exp *cheap* *expensive*))))

(defun output (expression)
  (with-output-to-string (s) (output-expression expression s)))

(defun output-expression (expression out)
    (typecase expression
      (symbol
       (case expression
         ((t) (format out "T"))
         ((nil) (format out "F"))
         (t (format out "~(~a~)" expression))))
      (cons
       (case (first expression)
         (and (format out "(")
              (output-expression (second expression) out)
              (format out " & ")
              (output-expression (third expression) out)
              (format out ")"))
         (or (format out "(")
              (output-expression (second expression) out)
              (format out " | ")
              (output-expression (third expression) out)
              (format out ")"))
         (not (format out "!")
              (output-expression (second expression) out))))))

(defun parse-expression (line idx)
  (let ((idx (skip-whitespace line idx)))
    (unless (or (not idx) (= idx (length line)))
      (case (char line idx)
        (#\(
         (let ((start idx))
           (multiple-value-bind (exp1 idx) (parse-expression line (1+ idx))
             (multiple-value-bind (op idx) (parse-op line (skip-whitespace line idx))
               (multiple-value-bind (exp2 idx) (parse-expression line idx)
                 (if (and idx (< idx (length line)) (eql (char line idx) #\)))
                     (values `(,op ,exp1 ,exp2) (1+ idx))
                     (error "Expression not closed opened at ~d" start)))))))
        (#\!
         (multiple-value-bind (exp idx) (parse-expression line (1+ idx))
           (values `(not ,exp) idx)))
        ((#\v #\w) (parse-symbol line idx))
        (#\T (values t (1+ idx)))
        (#\F (values nil (1+ idx)))))))

(defun skip-whitespace (line idx)
  (position-if-not #'(lambda (c) (eql c #\Space)) line :start idx))

(defun parse-op (line idx)
  (assert (member (char line idx) '(#\& #\|)))
  (values
   (case (char line idx) (#\& 'and) (#\| 'or))
   (1+ idx)))

(defun parse-symbol (line idx)
  (assert (member (char line idx) '(#\v #\w)))
  (let* ((end (position-if-not #'digit-char-p line :start (1+ idx)))
         (var (intern (string-upcase (subseq line idx end)))))
    (case (char line idx)
      (#\v (pushnew var *cheap*))
      (#\w (pushnew var *expensive*)))
    (values var end)))

(defun first-pass-predicate (expression cheap-variables)
  "Compute an new expression that only uses the variables in
  `cheap-variables' and which returns true whenever `expression' does
  but which may (conservatively) return true when expression would
  actually return false. Thus the resulting expression can be used as
  a first-pass predicate to find items that may belong in the set
  matched by expression."
  (simplify (bias-to-true expression (list* t nil cheap-variables))))

(defun bias-to-true (expression cheap)
  (bias expression cheap #'bias-to-true #'bias-to-false t))

(defun bias-to-false (expression cheap)
  (bias expression cheap #'bias-to-false #'bias-to-true nil))

(defun bias (expression cheap same-bias opposite-bias default)
  (typecase expression
    (symbol
     (if (member expression cheap) expression default))
    (cons
     (destructuring-bind (operator . args) expression
       (case operator
         (and `(and ,@(mapcar #'(lambda (x) (funcall same-bias x cheap)) args)))
         (or  `(or ,@(mapcar #'(lambda (x) (funcall same-bias x cheap)) args)))
         (not `(not ,(funcall opposite-bias (first args) cheap))))))))

(defun simplify (expression)
  (typecase expression
    (symbol expression)
    (cons
     (destructuring-bind (operator . args) expression
       (let ((simple-args (mapcar #'simplify args)))
         (case operator
           (and (simplify-and/or operator simple-args t))
           (or (simplify-and/or operator simple-args nil))
           (not (simplify-not simple-args))))))))

;;; Some other simplifications:

;;; (or a X) or (or X a)               => can replace all occurences of a in X with nil and then simplify X.
;;; (and a X) or (and X a)             => can replace all occurences of a in X with t and then simplify X
;;; (or (not a) X) or (or X (not a))   => can replace all occurences of a in X with t and then simplify X.
;;; (and (not a) X) or (and X (not a)) => can replace all occurences of a in X with nil and then simplify X

(defun simplify-and/or (op simple-args identity)
  (let ((args (remove-duplicates (remove identity simple-args) :test #'equal)))
    (cond
      ((not args) identity)
      ((member (not identity) args) (not identity))
      ((only-one args) (first args))
      ((opposite-p (first args) (second args)) (not identity))
      ((and (symbolp (first args)) (consp (second args)))
       (let* ((sym (first args))
              (other (second args))
              (other-simplified (simplify (subst identity sym other))))
         (if (equal other other-simplified)
             `(,op ,sym ,other)
             (simplify `(,op ,sym ,other-simplified)))))
      ((and (symbolp (second args)) (consp (first args)))
       (let* ((sym (second args))
              (other (first args))
              (other-simplified (simplify (subst identity sym other))))
         (if (equal other other-simplified)
             `(,op ,other ,sym)
             (simplify `(,op ,other-simplified ,sym)))))
      (t `(,op ,@args)))))

(defun simplify-not (simple-args)
  (destructuring-bind (arg) simple-args
    (cond
      ((eql arg t) nil)
      ((eql arg nil) t)
      ((not-expression-p arg) (second arg))
      (t `(not ,arg)))))

(defun only-one (list) (not (rest list)))

(defun not-expression-p (x) (and (consp x) (eql (first x) 'not)))

(defun opposite-p (arg1 arg2)
  (or (and (not-expression-p arg1) (equal (second arg1) arg2))
      (and (not-expression-p arg2) (equal (second arg2) arg1))))

(defun random-variables (cheap expensive)
  (nconc
   (loop for i from 1 to cheap collect (intern (format nil "V~d" i)))
   (loop for i from 1 to expensive collect (intern (format nil "W~d" i)))))

(defun random-expression (n variables)
  (let ((vars (coerce variables 'vector))
        (ops (vector 'and 'or 'not)))
    (simplify
     (cond
       ((zerop (random n))
        (aref vars (random (length vars))))
       (t
        (let ((op (aref ops (random (length ops)))))
          (case op
            ((and or) `(,op ,(random-expression (1- n) vars)
                            ,(random-expression (1- n) vars)))
            (not `(,op ,(random-expression (1- n) vars))))))))))

(defun foo (iters depth vars number-cheap)
  (let ((cheap (ldiff vars (nthcdr number-cheap vars))))
    (loop repeat iters
       for exp = (random-expression depth vars)
       collect `((:expression ,(output exp)) (:first-pass ,(output (first-pass-predicate exp cheap)))))))


;;; To test: generate a random expression. Convert to first-pass
;;; predicate. Turn each into a function. For each permutation of the
;;; cheap variables get the value of the first-pass function and make
;;; sure there's some permutation of the expensive variables that when
;;; passed, along with the same cheap variable values, to the full
;;; function yields the same result. I.e. if the first-pass function
;;; says true, there must exist a permutation of expensive vars that
;;; the full returns true. And if the first-pass returns false, then
;;; all permutations of the expensive vars must return false.

;;; Scoring: score = input.count { x => !g(x) }

;;; I.e. given the correctness constraint that g(x) must return true
;;; when f(x) does, maximize the number of items that it excludes by
;;; returning false. To find the best possible score, find all the
;;; permutations of all variables such that f(x) is always
;;; false. (Those are the ones for which g(x) could legitimately
;;; return false.) Then from the unique permutations of the cheap
;;; variables, see for how many g(x) returns false.

(defun variables (expr)
  (labels ((walk (x acc)
               (typecase x
                 (null acc)
                 (symbol
                  (if (member x '(and or not)) acc (cons x acc)))
                 (cons (walk (car x) (walk (cdr x) acc))))))
    (sort (delete-duplicates (walk expr ())) #'string<)))

(defun compile-expression (expr)
  (let ((params (variables expr)))
    `(lambda (,@params) ,expr)))

(defun number-to-booleans (n bits)
  (loop for i downfrom bits to 0
       collect (ldb-test (byte 1 i) n)))
