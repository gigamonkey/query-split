(in-package :cl-user)

(defpackage :query-split (:use :cl))

(in-package :query-split)


(defun code-challenge (file)
  (with-open-file (in file)
      (loop for line = (read-line in nil nil) while line
         do (write-line (unparse (first-pass-predicate (parse line)))))))

(defun write-results (file)
  (with-open-file (*standard-output*
                   (make-pathname :type "out" :defaults file)
                   :direction :output :if-exists :supersede)
    (code-challenge file)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Parsing

;;  formula     := variable | literal | expression
;;  variable    := cheap | expensive
;;  cheap       := v[0-9]+
;;  expensive   := w[0-9]+
;;  literal     := "T" | "F"
;;  expression  := conjunction | disjunction | negation
;;  conjunction := "(and" ws formula ws formula ")"
;;  disjunction := "(or" ws formula ws formula ")"
;;  negation    := "(not" ws formula ")"
;;  ws          := " "+

(defun parse (line)
  (subst nil 'f (read-from-string line)))

(defun unparse (expression)
  (typecase expression
    (symbol
     (case expression
       ((t) "T")
       ((nil) "F")
       (t (string-downcase expression))))
    (cons
     (destructuring-bind (op . args) expression
       (format nil "(~(~a~) ~{~a~^ ~})" op (mapcar #'unparse args))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Actual computaion of first-pass-predicate.

(defun first-pass-predicate (expression)
  (simplify (bias-to t (simplify expression))))

(defun bias-to (bias exp)
  (typecase exp
    (symbol
     (if (or (literal-p exp) (cheap-p exp)) exp bias))
    (cons
     (destructuring-bind (op . args) exp
       (let ((bias (if (eql op 'not) (not bias) bias)))
         `(,op ,@(mapcar #'(lambda (x) (bias-to bias x)) args)))))))

(defun literal-p (var) (member var '(t nil)))

(defun cheap-p (var) (char= (char (string var) 0) #\V))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Expression simplification.

(defun simplify (exp)
  (typecase exp
    (symbol exp)
    (cons
     (case (car exp)
       (and (simplify-and/or exp t))
       (or (simplify-and/or exp nil))
       (not (simplify-not exp))))))

(defun simplify-and/or (exp id)
  (destructuring-bind (op arg1 arg2) exp
    (multiple-value-bind (arg1 arg2)
        (rewrite-one-arg (simplify arg1) (simplify arg2) id)
      (cond
        ((or (eql arg1 (not id)) (eql arg2 (not id))) (not id))
        ((eql arg1 id) arg2)
        ((eql arg2 id) arg1)
        ((equal arg1 arg2) arg1)
        ((equal arg1 `(not ,arg2)) (not id))
        ((equal arg2 `(not ,arg1)) (not id))
        (t `(,op ,arg1 ,arg2))))))

(defun simplify-not (exp)
  (destructuring-bind (op arg) exp
    (assert (eql op 'not))
    (let ((arg (simplify arg)))
      (cond
        ((eql arg t) nil)
        ((eql arg nil) t)
        ((and (consp arg) (eql (car arg) 'not)) (cadr arg))
        (t `(not ,arg))))))

(defun rewrite-one-arg (arg1 arg2 id)
  (multiple-value-bind (new1 changed1) (rewrite arg1 (must-be id arg2))
    (multiple-value-bind (new2 changed2) (rewrite arg2 (must-be id arg1))
      (cond
        ((and changed1 changed2)
         (if (> (+ (count-literals new1) (count-literals arg2))
                (+ (count-literals arg1) (count-literals arg1)))
             (values new1 arg2)
             (values arg1 new2)))
        (t (values new1 new2))))))

(defun rewrite (exp must-be-table)
  (let* ((new (sublis must-be-table exp))
         (changed (not (equal new exp))))
    (values (if changed (simplify new) exp) changed)))

(defun count-literals (exp)
  (cond
    ((literal-p exp) 1)
    ((consp exp) (reduce #'+ (mapcar #'count-literals (rest exp))))
    (t 0)))

(defun must-be (value expr)
  "Assuming that expr must evaluate to the given value, return an
  alist of expressions whose value we can determine. E.g. if (and a b)
  must be t then we know both a and b must also be t."
  (typecase expr
    (symbol
     (unless (literal-p expr)
       (list (cons expr value))))
    (cons
     (list*
      `(,expr . value)
      (case (car expr)
        (and
         (when value
           (mapcan #'(lambda (x) (must-be t x)) (cdr expr))))
        (or
         (unless value
           (mapcan #'(lambda (x) (must-be nil x)) (cdr expr))))
        (not (must-be (not value) (cadr expr))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Generating random expressions

;;; (Better way: generate a truth table (in the form of a list of
;;; lists of boolean values for which the expression evaluates to
;;; true) in which we ensure that some of the cheap-variable prefixes
;;; are all false. Those are the ones for which the cheap filter
;;; should be able to return false.)

(defun random-variables (cheap expensive)
  (nconc
   (loop for i from 1 to cheap collect (intern (format nil "V~d" i)))
   (loop for i from 1 to expensive collect (intern (format nil "W~d" i)))))

(defun random-expression (n variables)
  (let ((vars (coerce variables 'vector))
        (ops (vector 'and 'or 'not)))
     (cond
       ((zerop (random n))
        (aref vars (random (length vars))))
       (t
        (let ((op (aref ops (random (length ops)))))
          (case op
            ((and or) `(,op ,(random-expression (1- n) vars)
                            ,(random-expression (1- n) vars)))
            (not `(,op ,(random-expression (1- n) vars)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Testing.

;;; 1. parse and unparse are inverses. Generate random expressions and
;;; check. (equal original (parse (unparse original)))

(defun test-parsing (iters depth cheap expensive)
  (loop with variables = (random-variables cheap expensive)
     repeat iters
     for original = (random-expression depth variables)
     for back = (parse (unparse original))
     unless (equal original back) do (format t "~a != ~a" original back)
     always (equal original (parse (unparse original)))))

;;; 2. (simplify x) and x are equivalent for all x. Generate random x
;;; with n variables. Walk through all possible values for those
;;; variables and check that (eql (apply x-fn args) (apply
;;; simplified-x-fn args))

(defun test-simplify (iters depth cheap expensive)
  (loop with variables = (random-variables cheap expensive)
     repeat iters
     for original      = (random-expression depth variables)
     for simplified    = (simplify original)
     for original-fn   = (compile-expression original variables)
     for simplified-fn = (compile-expression simplified variables)
     always (loop for i below (expt 2 (+ cheap expensive))
               for args = (number-to-booleans i (+ cheap expensive))
               for orig-result = (apply original-fn args)
               for simp-result = (apply simplified-fn args)
               unless (eql orig-result simp-result) do
                 (format t "~&~s ;; orig~&~s ;; simplified~&~a ;; args" original simplified args)
               always (eql orig-result simp-result))))

;;; 3. First pass predicates are correct. Generate a random expression
;;; with n cheap variables and m expensive. Compute a first-pass
;;; predicate. Walk through all possible values of the cheap variables
;;; and whenever the first-pass predicate returns false, check the
;;; full function with all possible values of the expensive variables
;;; (combined with the current values of the cheap variables) to make
;;; sure it is never true.

(defparameter *verbose-check* nil)

(defun test-predicates (iters depth cheap expensive)
  (loop with variables = (random-variables cheap expensive)
     repeat iters
     for original     = (random-expression depth variables)
     for predicate    = (first-pass-predicate original)
     for original-fn  = (compile-expression original variables)
     for predicate-fn = (compile-expression predicate (remove-if-not #'cheap-p variables))
     always (loop for i below (expt 2 cheap)
               for args = (number-to-booleans i cheap)
               for predicate-result = (apply predicate-fn args)
               always (or predicate-result ;; predicate says true, whatever.
                          (loop for j below (expt 2 expensive)
                             for extra-args = (number-to-booleans j expensive)
                             for full-result = (apply original-fn (append args extra-args))
                             if full-result do
                               (format t "~&~s ;; orig~&~s ;; predicate~&~a ;; cheap~&~a ;; expensive" original predicate args extra-args)
                             never full-result)))))

(defun expression-fn (expr variables)
  `(lambda (,@variables) (declare (ignorable ,@variables)) ,expr))

(defun compile-expression (expr variables)
  (let ((*error-output* (make-broadcast-stream)))
    (compile nil (expression-fn expr variables))))

(defun number-to-booleans (n bits)
  (loop for i downfrom (1- bits) to 0
       collect (ldb-test (byte 1 i) n)))
