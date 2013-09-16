(in-package :cl-user)

(defpackage :query-split (:use :cl))

(in-package :query-split)

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
  (let ((new (sublis must-be-table exp)))
    (values new (not (equal new exp)))))

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
     (cons
      (cons expr value)
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

(defun foo (iters depth vars)
  (loop repeat iters
     for exp = (random-expression depth vars)
     collect `((:expression ,(output exp)) (:first-pass ,(output (first-pass-predicate exp))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Testing.

;;; 1. parse and unparse are identities. Generate random expression.
;;; (equal original (parse (unparse original)))

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


;;; 3. First pass predicates are correct.





;;;
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

(defparameter *verbose-check* nil)

(defun test (input-line)
  (let* ((full-expression (parse input-line))
         (first-pass (first-pass-predicate full-expression))
         (all-variables (variables full-expression))
         (cheap-variables (remove-if-not #'cheap-p all-variables))
         (num-cheap (length cheap-variables))
         (num-expensive (- (length all-variables) num-cheap))
         (full-fn (compile nil (expression-fn full-expression all-variables)))
         (cheap-fn (compile nil (expression-fn first-pass cheap-variables))))
    (format t "~&~a~&variables: ~a (~d cheap; ~d expensive)~&cheap: ~a" input-line all-variables num-cheap num-expensive (unparse first-pass))
    (let* ((points 0)
           (ok
            (loop for i below (expt 2 num-cheap)
               for cheap-args = (number-to-booleans i num-cheap)
               for cheap-result = (apply cheap-fn cheap-args)
               when (not cheap-result) do
                 (incf points)
                 (when *verbose-check* (format t "~&args: ~a => ~a" cheap-args cheap-result))

               always (or cheap-result (check-all-false full-fn cheap-args num-expensive)))))
      (values ok points))))

(defun check-all-false (full-fn cheap-args num-expensive)
  (loop for i below (expt 2 num-expensive)
     for args = (append cheap-args (number-to-booleans i num-expensive))
     for result = (apply full-fn args)
     when *verbose-check* do (format t "~&  ~a => ~a" args result)
     never result))






(defun variables (expr)
  (labels ((walk (x acc)
               (typecase x
                 (symbol (if (member x '(nil t and or not)) acc (cons x acc)))
                 (cons (walk (car x) (walk (cdr x) acc))))))
    (sort (delete-duplicates (walk expr ())) #'string<)))

(defun expression-fn (expr variables)
  `(lambda (,@variables) (declare (ignorable ,@variables)) ,expr))

(defun compile-expression (expr variables)
  (compile nil (expression-fn expr variables)))

(defun number-to-booleans (n bits)
  (loop for i downfrom (1- bits) to 0
       collect (ldb-test (byte 1 i) n)))

(defun generate-and-invoke (depth num-variables)
  (let* ((vars (random-variables num-variables 0))
         (expr (random-expression depth vars))
         (source (expression-fn expr vars))
         (fn (compile nil source))
         (args (number-to-booleans (random (expt 2 num-variables)) (1- num-variables))))
    (print `(,source ,args))
    (apply fn args)))
