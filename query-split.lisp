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
    (symbol (case expression
              ((t) "T")
              ((nil) "F")
              (t (format nil "~(~a~)" expression))))
    (cons
     (destructuring-bind (op . args) expression
       (format nil "(~(~a~) ~{~a~^ ~})" op (mapcar #'unparse args))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Actual computaion of first-pass-predicate.

(defun first-pass-predicate (expression)
  (simplify (bias-to t expression)))

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
;;;
;;; Boolean expression simplification. We can simplify boolean
;;; expressions in a number of ways, some of which will be
;;; particularly useful for the expressions that come out of bias-to
;;; since we are likely to end up with expressions with more than the
;;; usual number of literals which can make more complex expressions
;;; take on known values. (E.g. (or v1 t) => t)
;;;
;;; Basic strategy: first flatten out the expression so that things
;;; like (and a (and b (and c))) turn into (and a b c). (We can also
;;; be a bit clever with De Morgan's Laws to flatten (and a (not (or b
;;; c))) into (and a (not b) (not c)).
;;;
;;; Now we really go to town, working from the bottom up. First
;;; simplify any ands containing literal nils, ors containing literal
;;; ts and nots containing any literal. In the same pass, we can
;;; probably eliminate any literal ts from ands and literal nils from
;;; ors. And eliminate duplicates sub-expressions from either. And if
;;; after all that we end up with empty ands or ors they can be
;;; replaced with their identity value.
;;;
;;; Perhaps at the same time

;;; (and v1 ... complex expressions eventually using v1 ...)
;;;
;;; Can still work bottom up, simplifying any ands and ors deep down
;;; in the expression first, then as we work our way back up, we may
;;; reach down into those expressions to change them based on
;;; higher-level assumed values. We could either re-simplify any
;;; expression after it is changed by rewriting or we could apply all
;;; rewriting all the way up to the top and then resimplify the whole
;;; tree. Probably better to do it on the way up since the worst that
;;; can happen is a low-level simplification would remove some
;;; expression which a higher-level rewrite might have rewritten.
;;;
;;; Any rewrites done at a low-level that lead to simplifiable
;;; expressions, will still be there after doing higher-level
;;; rewrites. So might as well do the rewrites bottom up and then do
;;; another, single, bottom-up simplification pass to simplify
;;; anything that came up during the rewrites.
;;;
;;; So: after flattening and doing one bottom-up simplification, do a
;;; bottom up, rewrite and simplify pass.

;;;
;;; Next, we can try to simplify expressions by replacing
;;; sub-expressions with the values they must have in order for
;;; enclosing expressions to have the value they must have. To take a
;;; simple example, if our whole expression was (and a (or a b) we can
;;; say, well, if a is nil the whole expression will be nil regardless
;;; of what the value of (or a b) is. Therefore we can rewrite (or a
;;; b) under the assumption that a is true. Thus (and a (or a b)) =>
;;; (and a (or t b)) which we can later simplify to (and a t) and
;;; finally to a. Which is kind of interesting in that the value of b
;;; has gone away entirely but that's actually logically correct. This
;;; step, however, could lead to an exponential explosion in the
;;; amount of work.

;;; sub-expressions. For example In
;;; any AND expression we can take each operand in turn and assume
;;; that it is true and determine any other expressions that will have
;;; known values under that assumption. Then see if any of those
;;; expressions occur in any of the other operands and replace them
;;; with the literal value. (This is legit because if we have (and a
;;; b), it can only be true if a is true. Therefore we can assume a is
;;; true and replace any occurence of it in b with t. If a is not true
;;; then the and will evaluate to false and it doesn't matter if
;;; changing a to t has made the value of b change. And if a is true,
;;; then replacing it with t is totally legit. And likewise with b
;;; since (and a b) === (and b a). This will handle the trivial case
;;; of (and a (not a)) by transforming it to (and a (not t)) => (and a
;;; nil) which will get turned into nil in the next step.
;;;
;;;
;;; Finally, remove any literal t's from and expressions and any
;;; literal nil's from or expressions and convert empty ands to t and
;;; or to nil. Again, this should probably be done from the bottom up.


(defun simplify (exp)
  (typecase exp
    (symbol
     exp)
    (cons
     (destructuring-bind (operator . args) exp
       (let ((simple-args (mapcar #'simplify args)))
         (case operator
           (and (simplify-and/or operator simple-args t))
           (or (simplify-and/or operator simple-args nil))
           (not (simplify-not simple-args))))))))

(defun flatten (exp)
  "Flatten expression so binary and's and or's are turned into multi-arg versions."
  (typecase exp
    (symbol exp)
    (cons
     (destructuring-bind (op . args) exp
       (case op
         ((and or)
          `(,op ,@(mapcan #'(lambda (x) (flat-terms op x)) (mapcar #'flatten args))))
         (not `(not ,(flatten (first args)))))))))

(defun flat-terms (op exp)
  "Get the list of terms we can extract from an expression, assuming
we're in the context of op."
  (typecase exp
    (symbol (list exp))
    (cons
     (cond
       ((eql op (car exp)) (rest exp))
       ((de-morgan-p op exp) (mapcar #'(lambda (x) `(not ,x)) (cdadr exp)))
       (t (list exp))))))

(defun de-morgan-p (op exp)
  (and (consp exp)
       (eql 'not (car exp))
       (consp (cadr exp))
       (eql (case op (and 'or) (or 'and)) (caadr exp))))


(defun must-be (value expr)
  "Assuming that expr must evaluate to the given value, return an
  alist of expressions whose value we can determine. E.g. if (and a b)
  must be t then we know both a and b must also be t."
  (typecase expr
    (symbol (list (cons expr value)))
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


;; (and (and a b) (and d c)) => `(,and ,@(and-terms '(and a b)) ,@(and-terms '(and d c)))

;;; Some other simplifications:

;;; (or a X) or (or X a)               => can replace all occurences of a in X with nil and then simplify X.
;;; (and a X) or (and X a)             => can replace all occurences of a in X with t and then simplify X
;;; (or (not a) X) or (or X (not a))   => can replace all occurences of a in X with t and then simplify X.
;;; (and (not a) X) or (and X (not a)) => can replace all occurences of a in X with nil and then simplify X

(defun simplify-and/or (op simple-args identity)
  (let ((args (remove-duplicates (remove identity simple-args) :test #'equal)))
    (cond
      ;; no interesting args, identity for this operator
      ((not args) identity)

      ;; (and ... nil ...) is nil and (or ... t ...) is t
      ((member (not identity) args) (not identity))

      ;; Only one distinct arg, it's the value
      ((only-one args) (first args))

      ;; !a & a => nil; !a | a => t
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

(defparameter *verbose-check* nill)

(defun test (input-line)
  (let* ((full-expression (parse input-line))
         (first-pass (first-pass-predicate full-expression))
         (all-variables (variables full-expression))
         (cheap-variables (remove-if-not #'cheap-p all-variables))
         (num-cheap (length cheap-variables))
         (num-expensive (- (length all-variables) num-cheap))
         (full-fn (compile nil (compile-expression full-expression all-variables)))
         (cheap-fn (compile nil (compile-expression first-pass cheap-variables))))
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

(defun compile-expression (expr variables)
  `(lambda (,@variables) (declare (ignorable ,@variables)) ,expr))

(defun number-to-booleans (n bits)
  (loop for i downfrom (1- bits) to 0
       collect (ldb-test (byte 1 i) n)))

(defun generate-and-invoke (depth num-variables)
  (let* ((vars (random-variables num-variables 0))
         (expr (random-expression depth vars))
         (source (compile-expression expr vars))
         (fn (compile nil source))
         (args (number-to-booleans (random (expt 2 num-variables)) (1- num-variables))))
    (print `(,source ,args))
    (apply fn args)))
