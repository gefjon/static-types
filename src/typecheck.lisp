(in-package :static-types)

(defun cons-car-eq-p (list expected-car)
  "test that list is a list, and that its car is eq to expected-car"
  (and (consp list) (eq (car list) expected-car)))

(defmacro expr-case (expr &rest clauses)
  (flet ((canonicalize-clause (clause)
           (alexandria:destructuring-case clause
             (((:repr :representational) type)
              `((typep ,expr ',type) ',type))
             ((:expr keyword &body forms)
              `((cons-car-eq-p ,expr ',keyword) ,@forms))
             ((:test predicate &body forms)
              `((funcall #',predicate ,expr) ,@forms))
             (((:t :otherwise) &body forms)
              `(t ,@forms)))))
    `(cond ,@(mapcar #'canonicalize-clause clauses))))

(compiler-defun macro-expr-p (expr)
  (and (consp expr)
       (let ((ident (first expr)))
         (macro-function ident))))

(compiler-defun typecheck (expr &optional (env *global-type-env*))
  (expr-case expr
             (:repr null)
             (:repr typed:bool)
             (:repr typed:int)
             (:test symbolp (cdr (type-env-values-assoc expr env)))
             (:expr progn
                    (typecheck-implicit-progn (cdr expr) env))
             (:expr let
                    (typecheck-let-expr expr env))
             (:expr typed:lambda
                    (typecheck-lambda-expr expr env))
             (:expr typed:force-type
                    (typecheck-force-type-expr expr env))
             (:expr funcall
                    (typecheck-funcall-expr expr env))
             (:expr check-type
                    (typecheck-check-type-expr expr env))
             (:expr function
                    (cdr (type-env-functions-assoc (second expr) env)))


             (:test macro-expr-p
                    (typecheck (macroexpand-1 expr) env))

             ;; keep this clause last - it tries to treat the list as
             ;; either a macro expansion or a funcall, but you have to
             ;; make sure it's not a special form first.
             (:test consp
                    (typecheck-funcall-expr `(funcall #',(car expr)
                                                      ,@(cdr expr))
                                            env))

             (:t (error "i don't know how to typecheck ~s" expr))))

(defmacro bind-expr ((name &rest bindings) expr &body body)
  (multiple-value-bind (body-forms declarations) (alexandria:parse-body body)
    `(destructuring-bind (,name ,@bindings) ,expr
       ,@declarations
       (assert (eq ,name ',name))
       ,@body-forms)))

(defmacro def-typecheck-expr ((&key (expr 'expr) (env 'env))
                                 (identifier &rest args)
                              &body body)
  (multiple-value-bind (body-forms declarations docstring)
      (alexandria:parse-body body :documentation t)
    (let ((name-to-define (symbol-concatenate 'typecheck-
                                              identifier
                                              '-expr)))
      `(compiler-defun ,name-to-define (,expr ,env)
         (declare (ignorable ,env))
         ,docstring
         (bind-expr (,identifier ,@args) ,expr
           ,@declarations
           ,@body-forms)))))

(def-typecheck-expr (:env env)
    (funcall function &rest arguments)
    (let* ((function-type (typecheck function env))
           (argtypes (second function-type))
           (return-type (third function-type)))
      (assert (alexandria:length= arguments argtypes))
      (iter (for arg-expr in arguments)
            (for arg-type in argtypes)
            (assert (equal (typecheck arg-expr env)
                           arg-type)))
      return-type))

(def-typecheck-expr () (typed:force-type type body)
    (declare (ignore body))
  type)

(def-typecheck-expr (:env env)
    (check-type place type &optional type-string)
  (declare (ignore type-string))
  (unify (typecheck place env)
         type)
  'null)

(def-typecheck-expr (:env env)
    (let ((binding initial-expr)) &body body)
  (let* ((initial-expr-type (typecheck initial-expr env))
         (new-env (type-env-values-acons binding initial-expr-type env)))
      (typecheck-implicit-progn body new-env)))

(def-typecheck-expr (:env env)
    (typed:lambda args &body body)
  (typecheck-function args body env))

(compiler-defun typecheck-implicit-progn (body env)
  (iter (for body-form in body)
        (for return-value = (typecheck body-form env))
        (finally (return return-value))))

(compiler-defun typecheck-function (arglist body env)
  (multiple-value-bind (local-env argument-types)
      (prepend-function-type-env arglist env)
    (let ((return-type (typecheck-implicit-progn body local-env)))
      `(typed:function ,argument-types ,return-type))))
