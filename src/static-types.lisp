(defpackage :static-types
  (:use :cl
        :iterate))
(in-package :static-types)

(defmacro bind-expr ((name &rest bindings) expr &body body)
  (multiple-value-bind (body-forms declarations) (alexandria:parse-body body)
    `(destructuring-bind (,name ,@bindings) ,expr
       ,@declarations
       (assert (eq ,name ',name))
       ,@body-forms)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defstruct type-env values functions)
  
  (defvar *global-type-env* (make-type-env :values nil :functions nil))
  
  (defun reset-global-type-env ()
    "useful for debugging"
    (setf *global-type-env* (make-type-env :values nil :functions nil)))
  
  ;;; test that list is a list, and that its car is eq to expected-car
  (defun list-car-p (list expected-car)
    (and (consp list) (eq (car list) expected-car)))

  (defun env-var-get (env binding)
    (cdr (assoc binding (type-env-values env))))

  (defun env-var-acons (var-binding type env)
    (with-slots (values functions) env
      (make-type-env :values (acons var-binding type values)
                     :functions functions)))

  (defun env-function-acons (fun-binding type env)
    (with-slots (values functions) env
      (make-type-env :values values
                     :functions (acons fun-binding type functions))))

  (defun env-function-get (env binding)
    (cdr (assoc binding (type-env-functions env))))

  (defun boolp (bool)
    (or (eq bool 'true)
        (eq bool 'false)))
  
  ;; env is an alist whose cars are variable bindings and whose cdrs
  ;; are types
  (defun typecheck (expr &optional (env *global-type-env*))
    (cond ((boolp expr) 'boolean)
                  ((symbolp expr) (env-var-get env expr))
                  ((typep expr 'fixnum) 'fixnum)
                  ((list-car-p expr 'progn) (typecheck-implicit-progn (cdr expr) env))
                  ((list-car-p expr 'let) (typecheck-let-expr expr env))
                  ((list-car-p expr 'tylambda) (typecheck-tylambda-expr expr env))
                  ((list-car-p expr 'force-type) (typecheck-force-type-expr expr))
                  ((list-car-p expr 'funcall) (typecheck-funcall-expr expr env))
                  (t (error "i don't know how to typecheck ~s" expr))))

  (defun typecheck-funcall-expr (expr env)
    (bind-expr (funcall function &rest arguments) expr
        (let* ((function-type (typecheck function env))
               (argtypes (cadr function-type))
               (return-type (caddr function-type)))
          (assert (alexandria:length= arguments argtypes))
          (iter (for arg-expr in arguments)
                (for arg-type in argtypes)
                (assert (equal (typecheck arg-expr env)
                               arg-type)))
          return-type)))
  (defun typecheck-force-type-expr (expr)
    (bind-expr (force-type type body) expr
        (declare (ignore body))
      type))

  (defun typecheck-implicit-progn (body env)
    (let (return-value)
      (dolist (body-form body return-value)
        (setf return-value (typecheck body-form env)))))

  (defun typecheck-let-expr (expr env)
    (bind-expr (let ((binding initial-expr)) &body body) expr
        (let* ((initial-expr-type (typecheck initial-expr env))
               (new-env (env-var-acons binding initial-expr-type env)))
          (typecheck-implicit-progn body new-env))))

  (defun typecheck-tylambda-expr (expr env)
    (bind-expr (tylambda args &body body) expr
        (typecheck-function args body env)))

  (defun typecheck-function (arglist body env)
    (multiple-value-bind (fnenv tyarglist) (tylambda-prepend-fnenv arglist env)
      (let ((return-type (typecheck-implicit-progn body fnenv)))
        `(function ,tyarglist ,return-type))))

  (defun tylambda-prepend-fnenv (arglist enclosing-env)
    "returns multiple values: (values fnenv list-of-arg-types)"
    (let ((fnenv enclosing-env)
          type-arglist)
      (dolist (arg arglist)
        (bind-expr (the type binding) arg
            (setf fnenv (env-var-acons binding type fnenv)
                  type-arglist (cons type type-arglist))))
      (values fnenv (nreverse type-arglist))))

  (defun tylambda-extract-args (arglist)
    (let (args types)
      (dolist (the-form arglist (values (nreverse args) (nreverse types)))
        (bind-expr (the type arg) the-form
            (setf args (cons arg args)
                  types (cons type types))))))

  (defun typechecked-function-body (args types body)
  (multiple-value-bind (body-forms declarations docstring) body
    `(,@declarations
      ,docstring
      (check-types-across-lists ,args ,types)
      ,@body-forms))))

(defmacro check-types-across-lists (places types)
  `(progn ,@(iter (for type in types)
                  (for place in places)
                  (collect `(check-type ,place ,type)))))

(defmacro tylambda (args &body body)
  (multiple-value-bind (args types) (tylambda-extract-args args)
    `(lambda ,args
       ,@(typechecked-function-body args types body))))

(defmacro tydefun (name lambda-list &body body)
  (let ((return-type (typecheck-function lambda-list body *global-type-env*)))
    (multiple-value-bind (args types) (tylambda-extract-args lambda-list)
      (let ((fn-type `(function ,types ,return-type)))
        (setf *global-type-env*
              (env-function-acons name fn-type *global-type-env*)))
      `(defun ,name ,args
         ,@(typechecked-function-body args types body)))))

(defmacro force-type (type expr)
  (declare (ignore type))
  "force the typechecker to interpret EXPR as if it had type TYPE. use
  with caution."
  expr)

(tydefun test-function ((the fixnum x))
  (let ((+ (force-type (function (fixnum fixnum) fixnum) #'+)))
   (funcall + x x)))

;; (assert (eq (typecheck '(let ((x 3)) x))
;;             'fixnum))
;; (assert (equal (typecheck '(tylambda ((the fixnum x)) x))
;;             '(function (fixnum) fixnum)))
