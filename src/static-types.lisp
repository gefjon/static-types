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

(defmacro compiler-state (&body body)
  "evaluate body in a way that makes its side effects (e.g. definitions and bindings) available at compile-time."
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     ,@body))

(compiler-state
 (defstruct type-env values functions type-vars))

(compiler-state
 (defvar *global-type-env* (make-type-env :values nil
                                           :functions nil
                                           :type-vars nil)))

(defmacro compiler-defun (name lambda-list &body body)
  "like cl:defun, but ensures that the newly defined function is accessable at compile-time."
  `(compiler-state
     (defun ,name ,lambda-list ,@body)))

(compiler-defun reset-global-type-env ()
  "useful for debugging"
  (setf *global-type-env* (make-type-env :values nil
                                         :functions nil
                                         :type-vars nil)))

(compiler-defun list-car-p (list expected-car)
  "test that list is a list, and that its car is eq to expected-car"
  (and (consp list) (eq (car list) expected-car)))

(compiler-defun env-var-get (env binding)
  (cdr (assoc binding (type-env-values env))))

(compiler-defun update-env (env
                            &key
                            (values #'identity)
                            (functions #'identity)
                            (type-vars #'identity))
  "map update functions across a type-env, returning a new instance."
  (with-slots ((old-values values)
               (old-functions functions)
               (old-types type-vars))
      env
    (make-type-env :values (funcall values old-values)
                   :functions (funcall functions old-functions)
                   :type-vars (funcall type-vars old-types))))

(compiler-defun env-var-acons (var-binding type env)
  "return a new type-env which is identical to env, except that it has (var-binding . type) cons'd to the front of its values"
  (update-env env :values #'(lambda (old-values)
                              (acons var-binding type old-values))))

(compiler-defun env-function-acons (fun-binding type env)
  "return a new type-env which is like env, with (fun-binding . type) cons'd to the front of its functions. please make sure that type is a function type, like (function (fixnum fixnum) fixnum)."
  (update-env env :functions #'(lambda (old-functions)
                             (acons fun-binding type old-functions))))

(compiler-defun env-function-get (env binding)
  (cdr (assoc binding (type-env-functions env))))

(compiler-defun env-var-append (env list)
  (update-env env :values #'(lambda (old-values)
                          (append list old-values))))

(deftype bool ()
  '(member t nil))

(compiler-defun fixnump (fixnum)
  (typep fixnum 'fixnum))

(defmacro typecheck-cond (expr &rest clauses)
  (flet ((canonicalize-clause (clause)
           (alexandria:destructuring-case clause
             (((:repr :representational) type)
              `((typep ,expr ',type) ',type))
             ((:expr keyword &body forms)
              `((list-car-p ,expr ',keyword) ,@forms))
             ((:test predicate &body forms)
              `((funcall #',predicate ,expr) ,@forms))
             ((:t &body forms)
              `(t ,@forms)))))
    `(cond ,@(mapcar #'canonicalize-clause clauses))))

(compiler-defun typecheck (expr &optional (env *global-type-env*))
  (typecheck-cond expr
                  (:repr bool)
                  (:repr fixnum)
                  (:test symbolp (env-var-get env expr))
                  (:expr progn
                         (typecheck-implicit-progn (cdr expr) env))
                  (:expr let
                         (typecheck-let-expr expr env))
                  (:expr tylambda
                         (typecheck-tylambda-expr expr env))
                  (:expr force-type
                         (typecheck-force-type-expr expr))
                  (:expr funcall
                         (typecheck-funcall-expr expr env))
                  (:expr check-type
                         (typecheck-check-type-expr expr env))
                  (:expr function
                         (env-function-get env (second expr)))

                  ;; keep this clause last - it tries to treat the
                  ;; list as a funcall, just like common lisp does,
                  ;; but you have to make sure it's not a special form
                  ;; first.
                  (:test listp 
                         (typecheck-funcall-expr `(funcall #',(car expr)
                                                           ,@(cdr expr))
                                                 env))
                  
                  (:t (error "i don't know how to typecheck ~s" expr))))

(compiler-defun typecheck-funcall-expr (expr env)
  (bind-expr (funcall function &rest arguments) expr
    (let* ((function-type (typecheck function env))
           (argtypes (second function-type))
           (return-type (third function-type)))
      (assert (alexandria:length= arguments argtypes))
      (iter (for arg-expr in arguments)
            (for arg-type in argtypes)
            (assert (equal (typecheck arg-expr env)
                           arg-type)))
      return-type)))

(compiler-defun typecheck-force-type-expr (expr)
  (bind-expr (force-type type body) expr
    (declare (ignore body))
    type))

(compiler-defun typecheck-implicit-progn (body env)
  (let (return-value)
    (dolist (body-form body return-value)
      (setf return-value (typecheck body-form env)))))

(compiler-defun unify (type other-type)
  (unless (equal type other-type)
    (error "could not unify ~s with ~s" type other-type)))

(compiler-defun typecheck-check-type-expr (expr env)
  (bind-expr (check-type place type &optional type-string) expr
    (declare (ignore type-string))
    (unify (typecheck place env)
           type)))

(compiler-defun typecheck-let-expr (expr env)
  (bind-expr (let ((binding initial-expr)) &body body) expr
    (let* ((initial-expr-type (typecheck initial-expr env))
           (new-env (env-var-acons binding initial-expr-type env)))
      (typecheck-implicit-progn body new-env))))

(compiler-defun typecheck-tylambda-expr (expr env)
  (bind-expr (tylambda args &body body) expr
    (typecheck-function args body env)))

(compiler-defun typecheck-function (arglist body env)
  (multiple-value-bind (fnenv tyarglist) (tylambda-prepend-fnenv arglist env)
    (let ((return-type (typecheck-implicit-progn body fnenv)))
      `(function ,tyarglist ,return-type))))

(compiler-defun tylambda-prepend-fnenv (arglist enclosing-env)
  "returns multiple values: (values fnenv list-of-arg-types)"
  (multiple-value-bind (args types) (tylambda-extract-args arglist)
    ;; i'm pretty sure nconc won't modify the last argument
    (values (env-var-append enclosing-env (pairlis args types))
            ;; i don't remember why this function returned types. i
            ;; think it was useful as a side effect, and i don't
            ;; remember if it still is.
            types)))

(compiler-defun make-unbound-type ()
  `(unbound ,(alexandria:make-gensym :type)))

(compiler-defun tylambda-extract-args (arglist)
  (let (args types)
    (dolist (arg-form arglist
                      (values (nreverse args)
                              (nreverse types)))
      (flet ((finalize (arg &optional (type (make-unbound-type)))
               (push arg args)
               (push type types)))
        (alexandria:destructuring-ecase arg-form
          ((:the type arg)
           (finalize arg type))
          (((:a :an) arg)
           (finalize arg)))))))

(defmacro check-types-across-lists (places types)
  `(progn ,@(iter (for type in types)
                  (for place in places)
                  (collect `(check-type ,place ,type)))))

(compiler-defun declare-types-across-lists (places types)
  `(declare ,@(iter (for type in types)
                    (for place in places)
                    (collect `(type ,type ,place)))))

(compiler-state
  (defvar *known-caller*
    nil
    "whether the calling environment is statically typed"))

(compiler-defun typechecked-function-body (args types body)
  (multiple-value-bind (body-forms declarations docstring) body
    (flet ((known-caller-function-body ()
             (push (declare-types-across-lists args types)
                   declarations)
             `(,@declarations
               ,docstring
               ,@body-forms))
           (check-types-function-body ()
             `(,@declarations
               ,docstring
               (check-types-across-lists ,args ,types)
               ,@body-forms)))
      (if *known-caller*
          (known-caller-function-body)
          (check-types-function-body)))))



(defmacro tylambda (args &body body)
  (multiple-value-bind (args types) (tylambda-extract-args args)
    `(lambda ,args
       ,@(typechecked-function-body args types body))))

(defmacro known-caller (form)
  (let ((*known-caller* t))
    form))

(defmacro checked-caller (form)
  (let ((*known-caller* nil))
    form))

(defmacro tydefun (name lambda-list &body body)
  (multiple-value-bind (args types) (tylambda-extract-args lambda-list)
    (let* ((fn-type (typecheck-function lambda-list
                                            (alexandria:parse-body body :documentation t)
                                            *global-type-env*))
           
           (known-caller-name (intern (concatenate 'string
                                                   (symbol-name name)
                                                   "-KNOWN-CALLER")
                                      (symbol-package name)))

           (defun-checked-caller
              (checked-caller
               `(defun ,name ,args
                  ,@(typechecked-function-body args types body))))

           (defun-known-caller
              (known-caller
                `(defun ,known-caller-name ,args
                   ,@(typechecked-function-body args types body)))))
      
      (setf *global-type-env*
            (env-function-acons name fn-type *global-type-env*))
      
      `(progn ,defun-checked-caller
              ,defun-known-caller))))

(defmacro force-type (type expr)
  (declare (ignore type))
  "force the typechecker to interpret EXPR as if it had type TYPE. use
  with caution."
  expr)

(tydefun test-function ((:the fixnum x))
  (let ((+ (force-type (function (fixnum fixnum) fixnum) #'+)))
    (funcall + x x)))

(tydefun test-call-test-function ((:the fixnum x))
  (let ((y (test-function x)))
    (check-type y fixnum)))

;; (assert (eq (typecheck '(let ((x 3)) x))
;;             'fixnum))
;; (assert (equal (typecheck '(tylambda ((the fixnum x)) x))
;;             '(function (fixnum) fixnum)))
