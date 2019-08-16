(in-package :static-types)

(compiler-defun extract-typed-arglist (arglist)
  "returns (VALUES LIST-OF-ARG-NAMES LIST-OF-TYPES)"
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

(compiler-defun prepend-function-type-env (arglist enclosing-env)
  "returns multiple values: (values fnenv list-of-arg-types)"
  (multiple-value-bind (args types) (extract-typed-arglist arglist)
    (values (type-env-values-append enclosing-env (pairlis args types))
            types)))
