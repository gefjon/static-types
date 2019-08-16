(in-package :static-types)

(defmacro check-types-across-lists (places types)
  `(progn ,@(iter (for type in types)
                  (for place in places)
                  (collect `(check-type ,place ,type)))))

(defmacro declare-types-across-lists (places types)
  `(declare ,@(iter (for type in types)
                    (for place in places)
                    (collect `(type ,type ,place)))))

(compiler-defun typechecked-function-body (args types body)
  (multiple-value-bind (body-forms declarations docstring) body
    `(,@declarations
               ,docstring
               (check-types-across-lists ,args ,types)
               ,@body-forms)))

(defmacro typed:lambda (args &body body)
  (multiple-value-bind (args types) (extract-typed-arglist args)
    `(lambda ,args
       ,@(typechecked-function-body args types body))))

(defmacro typed:defun (name lambda-list &body body)
  (multiple-value-bind (args types) (extract-typed-arglist lambda-list)
    (let ((fn-type (typecheck-function lambda-list
                                       (alexandria:parse-body body :documentation t)
                                       *global-type-env*)))

      (setf *global-type-env*
            (type-env-functions-acons name fn-type *global-type-env*))

      `(defun ,name ,args
         ,@(typechecked-function-body args types body)))))

(defmacro typed:force-type (type expr)
  (declare (ignore type))
  "force the typechecker to interpret EXPR as if it had type TYPE. use
  with caution."
  expr)
