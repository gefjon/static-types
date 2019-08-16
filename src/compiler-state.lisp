(in-package :static-types)

(defmacro compiler-state (&body body)
  "evaluate body in a way that makes its side effects (e.g. definitions and bindings) available at compile-time."
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     ,@body))

(defmacro compiler-defun (name lambda-list &body body)
  "like cl:defun, but ensures that the newly defined function is accessable at compile-time."
  `(compiler-state
     (defun ,name ,lambda-list ,@body)))
