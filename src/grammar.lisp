(in-package :static-types)

(defclass expression () ())

(defmacro defexpr (expr-name slots)
  "e.g. (DEFEXPR let ((binding symbol) (initializer expression) (body expression)))"
  `(defstruct ,expr-name ,slots
              :repr-type list))

(defexpr var-expression
    ((name symbol)))

(defexpr apply-expression
    ((arrow expression)
     (argument expression)))

(defexpr lambda-expression
    ((arguments (trivial-types:proper-list var-expression))
     (body expression)))

(defexpr literal-fixnum
    ((value fixnum)))

(defexpr literal-boolean
    ((value boolean)))

(defexpr if-expression
    ((predicate expression)
     (then-clause expression)
     (else-clause expression)))
