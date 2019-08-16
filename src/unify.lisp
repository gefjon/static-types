(in-package :static-types)

(compiler-defun unify (type other-type)
  "the laziest unification algorithm you've ever seen"
  (unless (equal type other-type)
    (error "could not unify ~s with ~s" type other-type)))

(compiler-defun make-unbound-type ()
  `(:unbound ,(alexandria:make-gensym :type)))

