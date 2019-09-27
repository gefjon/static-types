(in-package :static-types)

(declaim (ftype (function (t t &key (:test (function (t t) t))))
                tree-contains-p))
(defun tree-contains-p (tree element &key (test #'eq))
  (cond ((funcall test tree element) t)
        ((consp tree) (or (tree-contains-p (car tree)
                                           element
                                           :test test)
                          (tree-contains-p (cdr tree)
                                           element
                                           :test test)))
        ('otherwise nil)))

(declaim (ftype (function (type-variable type-scheme) t)
                type-occurs-in-p))
(defun type-occurs-in-p (type-variable type-scheme)
    (and (typep type-scheme 'forall-type)
         (tree-contains-p (forall-type-body type-scheme) type-variable)))

(declaim (ftype (function (type-scheme type-scheme) boolean)
                can-substitute-for-type-variable-p))
(defun can-substitute-for-type-variable-p (might-be-a-type-variable
                                           other-type)
  (and (typep might-be-a-type-variable 'type-variable)
       (not (type-occurs-in-p might-be-a-type-variable other-type))))

(declaim (ftype (function (arrow-type arrow-type) type-map)
                unify-arrows))
(defun unify-arrows (lhs rhs)
  (let* ((input-substitution (unify (arrow-type-input lhs)
                                    (arrow-type-input rhs)))
         (left-output (substitute-type-map (arrow-type-output lhs)
                                           input-substitution))
         (right-output (substitute-type-map (arrow-type-output rhs)
                                            input-substitution))
         (output-substitution (unify left-output right-output)))
    (concatenate 'list input-substitution output-substitution)))

(declaim (ftype (function (type-scheme type-scheme) type-map)
                unify))
(defun unify (lhs rhs)
  (cond
    ;; if lhs and rhs are the same type, unification is a no-op
    ((eq lhs rhs) ())
    ;; if lhs is an unbound type variable, we can unify by binding
    ;; it to rhs
    ((can-substitute-for-type-variable-p lhs rhs)
     (type-map-extend () lhs rhs))
    ;; we can do the same if rhs is an unbound type variable
    ((can-substitute-for-type-variable-p rhs lhs)
     (type-map-extend () rhs lhs))

    ;; if both are arrows, recurse on their inputs and outputs
    ((and (typep lhs 'arrow-type)
          (typep rhs 'arrow-type))
     (unify-arrows lhs rhs))

    ((error "failed to unify ~a with ~a" lhs rhs))))
