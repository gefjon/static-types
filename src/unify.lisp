(in-package :static-types)

(declaim (ftype (function (type-scheme type-scheme) boolean)
                same-primitive-type-p))
(defun same-primitive-type-p (lhs rhs)
  (and (typep lhs 'primitive-type)
       (typep rhs 'primitive-type)
       (eq (primitive-type-name lhs)
           (primitive-type-name rhs))))

(declaim (ftype (function (type-scheme type-map) boolean)
                unbound-type-variable-p))
(defun unbound-type-variable-p (type-scheme type-map)
  (check-anaphoric-types type-scheme type-map)
  (and (typep type-scheme 'type-variable)
       (not (type-variable-bound-p type-scheme type-map))))

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

(declaim (ftype (function (type-scheme type-scheme type-map) boolean)
                can-substitute-for-type-variable-p))
(defun can-substitute-for-type-variable-p (might-be-a-type-variable
                                           other-type
                                           type-map)
  (and (unbound-type-variable-p might-be-a-type-variable type-map)
       (not (type-occurs-in-p might-be-a-type-variable other-type))))

(declaim (ftype (function (type-map type-scheme type-scheme) type-map)
                unify))
(defun unify (type-map lhs rhs)
  (cond
    ;; if lhs and rhs are the same type, unification is a no-op
    ((eq lhs rhs) type-map)
    ((same-primitive-type-p lhs rhs) type-map)
    ;; if lhs is an unbound type variable, we can unify by binding
    ;; it to rhs
    ((unbound-type-variable-p lhs type-map)
     (type-map-extend type-map lhs rhs))
    ;; we can do the same if rhs is an unbound type variable
    ((unbound-type-variable-p rhs type-map)
     (type-map-extend type-map rhs lhs))

    ((error "failed to unify ~a with ~a in ~a" lhs rhs type-map))))
