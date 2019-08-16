(in-package :static-types)

(defmacro check-anaphoric-type (place-and-type)
  "PLACE-AND-TYPE must be a symbol which names both a place and a type"
  `(check-type ,place-and-type ,place-and-type))

(compiler-defun coerce-to-string (object)
  (typecase object
    (symbol (symbol-name object))
    (string object)
    (t (coerce object 'string))))

(compiler-defun symbol-concatenate (&rest symbols-or-strings)
  (intern (apply #'concatenate
                 (cons 'string (mapcar #'coerce-to-string
                                       symbols-or-strings)))))

