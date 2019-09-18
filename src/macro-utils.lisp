(in-package :static-types)

(defmacro check-anaphoric-type (place-and-type)
  "PLACE-AND-TYPE must be a symbol which names both a place and a type"
  `(check-type ,place-and-type ,place-and-type))

(defmacro check-anaphoric-types (&rest places-and-types)
  "PLACES-AND-TYPES must each be a symbol which names both a place and a type"
  `(progn
     ,@(mapcar #'(lambda (place-or-type)
                   `(check-anaphoric-type ,place-or-type))
               places-and-types)))

(compiler-defun coerce-to-string (object)
  (typecase object
    (symbol (symbol-name object))
    (string object)
    (t (coerce object 'string))))

(compiler-defun symbol-concatenate (&rest symbols-or-strings)
  (intern (apply #'concatenate
                 (cons 'string (mapcar #'coerce-to-string
                                       symbols-or-strings)))))

(compiler-defun make-keyword (symbol-or-string)
  (intern (coerce-to-string symbol-or-string)
          (find-package :keyword)))
