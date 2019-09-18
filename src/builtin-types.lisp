(in-package :static-types)

(defmacro def-builtin-type (name &key repr)
  (let ((repr (or repr name))
        (variable-name (symbol-concatenate '*
                                      name
                                      '*))
        (packaged-name (intern (symbol-name name)
                               (find-package :typed))))
    `(progn
       (deftype ,packaged-name ()
         ',repr)
       (defvar ,variable-name (make-primitive-type ',packaged-name)))))

(def-builtin-type fixnum)
(def-builtin-type bool :repr (member t nil))

(defparameter *identity*
  (let ((var (make-type-variable))) (make-forall-type (list var) (make-arrow-type var var)))
  "the type of the identity function, forall a. a -> a")
