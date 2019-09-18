(in-package :static-types)

(defgeneric type-scheme-free-variables (type-scheme))

(defmethod type-scheme-free-variables ((this type-variable))
  (list this))

(defmethod type-scheme-free-variables ((this arrow-type))
  (with-slots (input output) this
    (let ((input-free-vars (type-scheme-free-variables input))
          (output-free-vars (type-scheme-free-variables output)))
      (if (equal input-free-vars output-free-vars)
          input-free-vars
          (concatenate 'list input-free-vars output-free-vars)))))

(defmethod type-scheme-free-variables ((this forall-type))
  (with-slots (arguments body) this
    (set-difference arguments (type-scheme-free-variables body))))

(defmethod type-scheme-free-variables ((this primitive-type))
  (declare ;; we can't (IGNORE THIS), because it's used for method dispatch
   (ignorable this))
  ())

(declaim (ftype (function (type-scheme type-map) type-scheme)
                type-scheme-generalize))
(defun type-scheme-generalize (type-scheme type-map)
  (flet ((variable-bound-p (type-variable)
           (type-variable-bound-p type-variable type-map)))
    (let ((free-vars (remove-if #'variable-bound-p
                                (type-scheme-free-variables type-scheme))))
      (if free-vars (make-forall-type free-vars type-scheme)
          type-scheme))))
