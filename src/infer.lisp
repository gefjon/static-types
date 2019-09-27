(in-package :static-types)

(defgeneric infer (expression type-map)
  (:documentation "returns (values TYPE-SCHEME SUBSTITUTION), the inferred type and the partial unifing solution."))

(defmethod infer ((expression var-expression) type-map)
  (or (type-map-lookup type-map expression)
      (error "could infer type for unbound variable ~a in ~a"
             expression
             type-map)))
