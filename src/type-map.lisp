(in-package :static-types)

(deftype type-map ()
  "maps TYPE-VARIABLEs to TYPE-SCHEMEs"
  '(trivial-types:association-list type-variable type-scheme))

(declaim (ftype (function (type-map type-variable type-scheme) type-map)
                type-map-extend))
(defun type-map-extend (type-map type-variable type-scheme)
  "extend TYPE-MAP by associating TYPE-VARIABLE with TYPE-SCHEME"
  (acons type-variable type-scheme type-map))

(declaim (ftype (function (type-variable type-map) boolean)
                type-variable-bound-p))
(defun type-variable-bound-p (type-variable type-map)
  "T if TYPE-VARIABLE is bound in TYPE-MAP

O(n) wrt the size of TYPE-MAP"
  (not (not (assoc type-variable type-map))))
