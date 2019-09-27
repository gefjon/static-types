(in-package :static-types)

(defclass var
    ((name symbol)))

(deftype type-map ()
  "maps TYPE-VARs to TYPE-SCHEMEs"
  '(trivial-types:association-list var type-scheme))

(declaim (ftype (function (type-map var type-scheme) type-map)
                type-map-extend))
(defun type-map-extend (type-map var type-scheme)
  "extend TYPE-MAP by associating VAR with TYPE-SCHEME"
  (acons var type-scheme type-map))

(declaim (ftype (function (type-map var) (or null type-scheme))
                type-map-lookup))
(defun type-map-lookup (type-map var)
  (cdr (assoc var type-map :key #'var-name)))

(declaim (ftype (function (type-map var) boolean)
                type-map-bound-p))
(defun type-map-bound-p (type-map var)
  "T if TYPE-VAR is bound in TYPE-MAP

O(n) wrt the size of TYPE-MAP"
  (not (not (type-map-lookup type-map var))))
