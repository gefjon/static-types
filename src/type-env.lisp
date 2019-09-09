(in-package :static-types)

(defstruct forall-type
  (variable (error "required field uninit") :type symbol)
  body)

(defstruct primitive-type
  (name (error "required field uninit") :type symbol))

(defstruct type-variable
  (name (gensym "TYPE-VARIABLE-") :type symbol))

(deftype type-scheme ()
  '(or forall-type primitive-type type-variable))

(deftype type-map ()
  '(trivial-types:association-list type-variable type-scheme))

(declaim (ftype (function (type-map type-variable type-scheme) type-map)
                type-map-extend))
(defun type-map-extend (type-map type-variable type-scheme)
  (acons type-variable type-scheme type-map))

(declaim (ftype (function (type-variable type-map) boolean)
                type-variable-bound-p))
(defun type-variable-bound-p (type-variable type-map)
  (not (not (assoc type-variable type-map))))

(defstruct type-env
  (values nil :type type-map)
  (functions nil :type type-map)
  (type-vars nil :type type-map))

(declaim (ftype (function (type-env &key
                                    (:values (function (type-map) type-map))
                                    (:functions (function (type-map) type-map))
                                    (:type-vars (function (type-map) type-map)))
                          type-env)
                update-type-env))
(defun update-type-env (env &key (values #'identity)
                                 (functions #'identity)
                                 (type-vars #'identity))
  "map update functions across a type-env, returning a new instance."
  (with-slots ((old-values values)
               (old-functions functions)
               (old-types type-vars))
      env
    (make-type-env :values (funcall values old-values)
                   :functions (funcall functions old-functions)
                   :type-vars (funcall type-vars old-types))))

(declaim (ftype (function (symbol) symbol)
                symbol-to-keyword))
(compiler-defun symbol-to-keyword (symbol)
  (intern (symbol-name symbol)
          :keyword))

(declaim (ftype (function (symbol symbol) symbol)
                type-env-slot-accessor-name))
(compiler-defun type-env-slot-accessor-name (slot-name accessor)
  "computes the name of a pseudo-accessor function, like type-env-values-acons"
  (symbol-concatenate 'type-env-
                      slot-name
                      '-
                      accessor))

(defmacro derive-slot-mapper
    (slot-name mapper
     &key
       (arglist '(type-env arg)) (inner-arglist '(alist arg)))
  "ARGLIST must bind the symbol TYPE-ENV, and INNER-ARGLIST should consume the symbol ALIST"
  `(compiler-defun ,(type-env-slot-accessor-name slot-name mapper) ,arglist
     (update-type-env type-env
                      ,(symbol-to-keyword slot-name)
                      #'(lambda (alist) (,mapper ,@inner-arglist)))))

(defmacro derive-slot-accessor (slot-name accessor &key (arglist '(type-env arg))
                                                     (inner-arglist '(alist arg))
                                                     accessor-name-fragment)
  `(defun
       ,(type-env-slot-accessor-name slot-name (or accessor-name-fragment accessor))
     ,arglist
     (let ((alist (slot-value type-env ',slot-name)))
       (,accessor ,@inner-arglist))))

(defmacro map-form-across-2d ((outer-var outer-list)
                              (inner-var inner-list)
                              form)
  `(iter
     (for ,outer-var in ,outer-list)
     (nconcing
      (iter (for ,inner-var in ,inner-list)
            (collect ,form)))))

(compiler-defun canonicalize-accessor-clause (clause)
  (etypecase clause
    (symbol (list clause))
    (cons clause)))

(defmacro derive-many-accessors (&key
                                   slots
                                   mapper-functions
                                   accessor-functions)
  `(compiler-state
     ,@(map-form-across-2d (accessor-clause accessor-functions)
           (slot-name slots)
         `(derive-slot-accessor ,slot-name
                                ,@(canonicalize-accessor-clause accessor-clause)))
     ,@(map-form-across-2d (mapper-clause mapper-functions)
           (slot-name slots)
         `(derive-slot-mapper ,slot-name
                              ,@(canonicalize-accessor-clause mapper-clause)))))

(derive-many-accessors :slots
                     (values functions type-vars)

                     :mapper-functions
                     ((acons :arglist (key datum type-env)
                             :inner-arglist (key datum alist))
                      append)

                     :accessor-functions
                     ((assoc :arglist (item type-env)
                             :inner-arglist (item alist))
                      (alist-lookup :arglist (type-env identifier)
                                    :inner-arglist (alist identifier)
                                    :accessor-name-fragment lookup)))

(defun alist-lookup (alist identifier)
  (cdr (or (assoc identifier alist)
           (error "~s not found in alist" identifier))))

(defun empty-type-env ()
  "i like this as an explicit alias, even though the MAKE-TYPE-ENV defined by DEFSTRUCT does the same thing when called with no arguments."
  (make-type-env))

(defvar *global-type-env* (empty-type-env))

(defun reset-global-type-env ()
  "useful for debugging"
  (setf *global-type-env* (empty-type-env)))
