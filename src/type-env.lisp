(in-package :static-types)

(compiler-state
  (defstruct type-env
    (values nil :type trivial-types:association-list)
    (functions nil :type trivial-types:association-list)
    (type-vars nil :type trivial-types:association-list)))

(compiler-defun update-type-env (env
                                 &key
                                 (values #'identity)
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

(compiler-defun symbol-to-keyword (symbol)
  (check-anaphoric-type symbol)
  (intern (symbol-name symbol)
          :keyword))

(compiler-defun type-env-slot-accessor-name (slot-name accessor)
  (check-type slot-name symbol)
  (check-type accessor symbol)
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

(defmacro derive-slot-accessor
    (slot-name accessor
     &key
       (arglist '(type-env arg)) (inner-arglist '(alist arg)))
  `(compiler-defun ,(type-env-slot-accessor-name slot-name accessor) ,arglist
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

;; (for canonical-mapper
;;          = (canonicalize-mapper-function-clause mapper-clause))

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
                             :inner-arglist (item alist))))

(compiler-defun empty-type-env ()
  "i like this as an explicit alias, even though the MAKE-TYPE-ENV defined by DEFSTRUCT does the same thing when called with no arguments."
  (make-type-env))

(compiler-state
  (defvar *global-type-env* (empty-type-env)))

(compiler-defun reset-global-type-env ()
  "useful for debugging"
  (setf *global-type-env* (empty-type-env)))
