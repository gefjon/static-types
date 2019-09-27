(in-package :static-types)

(deftype substitution ()
  'type-map)

(defgeneric type-scheme-substitute (within search-for replace-with))

(defmethod type-scheme-substitute :around ((this type-scheme) search-for replace-with)
  "the most important method - substitutes if (EQ TYPE-SCHEME SEARCH-FOR)"
  (cond ((eq this search-for) replace-with)
        ((next-method-p) (call-next-method this search-for replace-with))
        ('otherwise this)))

(defmethod type-scheme-substitute ((this forall-type) search-for replace-with)
  (with-slots (arguments body) this
    (if (member search-for arguments)
        ;; if THIS closes over the variable SEARCH-FOR, we shouldn't
        ;; actually substitute, to respect the inner scope
        this
        ;; otherwise, try the substitution on BODY
        (let ((substituted-body (type-scheme-substitute body
                                                        search-for
                                                        replace-with)))
          (if (eq body substituted-body)
              ;; if no substitution, return THIS to preserve EQ and
              ;; avoid an allocation
              this
              ;; otherwise, it's a new type
              (make-forall-type arguments substituted-body))))))

(defmethod type-scheme-substitute ((this arrow-type) search-for replace-with)
  (with-slots (input output) this
    (let* ((substituted-input (type-scheme-substitute input
                                                     search-for
                                                     replace-with))
           (substituted-output (type-scheme-substitute output
                                                      search-for
                                                      replace-with))
           (input-changed (not (eq substituted-input input)))
           (output-changed (not (eq substituted-output output))))
      (if (or input-changed output-changed)
          (make-arrow-type substituted-input
                           substituted-output)
          ;; if no change, return THIS to preserve EQ and
          ;; avoid consing an ARROW-TYPE
          this))))

(defmacro do-not-substitute (type-class-name)
  "declares that searching and substituting the body of TYPE-CLASS-NAME is a no-op"
  `(defmethod type-scheme-substitute ((this ,type-class-name) search-for replace-with)
    (declare  ;; i don't think i'm allowed to DECLARE these IGNORE,
     ;; because they might be used for method discrimination. or
     ;; something. :shrug:
     (ignorable search-for replace-with))
    this))

(do-not-substitute type-variable)
(do-not-substitute primitive-type)

(declaim (ftype (function (type-scheme substitution) type-scheme)
                substitute-many))
(defun substitute-many (within-this-type-scheme substitution)
  "perform each of the substitutions in SUBSTITUTION in sequence on WITHIN-THIS-TYPE-SCHEME"
  (iter (with type-scheme = within-this-type-scheme)
        (for (search-for . replace-with) in substitution)
        (setf type-scheme (type-scheme-substitute type-scheme
                                                  search-for
                                                  replace-with))
        (finally (return type-scheme))))

(declaim (ftype (function (type-scheme (trivial-types:proper-list type-variable))
                          type-scheme)
                substitute-fresh-variables))
(defun substitute-fresh-variables (type-scheme vars-to-replace)
  "substitute all of the TYPE-VARIABLEs in VARS-TO-REPLACE with fresh unbound TYPE-VARIABLEs"
  (flet ((type-var-to-alist-cell (type-var)
           (cons type-var (new-type-variable))))
    (substitute-many type-scheme
                     (mapcar #'type-var-to-alist-cell vars-to-replace))))

(declaim (ftype (function (forall-type) type-scheme)
                forall-type-instantiate))
(defun forall-type-instantiate (forall-type)
  "unwrap a FORALL-TYPE into its body, replacing its arguments with new, unbound TYPE-VARIABLEs"
  (substitute-fresh-variables (forall-type-body forall-type)
                              (forall-type-arguments forall-type)))
