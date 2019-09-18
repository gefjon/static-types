(in-package :static-types)

(defgeneric type-scheme-substitute (within search-for replace-with))

(defmethod type-scheme-substitute :around ((this type-scheme) search-for replace-with)
  "the most important method - substitutes if (EQ TYPE-SCHEME SEARCH-FOR)"
  (cond ((eq this search-for) replace-with)
        ((next-method-p) (call-next-method this search-for replace-with))
        ('otherwise this)))

(defmethod type-scheme-substitute ((this forall-type) search-for replace-with)
  (with-slots (variable body) this
    (if (eq variable search-for)
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
              (make-forall-type variable substituted-body))))))

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

(declaim (ftype (function (forall-type) type-scheme)
                forall-type-instantiate))
(defun forall-type-instantiate (forall-type)
  "unwrap a FORALL-TYPE into its body, replacing its argument with a new, unbound TYPE-VARIABLE"
  (type-scheme-substitute (forall-type-body forall-type)
                          (forall-type-variable forall-type)
                          (make-type-variable)))
