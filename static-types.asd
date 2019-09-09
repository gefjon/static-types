(defsystem "static-types"
  :name "static-types"
  :version "0.0.0"
  :author "Arthur Goldman"
  :license "MIT"
  :depends-on (:iterate
                :alexandria
                :trivial-types)
  :components
  ((:file :package)
   (:module :src
            :depends-on (:package)
            :components ((:file :compiler-state)
                         (:file :macro-utils
                                :depends-on (:compiler-state))
                         (:file :type-env
                                :depends-on (:macro-utils))
                         (:file :unify
                                :depends-on (:type-env))
                         (:file :builtin-types
                                :depends-on (:type-env :macro-utils))
                         ;; (:file :arglist
                         ;;        :depends-on (:compiler-state :type-env))
                         ;; (:file :typecheck
                         ;;        :depends-on (:compiler-state
                         ;;                      :type-env
                         ;;                      :macro-utils
                         ;;                      :unify
                         ;;                      :arglist
                         ;;                      :builtin-types))
                         ;; (:file :typed
                         ;;        :depends-on (:arglist :typecheck))
                         ;; (:file :static-types
                         ;;        :depends-on (:compiler-state
                         ;;                      :type-env))
                         ;; (:file :load-typed
                         ;;        :depends-on (:compiler-state :typed))
                         ))))
