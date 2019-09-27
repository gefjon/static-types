(defsystem "static-types"
  :name "static-types"
  :version "0.0.0"
  :author "Arthur Goldman"
  :license "MIT"
  :depends-on (:iterate
                :alexandria
                :trivial-types
                :gefjon-utils)
  :components
  ((:file :package)
   (:module :src
            :depends-on (:package)
    :components ((:file :type-struct)
                 (:file :substitute
                  :depends-on (:type-struct))
                 (:file :type-map
                  :depends-on (:type-struct))
                 (:file :generalize
                  :depends-on (:type-struct
                               :type-map))
                 (:file :unify
                  :depends-on (:type-struct
                               :type-map
                               :substitute))
                 (:file :builtin-types)
                 (:file :grammar)
                 (:file :infer
                  :depends-on (:builtin-types
                               :unify
                               :type-struct
                               :type-map
                               :substitute
                               :grammar))))))
