(cl:defpackage :hindley-milner.asd
  (:use :cl :asdf))

(defsystem "static-types"
  :name "static-types"
  :version "0.0.0"
  :author "Arthur Goldman"
  :license "MIT"
  :pathname "src/"
  :depends-on (:iterate
                :alexandria)
  :components
  ((:file "static-types")))
