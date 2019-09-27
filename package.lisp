(defpackage :static-types
  (:use :cl :iterate :gefjon-utils)
  (:shadowing-import-from :gefjon-utils
   :defstruct
   :defclass))
(defpackage :typed
  (:export :bool
           :int
           :function
           :defun
           :lambda
           :force-type)
  (:import-from :cl
                :let
                :funcall
                :progn))
