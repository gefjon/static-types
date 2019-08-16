(defpackage :static-types
  (:use :cl
        :iterate))
(defpackage :typed
  (:export :bool
           :int
           :function
           :defun
           :lambda
           :force-type))
