(in-package :static-types)

(deftype typed:bool ()
  '(member :t :f))

(deftype typed:int ()
  'fixnum)

(deftype typed:function (arglist return-type)
  (declare (ignore arglist return-type))
  'function)
