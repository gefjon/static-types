;;;; this file doesn't even get loaded by asdf right now!
;;; i use it as an interactive scratch buffer with slime

(in-package :static-types)

(compiler-defun fixnump (fixnum)
  (typep fixnum 'fixnum))

(typed:defun test-function ((:the fixnum x))
  (let ((+ (typed:force-type (function (fixnum fixnum) fixnum) #'+)))
    (funcall + x x)))

(typed:defun test-call-test-function ((:the fixnum x))
  (let ((y (test-function x)))
    (check-type y fixnum)))

(assert (eq (typecheck '(let ((x 3)) x))
            'typed:int))
(assert (equal (typecheck '(tylambda ((the fixnum x)) x))
            '(typed:function (fixnum) fixnum)))
