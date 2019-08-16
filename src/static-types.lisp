;;;; this file doesn't even get loaded by asdf right now!
;;; i use it as an interactive scratch buffer with slime

(in-package :static-types)

(typed:defun test-function ((:the fixnum x))
  (let ((+ (typed:force-type (typed:function (fixnum fixnum) fixnum) #'+)))
    (funcall + x x)))

(typed:defun test-call-test-function ((:the fixnum x))
  (let ((y (test-function x)))
    (check-type y fixnum)))

(assert (eq (typecheck '(let ((x 3)) x))
            'typed:int))
(assert (equal (typecheck '(typed:lambda ((:the typed:int x)) x))
            '(typed:function (typed:int) typed:int)))
