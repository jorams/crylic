(defpackage :crylic-test/framework
  (:use :cl)
  (:import-from :1am
                #:is
                #:signals)
  (:export #:define-test-framework
           #:is
           #:signals

           ;; Default testing framework
           #:test
           #:run))
(in-package :crylic-test/framework)

(defmacro define-test-framework (tests-variable
                                 test-macro
                                 run-function)
  "Define a variable to hold a list of tests, a macro to define tests and a
function to run the tests."
  `(progn
     (defvar ,tests-variable ())
     (defmacro ,test-macro (name &body body)
       `(let ((1am:*tests* ()))
          (1am:test ,name ,@body)
          (dolist (test 1am:*tests*)
            (pushnew test ,',tests-variable))))
     (defun ,run-function ()
       (1am:run ,tests-variable))))

(define-test-framework *tests* test run)
