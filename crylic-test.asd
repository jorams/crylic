(in-package :asdf-user)
(defsystem :crylic-test
  :description "Tests for Crylic"
  :author "Joram Schrijver <i@joram.io>"
  :serial t
  :pathname "test"
  :depends-on (:crylic :1am)
  :components ((:file "framework")
               (:module "examples"
                :components ((:static-file "test.ini"))))
  :perform (test-op :after (op component)
                    (funcall (intern "RUN" :crylic-test/framework))))
