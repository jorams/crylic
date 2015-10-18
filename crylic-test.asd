(in-package :asdf-user)
(defsystem :crylic-test
  :description "Tests for Crylic"
  :author "Joram Schrijver <i@joram.io>"
  :serial t
  :pathname "test"
  :depends-on (:crylic :1am)
  :components ((:file "framework")
               (:module "examples"
                :components ((:static-file "test.ini")
                             (:static-file "java.properties")
                             (:static-file "example.reg")
                             (:static-file "example.cf")
                             (:static-file "apache2.conf")
                             (:static-file "nginx.conf")
                             (:static-file "lighttpd.conf")
                             (:static-file "squid.conf")
                             (:static-file "Config.in.cache")
                             (:static-file "example.sh")
                             (:static-file "docker.docker")
                             (:static-file "test.tcsh")
                             (:static-file "test.ps1")
                             (:static-file "batchfile.bat")
                             (:static-file "boot-9.scm")
                             (:static-file "r6rs-comments.scm")
                             (:static-file "test.java")
                             (:static-file "Intro.java")
                             (:static-file "example.java")
                             (:static-file "badcase.java"))))
  :perform (test-op :after (op component)
                    (funcall (intern "RUN" :crylic-test/framework))))
