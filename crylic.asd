(in-package :asdf-user)
(defsystem :crylic
  :description "Syntax highlighting library for Common Lisp"
  :author "Joram Schrijver <i@joram.io>"
  :license "MIT"
  :depends-on (#:cl-ppcre #:split-sequence #:closer-mop)
  :pathname "src"
  :components ((:file "tokens")
               (:file "lexer")
               (:file "filter")
               (:file "renderer")
               (:file "regex-lexer")
               (:module "lexers"
                :components ((:file "ini")
                             (:file "properties")
                             (:file "regedit")))
               (:module "filters"
                :components ((:file "merge-consecutive")
                             (:file "split-multiline")))
               (:module "renderers"
                :components ((:file "html"))))
  :in-order-to ((test-op (test-op :crylic-test))))
