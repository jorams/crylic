(asdf:defsystem :crylic
  :description "Syntax highlighting library for Common Lisp"
  :author "Joram Schrijver <i@joram.io>"
  :license "MIT"
  :depends-on (#:cl-ppcre #:split-sequence #:closer-mop)
  :serial t
  :pathname "src"
  :components ((:file "tokens")
               (:file "lexer")
               (:file "filter")
               (:file "renderer")
               (:file "cl-ppcre-extensions")
               (:file "regex-lexer")
               (:module "lexers"
                :components ((:file "ini")
                             (:file "properties")
                             (:file "regedit")
                             (:file "cfengine3")
                             (:file "apache")
                             (:file "nginx")
                             (:file "lighttpd")
                             (:file "squid")
                             (:file "kconfig")
                             (:file "bash")
                             (:file "docker")
                             (:file "tcsh")
                             (:file "powershell")
                             (:file "batch")
                             (:file "scheme")
                             (:file "java")
                             (:file "gosu")
                             (:file "groovy")
                             (:file "ioke")
                             (:file "clojure")))
               (:module "filters"
                :components ((:file "merge-consecutive")
                             (:file "split-multiline")))
               (:module "renderers"
                :components ((:file "html"))))
  :in-order-to ((test-op (test-op :crylic-test))))
