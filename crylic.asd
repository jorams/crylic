(asdf:defsystem :crylic
  :description "Syntax highlighting library for Common Lisp"
  :author "Joram Schrijver <i@joram.io>"
  :license "MIT"
  :depends-on (#:cl-ppcre #:closer-mop)
  :pathname "src"
  :components ((:file "tokens")
               (:file "lexer")
               (:file "filter")
               (:file "renderer")
               (:file "regex-lexer")
               (:module "lexers"
                :components ((:file "ini")))
               (:module "renderers"
                :components ((:file "html")))))
