(asdf:defsystem :crylic
  :description "Syntax highlighting library for Common Lisp"
  :author "Joram Schrijver <i@joram.io>"
  :license "MIT"
  :depends-on (#:cl-ppcre)
  :pathname "src"
  :components ((:file "tokens")
               (:file "lexer")
               (:file "renderer")
               (:file "regex-lexer")
               (:module "lexers"
                :components ((:file "ini")))
               (:module "renderers"
                :components ((:file "html")))))
