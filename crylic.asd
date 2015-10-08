(asdf:defsystem :crylic
  :description "Syntax highlighting library for Common Lisp"
  :author "Joram Schrijver <i@joram.io>"
  :license "MIT"
  :depends-on (#:cl-ppcre)
  :pathname "src"
  :components ((:file "lexer")
               (:file "tokens")
               (:file "renderer")
               (:file "regex-lexer")
               (:module "lexers"
                :components ((:file "conf")
                             (:file "ini")))
               (:module "renderers"
                :components ((:file "html")))))
