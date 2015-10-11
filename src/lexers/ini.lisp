(defpackage :crylic/lexers/ini
  (:use :cl :crylic/lexer :crylic/regex-lexer)
  (:export #:ini-lexer))
(in-package :crylic/lexers/ini)

(define-lexer ini-lexer (regex-lexer) ()
  (:title "INI")
  (:description "Lexer for configuration files in INI style.")
  (:tags "ini" "cfg" "dosini")
  (:filenames "*.ini" "*.cfg")
  (:mime-types "text/x-ini"))

(defstate ini-lexer :root
  ("\\s+" (:token :text))
  ("[;#].*" (:token :comment.single))
  ("\\[.*?\\]$" (:token :keyword))
  ("(.*?)([ \\t]*)(=)([ \\t]*)(.*(?:\\n[ \\t].+)*)"
   (:groups (:name.attribute :text :operator :text :string))))
