(defpackage :crylic/lexers/properties
  (:use :cl :crylic/lexer :crylic/regex-lexer)
  (:export #:properties-lexer))
(in-package :crylic/lexers/properties)

(define-lexer properties-lexer (regex-lexer) ()
  (:title "Properties")
  (:description "Lexer for configuration files in Java's properties format.")
  (:tags "properties" "jproperties")
  (:filenames "*.properties")
  (:mime-types "text/x-java-properties"))

(defstate properties-lexer :root
  ("\\s+" (:token :text))
  ("(?:[;#]|//).*$" (:token :comment))
  ("(.*?)([ \\t]*)([=:])([ \\t]*)(.*(?:(?<=\\\\)\\n.*)*)"
   (:groups (:name.attribute :text :operator :text :string))))
