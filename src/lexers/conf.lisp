(defpackage :crylic/lexers/conf
  (:use :cl :crylic/lexer :crylic/regex-lexer)
  (:export #:conf-lexer))
(in-package :crylic/lexers/conf)

(define-lexer conf-lexer (regex-lexer) ()
  (:default-initargs :title "Config File"
                     :description "A generic lexer for configuration files"
                     :tags (list "conf" "config" "configuration")
                     :filenames (list "*.conf" "*.config")))

(defstate conf-lexer :root
  ("#.*?\\n"        (:token 'crylic/tokens:comment))
  ("\".*?\""        (:token 'crylic/tokens:literal/string/double))
  ("'.*?'"          (:token 'crylic/tokens:literal/string/single))
  (("[a-z]\\w*" :case-insensitive-mode t)
   (:token 'crylic/tokens:name))
  ("\\d+"           (:token 'crylic/tokens:literal/number))
  ("[^\\d\\w#\"']+" (:token 'crylic/tokens:text)))
