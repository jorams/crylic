(defpackage :crylic/lexers/lighttpd
  (:use :cl :crylic/lexer :crylic/regex-lexer)
  (:export #:lighttpd-lexer))
(in-package :crylic/lexers/lighttpd)

(define-lexer lighttpd-lexer (regex-lexer) ()
  (:title "Lighttpd configuration file")
  (:description "Lexer for Lighttpd configuration files.")
  (:tags "lighty" "lighttpd")
  (:mime-types "text/x-lighttpd-conf"))

(defstate lighttpd-lexer :root ()
  ("#.*\\n" :token :comment.single)
  ("/\\S*" :token :name)                ; pathname
  ("[a-zA-Z._-]+" :token :keyword)
  ("\\d+\\.\\d+\\.\\d+\\.\\d+(?:/\\d+)?" :token :number)
  ("[0-9]+" :token :number)
  ("=>|=~|\\+=|==|=|\\+" :token :operator)
  ("\\$[A-Z]+" :token :name.builtin)
  ("[(){}\\[\\],]" :token :punctuation)
  ("\"([^\"\\\\]*(?:\\\\.[^\"\\\\]*)*)\"" :token :string.double)
  ("\\s+" :token :text))
