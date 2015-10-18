(defpackage :crylic/lexers/lighttpd
  (:use :cl :crylic/regex-lexer)
  (:export #:lighttpd-lexer))
(in-package :crylic/lexers/lighttpd)

(define-regex-lexer lighttpd-lexer () ()
  (:title "Lighttpd configuration file")
  (:description "Lexer for Lighttpd configuration files.")
  (:tags "lighty" "lighttpd")
  (:mime-types "text/x-lighttpd-conf"))

(defstate lighttpd-lexer :root ()
  ("#.*\\n" :comment.single)
  ("/\\S*" :name)                       ; pathname
  ("[a-zA-Z._-]+" :keyword)
  ("\\d+\\.\\d+\\.\\d+\\.\\d+(?:/\\d+)?" :number)
  ("[0-9]+" :number)
  ("=>|=~|\\+=|==|=|\\+" :operator)
  ("\\$[A-Z]+" :name.builtin)
  ("[(){}\\[\\],]" :punctuation)
  ("\"([^\"\\\\]*(?:\\\\.[^\"\\\\]*)*)\"" :string.double)
  ("\\s+" :text))
