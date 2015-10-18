(defpackage :crylic/lexers/nginx
  (:use :cl :crylic/regex-lexer)
  (:export #:nginx-lexer))
(in-package :crylic/lexers/nginx)

(define-regex-lexer nginx-lexer () ()
  (:title "Nginx configuration file")
  (:description "Lexer for Nginx configuration files.")
  (:tags "nginx")
  (:mime-types "text/x-nginx-conf"))

(defstate nginx-lexer :root ()
  ("(include)(\\s+)([^\\s;]+)"
   (groups :keyword :text :name))
  ("[^\\s;#]+" :keyword (state :stmt))
  (:include :base))

(defstate nginx-lexer :block ()
  ("\\}" :punctuation (state :pop! 2))
  ("[^\\s;#]+" :keyword.namespace (state :stmt))
  (:include :base))

(defstate nginx-lexer :stmt ()
  ("\\{" :punctuation (state :block))
  (";" :punctuation (state :pop!))
  (:include :base))

(defstate nginx-lexer :base ()
  ("#.*\\n" :comment.single)
  ("on|off" :name.constant)
  ("\\$[^\\s;#()]+" :name.variable)
  ("([a-z0-9.-]+)(:)([0-9]+)"
   (groups :name :punctuation :number.integer))
  ("[a-z-]+/[a-z-+]+" :string)          ; mimetype
  ("[0-9]+[km]?\\b" :number.integer)
  ("(~)(\\s*)([^\\s{]+)"
   (groups :punctuation :text :string.regex))
  ("[:=~]" :punctuation)
  ("[^\\s;#{}$]+" :string)              ; catch all
  ("/[^\\s;#]*" :name)                  ; pathname
  ("\\s+" :text)
  ("[$;]" :text)                        ; leftover characters
  )
