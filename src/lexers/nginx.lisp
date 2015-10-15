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
   :groups (:keyword :text :name))
  ("[^\\s;#]+" :token :keyword
               :state :stmt)
  (:include :base))

(defstate nginx-lexer :block ()
  ("\\}" :token :punctuation
         :state (:pop! 2))
  ("[^\\s;#]+" :token :keyword.namespace
               :state :stmt)
  (:include :base))

(defstate nginx-lexer :stmt ()
  ("\\{" :token :punctuation
         :state :block)
  (";" :token :punctuation
       :state :pop!)
  (:include :base))

(defstate nginx-lexer :base ()
  ("#.*\\n" :token :comment.single)
  ("on|off" :token :name.constant)
  ("\\$[^\\s;#()]+" :token :name.variable)
  ("([a-z0-9.-]+)(:)([0-9]+)"
   :groups (:name :punctuation :number.integer))
  ("[a-z-]+/[a-z-+]+" :token :string)   ; mimetype
  ("[0-9]+[km]?\\b" :token :number.integer)
  ("(~)(\\s*)([^\\s{]+)"
   :groups (:punctuation :text :string.regex))
  ("[:=~]" :token :punctuation)
  ("[^\\s;#{}$]+" :token :string)       ; catch all
  ("/[^\\s;#]*" :token :name)           ; pathname
  ("\\s+" :token :text)
  ("[$;]" :token :text)                 ; leftover characters
  )
