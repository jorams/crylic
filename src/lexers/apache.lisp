(defpackage :crylic/lexers/apache
  (:use :cl :crylic/regex-lexer)
  (:export #:apache-lexer))
(in-package :crylic/lexers/apache)

(define-regex-lexer apache-lexer () ()
  (:title "ApacheConf")
  (:description
   "Lexer for configuration files following the Apache config file format.")
  (:tags "apacheconf" "aconf" "apache")
  (:filenames ".htaccess" "apache.conf" "apache2.conf")
  (:mime-types "text/x-apacheconf")
  (:flags :multi-line-mode t :case-insensitive-mode t))

(defstate apache-lexer :root ()
  ("\\s+" :token :text)
  ("(#.*?)$" :token :comment)
  ("(<[^\\s>]+)(?:(\\s+)(.*?))?(>)"
   :groups (:name.tag :text :string :name.tag))
  ("([a-z]\\w*)(\\s+)"
   :groups (:name.builtin :text)
   :state :value)
  ("\\.+" :token :text))

(defstate apache-lexer :value ()
  ("\\\\\\n" :token :text)
  ("$" :token :text
       :state :pop!)
  ("\\\\" :token :text)
  ("[^\\S\\n]+" :token :text)
  ("\\d+\\.\\d+\\.\\d+\\.\\d+(?:/\\d+)?" :token :number)
  ("\\d+" :token :number)
  ("/([a-z0-9][\\w./-]+)" :token :string.other)
  (((concatenate 'string
                 "(on|off|none|any|all|double|email|dns|min|minimal|"
                 "os|productonly|full|emerg|alert|crit|error|warn|"
                 "notice|info|debug|registry|script|inetd|standalone|"
                 "user|group)\\b"))
   :token :keyword)
  ("\"([^\"\\\\]*(?:\\\\.[^\"\\\\]*)*)\"" :token :string.double)
  ("[^\\s\"\\\\]+" :token :text))
