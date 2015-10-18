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
  ("\\s+" :text)
  ("(#.*?)$" :comment)
  ("(<[^\\s>]+)(?:(\\s+)(.*?))?(>)"
   (groups :name.tag :text :string :name.tag))
  ("([a-z]\\w*)(\\s+)"
   (groups :name.builtin :text)
   (state :value))
  ("\\.+" :text))

(defstate apache-lexer :value ()
  ("\\\\\\n" :text)
  ("$" :text (state :pop!))
  ("\\\\" :text)
  ("[^\\S\\n]+" :text)
  ("\\d+\\.\\d+\\.\\d+\\.\\d+(?:/\\d+)?" :number)
  ("\\d+" :number)
  ("/([a-z0-9][\\w./-]+)" :string.other)
  (((concatenate 'string
                 "(on|off|none|any|all|double|email|dns|min|minimal|"
                 "os|productonly|full|emerg|alert|crit|error|warn|"
                 "notice|info|debug|registry|script|inetd|standalone|"
                 "user|group)\\b"))
   :keyword)
  ("\"([^\"\\\\]*(?:\\\\.[^\"\\\\]*)*)\"" :string.double)
  ("[^\\s\"\\\\]+" :text))
