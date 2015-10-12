(defpackage :crylic/lexers/apache
  (:use :cl :crylic/lexer :crylic/regex-lexer)
  (:export #:apache-lexer))
(in-package :crylic/lexers/apache)

(define-lexer apache-lexer (regex-lexer) ()
  (:title "ApacheConf")
  (:description
   "Lexer for configuration files following the Apache config file format.")
  (:tags "apacheconf" "aconf" "apache")
  (:filenames ".htaccess" "apache.conf" "apache2.conf")
  (:mime-types "text/x-apacheconf"))

(defstate apache-lexer :root (:multi-line-mode t :case-insensitive-mode t)
  ("\\s+" :token :text)
  ("(#.*?)$" :token :comment)
  ("(<[^\\s>]+)(?:(\\s+)(.*?))?(>)"
   :groups (:name.tag :text :string :name.tag))
  ("([a-z]\\w*)(\\s+)"
   :groups (:name.builtin :text)
   :state :value)
  ("\\.+" :token :text))

(defstate apache-lexer :value (:multi-line-mode t :case-insensitive-mode t)
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
