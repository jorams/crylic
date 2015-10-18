(defpackage :crylic/lexers/cfengine3
  (:use :cl :crylic/regex-lexer)
  (:export #:cfengine3-lexer))
(in-package :crylic/lexers/cfengine3)

(define-regex-lexer cfengine3-lexer () ()
  (:title "CFEngine3")
  (:description "Lexer for CFEngine3 policy files")
  (:tags "cfengine3" "cf3")
  (:filenames "*.cf"))

(defstate cfengine3-lexer :root ()
  ("#.*?\\n" :comment)
  ("(body)(\\s+)(\\S+)(\\s+)(control)"
   (groups :keyword :text :keyword :text :keyword))
  ("(body|bundle)(\\s+)(\\S+)(\\s+)(\\w+)(\\()"
   (groups :keyword :text :keyword :text :name.function :punctuation)
   (state :arglist))
  ("(body|bundle)(\\s+)(\\S+)(\\s+)(\\w+)"
   (groups :keyword :text :keyword :text :name.function))
  ("(\")([^\"]+)(\")(\\s+)(string|slist|int|real)(\\s*)(=>)(\\s*)"
   (groups :punctuation :name.variable :punctuation
                        :text :keyword.type :text :operator :text))
  ("(\\S+)(\\s*)(=>)(\\s*)"
   (groups :keyword.reserved :text :operator :text))
  ("\"" :string (state :string))
  ("(\\w+)(\\()" (groups :name.function :punctuation))
  ("([\\w.!&|()]+)(::)" (groups :name.class :punctuation))
  ("(\\w+)(:)" (groups :keyword.declaration :punctuation))
  ("@[{(][^)}]+[})]" :name.variable)
  ("[(){},;]" :punctuation)
  ("=>" :operator)
  ("->" :operator)
  ("\\d+\\.\\d+" :number.float)
  ("\\d+" :number.integer)
  ("\\w+" :name.function)
  ("\\s+" :text))

(defstate cfengine3-lexer :string ()
  ("\\$[{(]" :string.interpol (state :interpol))
  ("\\\\." :string.escape)
  ("\"" :string (state :pop!))
  ("\\n" :string)
  ("." :string))

(defstate cfengine3-lexer :interpol ()
  ("\\$[{(]" :string.interpol (state :interpol))
  ("[})]" :string.interpol (state :pop!))
  ("[^${()}]+"  :string.interpol))

(defstate cfengine3-lexer :arglist ()
  ("\\)" :punctuation (state :pop!))
  ("," :punctuation)
  ("\\w+" :name.variable)
  ("\\s+" :text))
