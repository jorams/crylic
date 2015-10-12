(defpackage :crylic/lexers/cfengine3
  (:use :cl :crylic/lexer :crylic/regex-lexer)
  (:export #:cfengine3-lexer))
(in-package :crylic/lexers/cfengine3)

(define-lexer cfengine3-lexer (regex-lexer) ()
  (:title "CFEngine3")
  (:description "Lexer for CFEngine3 policy files")
  (:tags "cfengine3" "cf3")
  (:filenames "*.cf"))

(defstate cfengine3-lexer :root
  ("#.*?\\n" :token :comment)
  ("(body)(\\s+)(\\S+)(\\s+)(control)"
   :groups (:keyword :text :keyword :text :keyword))
  ("(body|bundle)(\\s+)(\\S+)(\\s+)(\\w+)(\\()"
   :groups (:keyword :text :keyword :text :name.function :punctuation)
   :state :arglist)
  ("(body|bundle)(\\s+)(\\S+)(\\s+)(\\w+)"
   :groups (:keyword :text :keyword :text :name.function))
  ("(\")([^\"]+)(\")(\\s+)(string|slist|int|real)(\\s*)(=>)(\\s*)"
   :groups (:punctuation :name.variable :punctuation
                         :text :keyword.type :text :operator :text))
  ("(\\S+)(\\s*)(=>)(\\s*)"
   :groups (:keyword.reserved :text :operator :text))
  ("\"" :token :string
        :state :string)
  ("(\\w+)(\\()" :groups (:name.function :punctuation))
  ("([\\w.!&|()]+)(::)" :groups (:name.class :punctuation))
  ("(\\w+)(:)" :groups (:keyword.declaration :punctuation))
  ("@[{(][^)}]+[})]" :token :name.variable)
  ("[(){},;]" :token :punctuation)
  ("=>" :token :operator)
  ("->" :token :operator)
  ("\\d+\\.\\d+" :token :number.float)
  ("\\d+" :token :number.integer)
  ("\\w+" :token :name.function)
  ("\\s+" :token :text))

(defstate cfengine3-lexer :string
  ("\\$[{(]" :token :string.interpol
             :state :interpol)
  ("\\\\." :token :string.escape)
  ("\"" :token :string
        :state :pop!)
  ("\\n" :token :string)
  ("." :token :string))

(defstate cfengine3-lexer :interpol
  ("\\$[{(]" :token :string.interpol
             :state :interpol)
  ("[})]" :token :string.interpol
          :state :pop!)
  ("[^${()}]+"  :token :string.interpol))

(defstate cfengine3-lexer :arglist
  ("\\)" :token :punctuation
         :state :pop!)
  ("," :token :punctuation)
  ("\\w+" :token :name.variable)
  ("\\s+" :token :text))
