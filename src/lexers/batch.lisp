(defpackage :crylic/lexers/batch
  (:use :cl :crylic/regex-lexer)
  (:export #:batch-lexer))
(in-package :crylic/lexers/batch)

(define-regex-lexer batch-lexer () ()
  (:title "Batchfile")
  (:description "Lexer for the DOS/Windows Batch file format.")
  (:tags "bat" "batch" "dosbatch" "winbatch")
  (:filenames "*.bat" "*.cmd")
  (:mime-types "application/x-dos-batch"))

(defstate batch-lexer :root  (:multi-line-mode t
                              :case-insensitive-mode t)
  ;; Lines can start with @ to prevent echo
  ("^\\s*@" :token :punctuation)
  ("^(\\s*)(rem\\s.*)$" :groups (:text :comment))
  ("\".*?\"" :token :string.double)
  ("'.*?'" :token :string.single)
  ("%%?[~$:\\w]+%?" :token :name.variable)
  ("::.*" :token :comment)              ; Technically :: only works at BOL
  ("\\b(set)(\\s+)(\\w+)" :groups (:keyword :text :name.variable))
  ("\\b(call)(\\s+)(:\\w+)" :groups (:keyword :text :name.label))
  ("\\b(goto)(\\s+)(\\w+)" :groups (:keyword :text :name.label))
  (((concatenate 'string
                 "\\b(set|call|echo|on|off|endlocal|for|do|goto|if|pause|"
                 "setlocal|shift|errorlevel|exist|defined|cmdextversion|"
                 "errorlevel|else|cd|md|del|deltree|cls|choice)\\b"))
   :token :keyword)
  ("\\b(equ|neq|lss|leq|gtr|geq)\\b" :token :operator)
  (:include :basic)
  ("." :token :text))

(defstate batch-lexer :echo  (:multi-line-mode t
                              :case-insensitive-mode t)
  ("\\^\\^|\\^<|\\^>|\\^\\|" :token :string.escape)
  ("\\n" :token :text
         :state :pop!)
  (:include :basic)
  ("[^\\'\"^]+" :token :text))

(defstate batch-lexer :basic (:multi-line-mode t
                              :case-insensitive-mode t)
  ("\".*?\"" :token :string.double)
  ("'.*?'" :token :string.single)
  ("`.*?`" :token :string.backtick)
  ("-?\\d+" :token :number)
  ("," :token :punctuation)
  ("=" :token :operator)
  ("/\\S+" :token :name)
  (":\\w+" :token :name.label)
  ("\\w:\\w+" :token :text)
  ("([<>|])(\\s*)(\\w+)" :groups (:punctuation :text :name)))
