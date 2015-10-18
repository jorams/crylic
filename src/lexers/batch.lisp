(defpackage :crylic/lexers/batch
  (:use :cl :crylic/regex-lexer)
  (:export #:batch-lexer))
(in-package :crylic/lexers/batch)

(define-regex-lexer batch-lexer () ()
  (:title "Batchfile")
  (:description "Lexer for the DOS/Windows Batch file format.")
  (:tags "bat" "batch" "dosbatch" "winbatch")
  (:filenames "*.bat" "*.cmd")
  (:mime-types "application/x-dos-batch")
  (:flags :multi-line-mode t :case-insensitive-mode t))

(defstate batch-lexer :root  ()
  ;; Lines can start with @ to prevent echo
  ("^\\s*@" :punctuation)
  ("^(\\s*)(rem\\s.*)$" (groups :text :comment))
  ("\".*?\"" :string.double)
  ("'.*?'" :string.single)
  ("%%?[~$:\\w]+%?" :name.variable)
  ("::.*" :comment)                     ; Technically :: only works at BOL
  ("\\b(set)(\\s+)(\\w+)" (groups :keyword :text :name.variable))
  ("\\b(call)(\\s+)(:\\w+)" (groups :keyword :text :name.label))
  ("\\b(goto)(\\s+)(\\w+)" (groups :keyword :text :name.label))
  (((string+ "\\b(set|call|echo|on|off|endlocal|for|do|goto|if|pause|"
             "setlocal|shift|errorlevel|exist|defined|cmdextversion|"
             "errorlevel|else|cd|md|del|deltree|cls|choice)\\b"))
   :keyword)
  ("\\b(equ|neq|lss|leq|gtr|geq)\\b" :operator)
  (:include :basic)
  ("." :text))

(defstate batch-lexer :echo  ()
  ("\\^\\^|\\^<|\\^>|\\^\\|" :string.escape)
  ("\\n" :text (state :pop!))
  (:include :basic)
  ("[^\\'\"^]+" :text))

(defstate batch-lexer :basic ()
  ("\".*?\"" :string.double)
  ("'.*?'" :string.single)
  ("`.*?`" :string.backtick)
  ("-?\\d+" :number)
  ("," :punctuation)
  ("=" :operator)
  ("/\\S+" :name)
  (":\\w+" :name.label)
  ("\\w:\\w+" :text)
  ("([<>|])(\\s*)(\\w+)" (groups :punctuation :text :name)))
