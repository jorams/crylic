(defpackage :crylic/lexers/java
  (:use :cl :crylic/regex-lexer)
  (:export #:java-lexer))
(in-package :crylic/lexers/java)

(define-regex-lexer java-lexer () ()
  (:title "Java")
  (:description "Lexer for Java source code")
  (:tags "java")
  (:filenames "*.java")
  (:mime-types "text/x-java")
  (:flags :multi-line-mode t :single-line-mode t))

(defstate java-lexer :root ()
  ("[^\\S\\n]+" :text)
  ("//.*?\\n" :comment.single)
  ("/\\*.*?\\*/" :comment.multiline)
  ;; Keywords: go before method names to avoid lexing "throw new XYZ" as a
  ;; method signature
  (((concatenate
     'string
     "(assert|break|case|catch|continue|default|do|else|finally|for|"
     "if|goto|instanceof|new|return|switch|this|throw|try|while)\\b"))
   :keyword)
  ;; Method names
  (((concatenate
     'string
     "((?:(?:[^\\W\\d]|\\$)[\\w.\\[\\]$<>]*\\s+)+?)" ; return arguments
     "((?:[^\\W\\d]|\\$)[\\w$]*)"                    ; method name
     "(\\s*)(\\()"))                                 ; signature start
   (groups (using 'java-lexer) :name.function :text :operator))
  ("@[^\\W\\d][\\w.]*" :name.decorator)
  (((concatenate
     'string
     "(abstract|const|enum|extends|final|implements|native|private|"
     "protected|public|static|strictfp|super|synchronized|throws|"
     "transient|volatile)\\b"))
   :keyword.declaration)
  ("(boolean|byte|char|double|float|int|long|short|void)\\b" :keyword.type)
  ("(package)(\\s+)" (groups :keyword.namespace :text)
                     (state :import))
  ("(true|false|null)\\b" :keyword.constant)
  ("(class|interface)(\\s+)" (groups :keyword.declaration :text)
                             (state :class))
  ("(import)(\\s+)" (groups :keyword.namespace :text)
                    (state :import))
  ("\"(\\\\\\\\|\\\\\"|[^\"])*\"" :string)
  ("'\\\\.'|'[^\\\\]'|'\\\\u[0-9a-fA-F]{4}'" :string.char)
  ("(\\.)((?:[^\\W\\d]|\\$)[\\w$]*)" (groups :operator :name.attribute))
  ("^\\s*([^\\W\\d]|\\$)[\\w$]*:" :name.label)
  ("([^\\W\\d]|\\$)[\\w$]*" :name)
  ("[~^*!%&\\[\\](){}<>|+=:;,./?-]" :operator)
  ("[0-9][0-9]*\\.[0-9]+([eE][0-9]+)?[fd]?" :number.float)
  ("0x[0-9a-fA-F]+" :number.hex)
  ("[0-9]+(_+[0-9]+)*L?" :number.integer)
  ("\\n" :text))

(defstate java-lexer :class ()
  ("([^\\W\\d]|\\$)[\\w$]*" :name.class (state :pop!)))

(defstate java-lexer :import ()
  ("[\\w.]+\\*?" :name.namespace (state :pop!)))
