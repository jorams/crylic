(defpackage :crylic/lexers/groovy
  (:use :cl :crylic/regex-lexer)
  (:export #:groovy-lexer))
(in-package :crylic/lexers/groovy)

(define-regex-lexer groovy-lexer () ()
  (:title "Groovy")
  (:description "Lexer for Groovy source code.")
  (:tags "groovy")
  (:filenames "*.groovy" "*.gradle")
  (:mime-types "text/x-groovy")
  (:flags :multi-line-mode t :single-line-mode t))

(defstate groovy-lexer :root ()
  ;; Groovy allows a file to start with a shebang
  ("#!(.*?)$" :comment.preproc (state :base))
  ("\\A" (state :base)))

(defstate groovy-lexer :base ()
  ;; method names
  (((string+ "^(\\s*(?:[a-zA-Z_][\\w.\\[\\]]*\\s+)+?)" ; return arguments
             "([a-zA-Z_]\\w*)"                         ; method name
             "(\\s*)(\\()"))                           ; signature start
   (groups (using 'groovy-lexer) :name.function :text :operator))
  ("[^\\S\\n]" :text)
  ("//.*?\\n" :comment.single)
  ("/\\*.*?\\*/" :comment.multiline)
  ("@[a-zA-Z_][\\w.]*" :name.decorator)
  (((string+
     "(assert|break|case|catch|continue|default|do|else|finally|for|"
     "if|goto|instanceof|new|return|switch|this|throw|try|while|in|as)\\b"))
   :keyword)
  (((string+ "(abstract|const|enum|extends|final|implements|native|private|"
             "protected|public|static|strictfp|super|synchronized|throws|"
             "transient|volatile)\\b"))
   :keyword.declaration)
  (((string+ "(def|boolean|byte|char|double|float|int|long|short|void)\\b"))
   :keyword.type)
  ("(package)(\\s+)" (groups :keyword.namespace :text))
  ("(true|false|null)\\b" :keyword.constant)
  ("(class|interface)(\\s+)" (groups :keyword.declaration :text)
                             (state :class))
  ("(import)(\\s+)" (groups :keyword.namespace :text)
                    (state :import))
  ("\"\"\".*?\"\"\"" :string.double)
  ("'''.*?'''" :string.double)
  ("\"(\\\\\\\\|\\\\\"|[^\"])*\"" :string.double)
  ("'(\\\\\\\\|\\\\'|[^'])*'" :string.single)
  ("\\$/((?!/\\$).)*/\\$" :string)
  ("/(\\\\\\\\|\\\\\"|[^/])*/" :string)
  ("'\\\\.'|'[^\\\\]'|'\\\\u[0-9a-fA-F]{4}'" :string.char)
  ("(\\.)([a-zA-Z_]\\w*)" (groups :operator :name.attribute))
  ("[a-zA-Z_]\\w*:" :name.label)
  ("[a-zA-Z_$]\\w*" :name)
  ("[~^*!%&\\[\\](){}<>|+=:;,./?-]" :operator)
  ("[0-9][0-9]*\\.[0-9]+([eE][0-9]+)?[fd]?" :number.float)
  ("0x[0-9a-fA-F]+" :number.hex)
  ("[0-9]+L?" :number.integer)
  ("\\n" :text))

(defstate groovy-lexer :class ()
  ("[a-zA-Z_]\\w*" :name.class (state :pop!)))

(defstate groovy-lexer :import ()
  ("[\\w.]+\\*?" :name.namespace (state :pop!)))
