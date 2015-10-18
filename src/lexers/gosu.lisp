(defpackage :crylic/lexers/gosu
  (:use :cl :crylic/regex-lexer)
  (:export #:gosu-lexer #:gosu-template-lexer))
(in-package :crylic/lexers/gosu)

(define-regex-lexer gosu-lexer () ()
  (:title "Gosu")
  (:description "Lexer for Gosu source code")
  (:tags "gosu")
  (:filenames "*.gs" "*.gsx" "*.gsp" "*.vark")
  (:mime-types "text/x-gosu")
  (:flags :multi-line-mode t :single-line-mode t))

(define-regex-lexer gosu-template-lexer (gosu-lexer) ()
  (:title "Gosu Template")
  (:description "Lexer for Gosu templates")
  (:tags "gst")
  (:filenames "*.gst")
  (:mime-types "text/x-gosu-template")
  (:default-state :template-text))

(defstate gosu-lexer :root ()
  ;; Method names
  (((string+ "^(\\s*(?:[a-zA-Z_][\\w.\\[\\]]*\\s+)+?)" ; modifiers etc.
             "([a-zA-Z_]\\w*)"                         ; method name
             "(\\s*)(\\()"))                           ; signature start
   (groups (using 'gosu-lexer) :name.function :text :operator))
  ("[^\\S\\n]" :text)
  ("//.*?\\n" :comment.single)
  ("/\\*.*?\\*/" :comment.multiline)
  ("@[a-zA-Z_][\\w.]*" :name.decorator)
  (((string+
     "(in|as|typeof|statictypeof|typeis|typeas|if|else|foreach|for|"
     "index|while|do|continue|break|return|try|catch|finally|this|"
     "throw|new|switch|case|default|eval|super|outer|classpath|"
     "using)\\b"))
   :keyword)
  (((string+
     "(var|delegate|construct|function|private|internal|protected|"
     "public|abstract|override|final|static|extends|transient|"
     "implements|represents|readonly)\\b"))
   :keyword.declaration)
  ("(property\\s+)(get|set)?" :keyword.declaration)
  ("(boolean|byte|char|double|float|int|long|short|void|block)\\b"
   :keyword.type)
  ("(package)(\\s+)" (groups :keyword.namespace :text))
  ("(true|false|null|NaN|Infinity)\\b" :keyword.constant)
  ("(class|interface|enhancement|enum)(\\s+)([a-zA-Z_]\\w*)"
   (groups :keyword.declaration :text :name.class))
  ("(uses)(\\s+)([\\w.]+\\*?)"
   (groups :keyword.namespace :text :name.namespace))
  ("\"" :string (state :string))
  ("(\\??[.#])([a-zA-Z_]\\w*)" (groups :operator :name.attribute))
  ("(:)([a-zA-Z_]\\w*)" (groups :operator :name.attribute))
  ("[a-zA-Z_$]\\w*" :name)
  ("and|or|not|[\\\\~^*!%&\\[\\](){}<>|+=:;,./?-]" :operator)
  ("[0-9][0-9]*\\.[0-9]+([eE][0-9]+)?[fd]?" :number.float)
  ("[0-9]+" :number.integer)
  ("\\n" :text))

(defstate gosu-lexer :template-text ()
  ("(\\\\<)|(\\\\\\$)" :string)
  ("(<%@\\s+)(extends|params)" (groups :operator :name.decorator)
                               (state :string-template))
  ("<%!--.*?--%>" :comment.multiline)
  ("(<%)|(<%=)" :operator (state :string-template))
  ("\\$\\{" :operator (state :string-template-shorthand))
  ("." :string))

(defstate gosu-lexer :string ()
  ("\"" :string (state :pop!))
  (:include :template-text))

(defstate gosu-lexer :string-template ()
  ("\"" :string (state :string))
  ("%>" :operator (state :pop!))
  (:include :root))

(defstate gosu-lexer :string-template-shorthand ()
  ("\"" :string (state :string))
  ("\\{" :operator (state :string-template-shorthand))
  ("\\}" :operator (state :pop!))
  (:include :root))
