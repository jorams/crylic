(defpackage :crylic/lexers/bash
  (:use :cl :crylic/regex-lexer)
  (:export #:bash-lexer))
(in-package :crylic/lexers/bash)

(define-regex-lexer bash-lexer () ()
  (:title "Bash")
  (:description "Lexer for (ba|k|)sh shell scripts.")
  (:tags "bash" "sh" "ksh" "shell")
  (:filenames "*.sh" "*.ksh" "*.bash" "*.ebuild" "*.eclass"
              ".bashrc" "bashrc" ".bash_*" "bash_*" "PKGBUILD")
  (:mime-types "application/x-sh" "application/x-shellscript"))

(defstate bash-lexer :root ()
  (:include :basic)
  ("`" :string.backtick
       (state :backticks))
  (:include :data)
  (:include :interp))

(defstate bash-lexer :interp ()
  ("\\$\\(\\(" :keyword (state :math))
  ("\\$\\(" :keyword (state :paren))
  ("\\$\\{#?" :string.interpol (state :curly))
  ("\\$[a-fA-F_][a-fA-F0-9_]*" :name.variable) ; user variable
  ("\\$(?:\\d+|[#$?!_*@-])" :name.variable)    ; builtin
  ("\\$" :text))

(defstate bash-lexer :basic ()
  (((concatenate 'string
                 "\\b(if|fi|else|while|do|done|for|then|return|function|case|"
                 "select|continue|until|esac|elif)(\\s*)\\b"))
   (groups :keyword :text))
  (((concatenate 'string
                 "\\b(alias|bg|bind|break|builtin|caller|cd|command|compgen|"
                 "complete|declare|dirs|disown|echo|enable|eval|exec|exit|"
                 "export|false|fc|fg|getopts|hash|help|history|jobs|kill|let|"
                 "local|logout|popd|printf|pushd|pwd|read|readonly|set|shift|"
                 "shopt|source|suspend|test|time|times|trap|true|type|typeset|"
                 "ulimit|umask|unalias|unset|wait)\\s*\\b(?!\\.)"))
   :name.builtin)
  ("\\A#!.+\\n" :comment.hashbang)
  ("#.*\\n" :comment.single)
  ("\\\\[\\w\\W]" :string.escape)
  ("(\\b\\w+)(\\s*)(=)" (groups :name.variable :text :operator))
  ("[\\[\\]{}()=]" :operator)
  ("<<<" :operator)                     ; here-string
  ("<<-?\\s*(\\'?)\\\\?(\\w+)[\\w\\W]+?\\2" :string)
  ("&&|\\|\\|" :operator))

(defstate bash-lexer :data ()
  ("(?s)\\$?\"(\\\\\\\\|\\\\[0-7]+|\\\\.|[^\"\\\\$])*\"" :string.double)
  ("\"" :string.double
        (state :string))
  ("(?s)\\$'(\\\\\\\\|\\\\[0-7]+|\\\\.|[^'\\\\])*'" :string.single)
  ("(?s)'.*?'" :string.single)
  (";" :punctuation)
  ("&" :punctuation)
  ("\\|" :punctuation)
  ("\\s+" :text)
  ("\\d+(?= |\\Z)" :number)
  ("[^=\\s\\[\\]{}()$\"\\'`\\\\<&|;]+" :text)
  ("<" :text))

(defstate bash-lexer :string ()
  ("\"" :string.double (state :pop!))
  ("(?s)(\\\\\\\\|\\\\[0-7]+|\\\\.|[^\"\\\\$])+" :string.double)
  (:include :interp))

(defstate bash-lexer :curly ()
  ("\\}" :string.interpol (state :pop!))
  (":-" :keyword)
  ("\\w+" :name.variable)
  ("[^}:\"\\'`$\\\\]+" :punctuation)
  (":" :punctuation)
  (:include :root))

(defstate bash-lexer :paren ()
  ("\\)" :keyword (state :pop!))
  (:include :root))

(defstate bash-lexer :math ()
  ("\\)\\)" :keyword (state :pop!))
  ("[-+*/%^|&]|\\*\\*|\\|\\|" :operator)
  ("\\d+#\\d+" :number)
  ("\\d+#(?! )" :number)
  ("\\d+" :number)
  (:include :root))

(defstate bash-lexer :backticks ()
  ("`" :string.backtick (state :pop!))
  (:include :root))
