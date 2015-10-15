(defpackage :crylic/lexers/bash
  (:use :cl :crylic/lexer :crylic/regex-lexer)
  (:export #:bash-lexer))
(in-package :crylic/lexers/bash)

(define-lexer bash-lexer (regex-lexer) ()
  (:title "Bash")
  (:description "Lexer for (ba|k|)sh shell scripts.")
  (:tags "bash" "sh" "ksh" "shell")
  (:filenames "*.sh" "*.ksh" "*.bash" "*.ebuild" "*.eclass"
              ".bashrc" "bashrc" ".bash_*" "bash_*" "PKGBUILD")
  (:mime-types "application/x-sh" "application/x-shellscript"))

(defstate bash-lexer :root ()
  (:include :basic)
  ("`" :token :string.backtick
       :state :backticks)
  (:include :data)
  (:include :interp))

(defstate bash-lexer :interp ()
  ("\\$\\(\\(" :token :keyword
               :state :math)
  ("\\$\\(" :token :keyword
            :state :paren)
  ("\\$\\{#?" :token :string.interpol
              :state :curly)
  ("\\$[a-fA-F_][a-fA-F0-9_]*" :token :name.variable) ; user variable
  ("\\$(?:\\d+|[#$?!_*@-])" :token :name.variable)    ; builtin
  ("\\$" :token :text))

(defstate bash-lexer :basic ()
  (((concatenate 'string
                 "\\b(if|fi|else|while|do|done|for|then|return|function|case|"
                 "select|continue|until|esac|elif)(\\s*)\\b"))
   :groups (:keyword :text))
  (((concatenate 'string
                 "\\b(alias|bg|bind|break|builtin|caller|cd|command|compgen|"
                 "complete|declare|dirs|disown|echo|enable|eval|exec|exit|"
                 "export|false|fc|fg|getopts|hash|help|history|jobs|kill|let|"
                 "local|logout|popd|printf|pushd|pwd|read|readonly|set|shift|"
                 "shopt|source|suspend|test|time|times|trap|true|type|typeset|"
                 "ulimit|umask|unalias|unset|wait)\\s*\\b(?!\\.)"))
   :token :name.builtin)
  ("\\A#!.+\\n" :token :comment.hashbang)
  ("#.*\\n" :token :comment.single)
  ("\\\\[\\w\\W]" :token :string.escape)
  ("(\\b\\w+)(\\s*)(=)"
   :groups (:name.variable :text :operator))
  ("[\\[\\]{}()=]" :token :operator)
  ("<<<" :token :operator)              ; here-string
  ("<<-?\\s*(\\'?)\\\\?(\\w+)[\\w\\W]+?\\2" :token :string)
  ("&&|\\|\\|" :token :operator))

(defstate bash-lexer :data ()
  ("(?s)\\$?\"(\\\\\\\\|\\\\[0-7]+|\\\\.|[^\"\\\\$])*\""
   :token :string.double)
  ("\"" :token :string.double
        :state :string)
  ("(?s)\\$'(\\\\\\\\|\\\\[0-7]+|\\\\.|[^'\\\\])*'" :token :string.single)
  ("(?s)'.*?'" :token :string.single)
  (";" :token :punctuation)
  ("&" :token :punctuation)
  ("\\|" :token :punctuation)
  ("\\s+" :token :text)
  ("\\d+(?= |\\Z)" :token :number)
  ("[^=\\s\\[\\]{}()$\"\\'`\\\\<&|;]+" :token :text)
  ("<" :token :text))

(defstate bash-lexer :string ()
  ("\"" :token :string.double
        :state :pop!)
  ("(?s)(\\\\\\\\|\\\\[0-7]+|\\\\.|[^\"\\\\$])+" :token :string.double)
  (:include :interp))

(defstate bash-lexer :curly ()
  ("\\}" :token :string.interpol
         :state :pop!)
  (":-" :token :keyword)
  ("\\w+" :token :name.variable)
  ("[^}:\"\\'`$\\\\]+" :token :punctuation)
  (":" :token :punctuation)
  (:include :root))

(defstate bash-lexer :paren ()
  ("\\)" :token :keyword
         :state :pop!)
  (:include :root))

(defstate bash-lexer :math ()
  ("\\)\\)" :token :keyword
            :state :pop!)
  ("[-+*/%^|&]|\\*\\*|\\|\\|" :token :operator)
  ("\\d+#\\d+" :token :number)
  ("\\d+#(?! )" :token :number)
  ("\\d+" :token :number)
  (:include :root))

(defstate bash-lexer :backticks ()
  ("`" :token :string.backtick
       :state :pop!)
  (:include :root))
