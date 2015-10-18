(defpackage :crylic/lexers/tcsh
  (:use :cl :crylic/regex-lexer)
  (:export #:tcsh-lexer))
(in-package :crylic/lexers/tcsh)

(define-regex-lexer tcsh-lexer () ()
  (:title "Tcsh")
  (:description "Lexer for tcsh scripts.")
  (:tags "tcsh" "csh")
  (:filenames "*.tcsh" "*.csh")
  (:mime-types "application/x-csh"))

(defstate tcsh-lexer :root ()
  (:include :basic)
  ("\\$\\(" :keyword (state :paren))
  ("\\$\\{#?" :keyword (state :curly))
  ("`" :string.backtick (state :backticks))
  (:include :data))

(defstate tcsh-lexer :basic ()
  (((concatenate 'string
                 "\\b(if|endif|else|while|then|foreach|case|default|"
                 "continue|goto|breaksw|end|switch|endsw)\\s*\\b"))
   :keyword)
  (((concatenate
     'string
     "\\b(alias|alloc|bg|bindkey|break|builtins|bye|caller|cd|chdir|"
     "complete|dirs|echo|echotc|eval|exec|exit|fg|filetest|getxvers|"
     "glob|getspath|hashstat|history|hup|inlib|jobs|kill|"
     "limit|log|login|logout|ls-F|migrate|newgrp|nice|nohup|notify|"
     "onintr|popd|printenv|pushd|rehash|repeat|rootnode|popd|pushd|"
     "set|shift|sched|setenv|setpath|settc|setty|setxvers|shift|"
     "source|stop|suspend|source|suspend|telltc|time|"
     "umask|unalias|uncomplete|unhash|universe|unlimit|unset|unsetenv|"
     "ver|wait|warp|watchlog|where|which)\\s*\\b"))
   :name.builtin)
  ("#.*" :comment)
  ("\\\\[\\w\\W]" :string.escape)
  ("(\\b\\w+)(\\s*)(=)" (groups :name.variable :text :operator))
  ("[\\[\\]{}()=]+" :operator)
  ("<<\\s*(\\'?)\\\\?(\\w+)[\\w\\W]+?\\2" :string)
  (";" :punctuation))

(defstate tcsh-lexer :data ()
  ("(?s)\"(\\\\\\\\|\\\\[0-7]+|\\\\.|[^\"\\\\])*\"" :string.double)
  ("(?s)'(\\\\\\\\|\\\\[0-7]+|\\\\.|[^'\\\\])*'" :string.single)
  ("\\s+" :text)
  ("[^=\\s\\[\\]{}()$\"\\'`\\\\;#]+" :text)
  ("\\d+(?= |\\Z)" :number)
  ("\\$#?(\\w+|.)" :name.variable))

(defstate tcsh-lexer :curly ()
  ("\\}" :keyword (state :pop!))
  (":-" :keyword)
  ("\\w+" :name.variable)
  ("[^}:\"\\'`$]+" :punctuation)
  (":" :punctuation)
  (:include :root))

(defstate tcsh-lexer :paren ()
  ("\\)" :keyword (state :pop!))
  (:include :root))

(defstate tcsh-lexer :backticks ()
  ("`" :string.backtick (state :pop!))
  (:include :root))
