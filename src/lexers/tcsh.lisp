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
  ("\\$\\(" :token :keyword
            :state :paren)
  ("\\$\\{#?" :token :keyword
              :state :curly)
  ("`" :token :string.backtick
       :state :backticks)
  (:include :data))

(defstate tcsh-lexer :basic ()
  (((concatenate 'string
                 "\\b(if|endif|else|while|then|foreach|case|default|"
                 "continue|goto|breaksw|end|switch|endsw)\\s*\\b"))
   :token :keyword)
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
   :token :name.builtin)
  ("#.*" :token :comment)
  ("\\\\[\\w\\W]" :token :string.escape)
  ("(\\b\\w+)(\\s*)(=)" :groups (:name.variable :text :operator))
  ("[\\[\\]{}()=]+" :token :operator)
  ("<<\\s*(\\'?)\\\\?(\\w+)[\\w\\W]+?\\2" :token :string)
  (";" :token :punctuation))

(defstate tcsh-lexer :data ()
  ("(?s)\"(\\\\\\\\|\\\\[0-7]+|\\\\.|[^\"\\\\])*\"" :token :string.double)
  ("(?s)'(\\\\\\\\|\\\\[0-7]+|\\\\.|[^'\\\\])*'" :token :string.single)
  ("\\s+" :token :text)
  ("[^=\\s\\[\\]{}()$\"\\'`\\\\;#]+" :token :text)
  ("\\d+(?= |\\Z)" :token :number)
  ("\\$#?(\\w+|.)" :token :name.variable))

(defstate tcsh-lexer :curly ()
  ("\\}" :token :keyword
         :state :pop!)
  (":-" :token :keyword)
  ("\\w+" :token :name.variable)
  ("[^}:\"\\'`$]+" :token :punctuation)
  (":" :token :punctuation)
  (:include :root))

(defstate tcsh-lexer :paren ()
  ("\\)" :token :keyword
         :state :pop!)
  (:include :root))

(defstate tcsh-lexer :backticks ()
  ("`" :token :string.backtick
       :state :pop!)
  (:include :root))
