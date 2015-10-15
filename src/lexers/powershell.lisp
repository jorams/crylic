(defpackage :crylic/lexers/powershell
  (:use :cl :crylic/lexer :crylic/regex-lexer)
  (:export #:powershell-lexer))
(in-package :crylic/lexers/powershell)

(define-lexer powershell-lexer (regex-lexer) ()
  (:title "PowerShell")
  (:description "Lexer for Windows PowerShell code.")
  (:tags "powershell" "posh" "ps1" "psm1")
  (:filenames "*.ps1" "*.psm1")
  (:mime-types "text/x-powershell"))

(let ((keywords '("while" "validateset" "validaterange" "validatepattern"
                  "validatelength" "validatecount" "until" "trap" "switch"
                  "return" "ref" "process" "param" "parameter" "in" "if"
                  "global:" "function" "foreach" "for" "finally" "filter"
                  "end" "elseif" "else" "dynamicparam" "do" "default"
                  "continue" "cmdletbinding" "break" "begin" "alias" "\\?" "%"
                  "#script" "#private" "#local" "#global" "mandatory"
                  "parametersetname" "position" "valuefrompipeline"
                  "valuefrompipelinebypropertyname"
                  "valuefromremainingarguments" "helpmessage" "try" "catch"
                  "throw"))
      (operators '("and" "as" "band" "bnot" "bor" "bxor" "casesensitive"
                   "ccontains" "ceq" "cge" "cgt" "cle" "clike" "clt" "cmatch"
                   "cne" "cnotcontains" "cnotlike" "cnotmatch" "contains"
                   "creplace" "eq" "exact" "f" "file" "ge" "gt" "icontains"
                   "ieq" "ige" "igt" "ile" "ilike" "ilt" "imatch" "ine"
                   "inotcontains" "inotlike" "inotmatch" "ireplace" "is"
                   "isnot" "le" "like" "lt" "match" "ne" "not" "notcontains"
                   "notlike" "notmatch" "or" "regex" "replace" "wildcard"))
      (verbs '("write" "where" "wait" "use" "update" "unregister" "undo"
               "trace" "test" "tee" "take" "suspend" "stop" "start" "split"
               "sort" "skip" "show" "set" "send" "select" "scroll" "resume"
               "restore" "restart" "resolve" "resize" "reset" "rename" "remove"
               "register" "receive" "read" "push" "pop" "ping" "out" "new"
               "move" "measure" "limit" "join" "invoke" "import" "group" "get"
               "format" "foreach" "export" "expand" "exit" "enter" "enable"
               "disconnect" "disable" "debug" "cxnew" "copy" "convertto"
               "convertfrom" "convert" "connect" "complete" "compare" "clear"
               "checkpoint" "aggregate" "add"))
      (commenthelp '("component" "description" "example" "externalhelp"
                     "forwardhelpcategory" "forwardhelptargetname"
                     "functionality" "inputs" "link" "notes" "outputs"
                     "parameter" "remotehelprunspace" "role" "synopsis")))
  (defstate powershell-lexer :root (:single-line-mode t
                                    :multi-line-mode t
                                    :case-insensitive-mode t)
    ;; We need to count pairs of parentheses for correct highlighting of
    ;; '$(...)' blocks in strings
    ("\\(" :token :punctuation
           :state :child)
    ("\\s+" :token :text)
    (((words commenthelp :prefix "^(\\s*#[#\\s]*)(\\."
                         :suffix ")([^\\n]*$)"
                         :capturing nil))
     :groups (:comment :string.doc :comment))
    ("#[^\\n]*?$" :token :comment)
    ("(&lt;|<)#" :token :comment.multiline
                 :state :multiline)
    ("@\"\\n" :token :string.heredoc
              :state :heredoc-double)
    ("@'\\n.*?\\n'@" :token :string.heredoc)
    ;; escaped syntax
    ("`[\\'\"$@-]" :token :punctuation)
    ("\"" :token :string.double
          :state :string)
    ("'([^']|'')*'" :token :string.single)
    ("(\\$|@@|@)((global|script|private|env):)?\\w+" :token :name.variable)
    (((words keywords :suffix "\\b")) :token :keyword)
    (((words operators :prefix "-" :suffix "\\b")) :token :operator)
    (((words verbs :suffix "-[a-z_]\\w*\\b")) :token :name.builtin)
    ("\\[[a-z_\\[][\\w. `,\\[\\]]*\\]" :token :name.constant) ; .net [type]s
    ("-[a-z_]\\w*" :token :name)
    ("\\w+" :token :name)
    ("[.,;@{}\\[\\]$()=+*/\\\\&%!~?^`|<>-]|::" :token :punctuation))

  (defstate powershell-lexer :child (:single-line-mode t
                                     :multi-line-mode t
                                     :case-insensitive-mode t)
    ("\\)" :token :punctuation
           :state :pop!)
    (:include :root))

  (defstate powershell-lexer :multiline (:single-line-mode t
                                         :multi-line-mode t
                                         :case-insensitive-mode t)
    ("[^#&.]+" :token :comment.multiline)
    ("#(>|&gt;)" :token :comment.multiline
                 :state :pop!)
    (((words commenthelp :prefix "\\.")) :token :string.doc)
    ("[#&.]" :token :comment.multiline))

  (defstate powershell-lexer :string (:single-line-mode t
                                      :multi-line-mode t
                                      :case-insensitive-mode t)
    ("`[0abfnrtv'\\\"$`]" :token :string.escape)
    ("[^$`\"]+" :token :string.double)
    ("\\$\\(" :token :punctuation
              :state :child)
    ("\"\"" :token :string.double)
    ("[`$]" :token :string.double)
    ("\"" :token :string.double
          :state :pop!))

  (defstate powershell-lexer :heredoc-double (:single-line-mode t
                                              :multi-line-mode t
                                              :case-insensitive-mode t)
    ("\\n\"@" :token :string.heredoc
              :state :pop!)
    ("\\$\\(" :token :punctuation
              :state :child)
    ("[^@\\n]+\"]" :token :string.heredoc)
    ("." :token :string.heredoc)))
