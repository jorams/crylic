(defpackage :crylic/lexers/powershell
  (:use :cl :crylic/regex-lexer)
  (:export #:powershell-lexer))
(in-package :crylic/lexers/powershell)

(define-regex-lexer powershell-lexer () ()
  (:title "PowerShell")
  (:description "Lexer for Windows PowerShell code.")
  (:tags "powershell" "posh" "ps1" "psm1")
  (:filenames "*.ps1" "*.psm1")
  (:mime-types "text/x-powershell")
  (:flags :single-line-mode t :multi-line-mode t :case-insensitive-mode t))

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
  (defstate powershell-lexer :root ()
    ;; We need to count pairs of parentheses for correct highlighting of
    ;; '$(...)' blocks in strings
    ("\\(" :punctuation (state :child))
    ("\\s+" :text)
    (((words commenthelp :prefix "^(\\s*#[#\\s]*)(\\."
                         :suffix ")([^\\n]*$)"
                         :capturing nil))
     (groups :comment :string.doc :comment))
    ("#[^\\n]*?$" :comment)
    ("(&lt;|<)#" :comment.multiline
                 (state :multiline))
    ("@\"\\n" :string.heredoc (state :heredoc-double))
    ("@'\\n.*?\\n'@" :string.heredoc)
    ;; escaped syntax
    ("`[\\'\"$@-]" :punctuation)
    ("\"" :string.double (state :string))
    ("'([^']|'')*'" :string.single)
    ("(\\$|@@|@)((global|script|private|env):)?\\w+" :name.variable)
    (((words keywords :suffix "\\b")) :keyword)
    (((words operators :prefix "-" :suffix "\\b")) :operator)
    (((words verbs :suffix "-[a-z_]\\w*\\b")) :name.builtin)
    ("\\[[a-z_\\[][\\w. `,\\[\\]]*\\]" :name.constant) ; .net [type]s
    ("-[a-z_]\\w*" :name)
    ("\\w+" :name)
    ("[.,;@{}\\[\\]$()=+*/\\\\&%!~?^`|<>-]|::" :punctuation))

  (defstate powershell-lexer :child ()
    ("\\)" :punctuation (state :pop!))
    (:include :root))

  (defstate powershell-lexer :multiline ()
    ("[^#&.]+" :comment.multiline)
    ("#(>|&gt;)" :comment.multiline (state :pop!))
    (((words commenthelp :prefix "\\.")) :string.doc)
    ("[#&.]" :comment.multiline))

  (defstate powershell-lexer :string ()
    ("`[0abfnrtv'\\\"$`]" :string.escape)
    ("[^$`\"]+" :string.double)
    ("\\$\\(" :punctuation (state :child))
    ("\"\"" :string.double)
    ("[`$]" :string.double)
    ("\"" :string.double (state :pop!)))

  (defstate powershell-lexer :heredoc-double ()
    ("\\n\"@" :string.heredoc (state :pop!))
    ("\\$\\(" :punctuation (state :child))
    ("[^@\\n]+\"]" :string.heredoc)
    ("." :string.heredoc)))
