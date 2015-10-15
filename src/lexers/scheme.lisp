(defpackage :crylic/lexers/scheme
  (:use :cl :crylic/lexer :crylic/regex-lexer)
  (:export #:scheme-lexer))
(in-package :crylic/lexers/scheme)

(define-lexer scheme-lexer (regex-lexer) ()
  (:title "Scheme")
  (:description "Lexer for Scheme code.")
  (:tags "scheme" "scm")
  (:filenames "*.scm" "*.ss")
  (:mime-types "text/x-scheme" "application/x-scheme"))

(let ((keywords '("lambda" "define" "if" "else" "cond" "and" "or" "case" "let"
                  "let*" "letrec" "begin" "do" "delay" "set!" "=>" "quote"
                  "quasiquote" "unquote" "unquote-splicing" "define-syntax"
                  "let-syntax" "letrec-syntax" "syntax-rules"))
      (builtins '("*" "+" "-" "/" "<" "<=" "=" ">" ">=" "abs" "acos" "angle"
                  "append" "apply" "asin" "assoc" "assq" "assv" "atan"
                  "boolean?" "caaaar" "caaadr" "caaar" "caadar" "caaddr" "caadr"
                  "caar" "cadaar" "cadadr" "cadar" "caddar" "cadddr" "caddr"
                  "cadr" "call-with-current-continuation" "call-with-input-file"
                  "call-with-output-file" "call-with-values" "call/cc" "car"
                  "cdaaar" "cdaadr" "cdaar" "cdadar" "cdaddr" "cdadr" "cdar"
                  "cddaar" "cddadr" "cddar" "cdddar" "cddddr" "cdddr" "cddr"
                  "cdr" "ceiling" "char->integer" "char-alphabetic?"
                  "char-ci<=?" "char-ci<?" "char-ci=?" "char-ci>=?" "char-ci>?"
                  "char-downcase" "char-lower-case?" "char-numeric?"
                  "char-ready?" "char-upcase" "char-upper-case?"
                  "char-whitespace?" "char<=?" "char<?" "char=?" "char>=?"
                  "char>?" "char?" "close-input-port" "close-output-port"
                  "complex?" "cons" "cos" "current-input-port"
                  "current-output-port" "denominator" "display" "dynamic-wind"
                  "eof-object?" "eq?" "equal?" "eqv?" "eval" "even?"
                  "exact->inexact" "exact?" "exp" "expt" "floor" "for-each"
                  "force" "gcd" "imag-part" "inexact->exact" "inexact?"
                  "input-port?" "integer->char" "integer?"
                  "interaction-environment" "lcm" "length" "list"
                  "list->string" "list->vector" "list-ref" "list-tail" "list?"
                  "load" "log" "magnitude" "make-polar" "make-rectangular"
                  "make-string" "make-vector" "map" "max" "member" "memq" "memv"
                  "min" "modulo" "negative?" "newline" "not" "null-environment"
                  "null?" "number->string" "number?" "numerator" "odd?"
                  "open-input-file" "open-output-file" "output-port?" "pair?"
                  "peek-char" "port?" "positive?" "procedure?" "quotient"
                  "rational?" "rationalize" "read" "read-char" "real-part"
                  "real?" "remainder" "reverse" "round"
                  "scheme-report-environment" "set-car!" "set-cdr!" "sin" "sqrt"
                  "string" "string->list" "string->number" "string->symbol"
                  "string-append" "string-ci<=?" "string-ci<?" "string-ci=?"
                  "string-ci>=?" "string-ci>?" "string-copy" "string-fill!"
                  "string-length" "string-ref" "string-set!" "string<=?"
                  "string<?" "string=?" "string>=?" "string>?" "string?"
                  "substring" "symbol->string" "symbol?" "tan" "transcript-off"
                  "transcript-on" "truncate" "values" "vector" "vector->list"
                  "vector-fill!" "vector-length" "vector-ref" "vector-set!"
                  "vector?" "with-input-from-file" "with-output-to-file"
                  "write" "write-char" "zero?"))
      (valid-name "[\\w!$%&*+,/:<=>?@^~|-]+"))
  (defstate scheme-lexer :root ()
    ;; Comments to end of line
    (";.*$" :token :comment.single)
    ;; Multi-line comments
    ("#\\|" :token :comment.multiline
            :state :multiline-comment)
    ;; Commented form (entire sexpr followng)
    ("#;\\s*\\(" :token :comment
                 :state :commented-form)
    ;; Signifies that the program text that follows is written with the lexical
    ;; and datum syntax described in r6rs
    ("#!r6rs" :token :comment)
    ;; whitespace - usually not relevant
    ("\\s+" :token :text)
    ;; Numbers
    ("-?\\d+\\.\\d+" :token :number.float)
    ("-?\\d+" :token :number.integer)
    ;; Strings, symbols and characters
    ("\"(\\\\\\\\|\\\\\"|[^\"])*\"" :token :string)
    (((format nil "'~A" valid-name)) :token :string.symbol)
    ("#\\\\([()/'\\\"._!§$%& ?=+-]|[a-zA-Z0-9]+)" :token :string.char)
    ;; Constants
    ("(#t|#f)" :token :name.constant)
    ;; Special operators
    ("('|#|`|,@|,|\\.)" :token :operator)
    ;; Highlight keywords
    (((words (mapcar #'ppcre:quote-meta-chars keywords) :suffix " "))
     :token :keyword)
    ;; First variable in a quoted string
    (((format nil "(?<='\\()~A" valid-name)) :token :name.variable)
    (((format nil "(?<=#\\()~A" valid-name)) :token :name.variable)
    ;; Highlight builtins
    (((words (mapcar #'ppcre:quote-meta-chars builtins)
             :prefix "(?<=\\()"
             :suffix " "))
     :token :name.builtin)
    ;; Remaining functions
    (((format nil "(?<=\\()~A" valid-name)) :token :name.function)
    ;; Find the remaining variables
    (valid-name :token :name.variable)
    ;; The famous parentheses!
    ("(\\(|\\))" :token :punctuation)
    ("(\\[|\\])" :token :punctuation))

  (defstate scheme-lexer :multiline-comment ()
    ("#\\|" :token :comment.multiline
            :state :multiline-comment)
    ("\\|#" :token :comment.multiline
            :state :pop!)
    ("[^|#]+" :token :comment.multiline)
    ("[|#]" :token :comment.multiline))

  (defstate scheme-lexer :commented-form ()
    ("\\(" :token :comment
           :state :commented-form)
    ("\\)" :token :comment
           :state :pop!)
    ("[^()]" :token :comment)))
