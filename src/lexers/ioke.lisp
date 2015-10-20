(defpackage :crylic/lexers/ioke
  (:use :cl :crylic/regex-lexer)
  (:export #:ioke-lexer))
(in-package :crylic/lexers/ioke)

(define-regex-lexer ioke-lexer () ()
  (:title "Ioke")
  (:description "Lexer for Ioke source code.")
  (:tags "ioke" "ik")
  (:filenames "*.ik")
  (:mime-types "text/x-iokesrc"))

(defstate ioke-lexer :interpolatable-text ()
  (((string+
     "(\\\\b|\\\\e|\\\\t|\\\\n|\\\\f|\\\\r|\\\\\"|\\\\\\\\|\\\\#|\\\\\\Z|"
     "\\\\u[0-9a-fA-F]{1,4}|\\\\[0-3]?[0-7]?[0-7])"))
   :string.escape)
  ("#\\{" :punctuation (state :text-interpolation-root)))

(defstate ioke-lexer :text ()
  ("(?<!\\\\)\"" :string (state :pop!))
  (:include :interpolatable-text)
  ("[^\"]" :string))

(defstate ioke-lexer :documentation ()
  ("(?<!\\\\)\"" :string.doc (state :pop!))
  (:include :interpolatable-text)
  ("[^\"]" :string.doc))

(defstate ioke-lexer :text-interpolation-root ()
  ("\\}" :punctuation (state :pop!))
  (:include :root))

(defstate ioke-lexer :slash-regexp ()
  ("(?<!\\\\)/[oxpniums]*" :string.regex (state :pop!))
  (:include :interpolatable-text)
  ("\\\\/" :string.regex)
  ("[^/]" :string.regex))

(defstate ioke-lexer :square-regexp ()
  ("(?<!\\\\)/[oxpniums]*" :string.regex (state :pop!))
  (:include :interpolatable-text)
  ("\\\\]" :string.regex)
  ("[^\\]]" :string.regex))

(defstate ioke-lexer :square-text ()
  ("(?<!\\\\)]" :string (state :pop!))
  (:include :interpolatable-text)
  ("[^\\]]" :string))

(defstate ioke-lexer :root ()
  ("\\n" :text)
  ("\\s+" :text)

  ;; Comments
  (";(.*?)\\n" :comment)
  ("\\A#!(.*?)\\n" :comment)

  ;; Regexps
  ("#/" :string.regex (state :slash-regexp))
  ("#r\\[" :string.regex (state :square-regexp))

  ;; Symbols
  (":[\\w!:?]+" :string.symbol)
  ("[\\w!:?]+:(?![\\w!?])" :string.other)
  (":\"(\\\\\\\\|\\\\\"|[^\"])*\"" :string.symbol)

  ;; Documentation
  (((string+
     "((?<=fn\\()|(?<=fnx\\()|(?<=method\\()|(?<=macro\\()|(?<=lecro\\()"
     "|(?<=syntax\\()|(?<=dmacro\\()|(?<=dlecro\\()|(?<=dlecrox\\()"
     "|(?<=dsyntax\\())\\s*\""))
   :string.doc (state :documentation))

  ;; Text
  ("\"" :string (state :text))
  ("#\\[" :string (state :square-text))

  ;; Mimic
  ("\\w[\\w!:?]+(?=\\s*=.*mimic\\s)" :name.entity)

  ;; Assignment
  ("[a-zA-Z_][\\w!:?]*(?=[\\s]*[+*/-]?=[^=].*($|\\.))" :name.variable)

  ;; Keywords
  (((string+ "(break|cond|continue|do|ensure|for|for:dict|for:set|if|let|"
             "loop|p:for|p:for:dict|p:for:set|return|unless|until|while|"
             "with)(?![\\w!:?])"))
   :keyword.reserved)

  ;; Origin
  ("(eval|mimic|print|println)(?![\\w!:?])" :keyword)

  ;; Base
  (((string+ "(cell\\?|cellNames|cellOwner\\?|cellOwner|cells|cell|"
             "documentation|hash|identity|mimic|removeCell\\!|undefineCell\\!)"
             "(?![\\w!:?])"))
   :keyword)

  ;; Ground
  ("(stackTraceAsText)(?![\\w!:?])" :keyword)

  ;; DefaultBehaviour Literals
  ("(dict|list|message|set)(?![\\w!:?])" :keyword.reserved)

  ;; DefaultBehaviour Case
  (((string+ "(case|case:and|case:else|case:nand|case:nor|case:not|case:or|"
             "case:otherwise|case:xor)(?![\\w!:?])"))
   :keyword.reserved)

  ;; DefaultBehaviour Reflection
  (((string+
     "(asText|become\\!|derive|freeze\\!|frozen\\?|in\\?|is\\?|kind\\?|"
     "mimic\\!|mimics|mimics\\?|prependMimic\\!|removeAllMimics\\!|"
     "removeMimic\\!|same\\?|send|thaw\\!|uniqueHexId)"
     "(?![\\w!:?])"))
   :keyword)

  ;; DefaultBehaviour Aspects
  ("(after|around|before)(?![\\w!:?])" :keyword.reserved)

  ;; DefaultBehaviour
  (((string+ "(kind|cellDescriptionDict|cellSummary|genSym|inspect|notice)"
             "(?![\\w!:?])"))
   :keyword)
  ("(use|destructuring)" :keyword.reserved)

  ;; DefaultBehavior BaseBehavior
  (((string+ "(cell\\?|cellOwner\\?|cellOwner|cellNames|cells|cell|"
             "documentation|identity|removeCell!|undefineCell)"
             "(?![\\w!:?])"))
   :keyword)

  ;; DefaultBehavior Internal
  (((string+ "(internal:compositeRegexp|internal:concatenateText|"
             "internal:createDecimal|internal:createNumber|"
             "internal:createRegexp|internal:createText)"
             "(?![\\w!:?])"))
   :keyword.reserved)

  ;; DefaultBehaviour Conditions
  (((string+ "(availableRestarts|bind|error\\!|findRestart|handle|"
             "invokeRestart|rescue|restart|signal\\!|warn\\!)"
             "(?![\\w!:?])"))
   :keyword.reserved)

  ;; Constants
  ("(nil|false|true)(?![\\w!:?])" :name.constant)

  ;; Names
  (((string+ "(Arity|Base|Call|Condition|DateTime|Aspects|Pointcut|"
             "Assignment|BaseBehavior|Boolean|Case|AndCombiner|Else|"
             "NAndCombiner|NOrCombiner|NotCombiner|OrCombiner|XOrCombiner|"
             "Conditions|Definitions|FlowControl|Internal|Literals|"
             "Reflection|DefaultMacro|DefaultMethod|DefaultSyntax|Dict|"
             "FileSystem|Ground|Handler|Hook|IO|IokeGround|Struct|"
             "LexicalBlock|LexicalMacro|List|Message|Method|Mixins|"
             "NativeMethod|Number|Origin|Pair|Range|Reflector|Regexp Match|"
             "Regexp|Rescue|Restart|Runtime|Sequence|Set|Symbol|"
             "System|Text|Tuple)(?![\\w!:?])"))
   :name.builtin)

  ;; Functions
  (((string+ "(generateMatchMethod|aliasMethod|"
             #\GREEK_SMALL_LETTER_LAMDA  "|" #\LATIN_SMALL_LETTER_TURNED_Y
             "|fnx|fn|method|"
             "dmacro|dlecro|syntax|macro|dlecrox|lecrox|lecro|syntax)"
             "(?![\\w!:?])"))
   :name.function)

  ;; Numbers
  ("-?0[xX][0-9a-fA-F]+" :number.hex)
  ("-?(\\d+\\.?\\d*|\\d*\\.\\d+)([eE][+-]?[0-9]+)?" :number.float)
  ("-?\\d+" :number.integer)

  ("#\\(" :punctuation)

  ;; Operators
  (((string+ "(&&>>|\\|\\|>>|\\*\\*>>|:::|::|\\.\\.\\.|===|\\*\\*>|\\*\\*=|&&>|&&=|"
             "\\|\\|>|\\|\\|=|\\->>|\\+>>|!>>|<>>>|<>>|&>>|%>>|#>>|@>>|/>>|\\*>>|"
             "\\?>>|\\|>>|\\^>>|~>>|\\$>>|=>>|<<=|>>=|<=>|<\\->|=~|!~|=>|\\+\\+|"
             "\\-\\-|<=|>=|==|!=|&&|\\.\\.|\\+=|\\-=|\\*=|\\/=|%=|&=|\\^=|\\|=|<\\-|"
             "\\+>|!>|<>|&>|%>|#>|\\@>|\\/>|\\*>|\\?>|\\|>|\\^>|~>|\\$>|<\\->|\\->|"
             "<<|>>|\\*\\*|\\?\\||\\?&|\\|\\||>|<|\\*|\\/|%|\\+|\\-|&|\\^|\\||=|\\$|!|~|"
             "\\\\|#|" #\NOT_EQUAL_TO "|" #\RING_OPERATOR "|"
             #\ELEMENT_OF "|" #\NOT_AN_ELEMENT_OF ")"))
   :operator)
  ("(and|nand|or|xor|nor|return|import)(?![\\w!?])" :operator)

  ;; Punctuation
  ("(\\`\\`|\\`|\\'\\'|\\'|\\.|\\,|@@|@|\\[|\\]|\\(|\\)|\\{|\\})" :punctuation)

  ;; Kinds
  ("[A-Z][\\w!:?]*" :name.class)

  ;; Default cellnames
  ("[a-z_][\\w!:?]*" :name))
