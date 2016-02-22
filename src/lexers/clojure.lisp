(defpackage :crylic/lexers/clojure
  (:use :cl :crylic/regex-lexer)
  (:export #:clojure-lexer #:clojurescript-lexer))
(in-package :crylic/lexers/clojure)

(define-regex-lexer clojure-lexer () ()
  (:title "Clojure")
  (:description "Lexer for Clojure source code.")
  (:tags "clojure" "clj")
  (:filenames "*.clj")
  (:mime-types "text/x-clojure" "application/x-clojure"))

(define-regex-lexer clojurescript-lexer (clojure-lexer) ()
  (:title "ClojureScript")
  (:description "Lexer for ClojureScript source code.")
  (:tags "clojurescript" "cljs")
  (:filenames "*.cljs")
  (:mime-types "text/x-clojurescript" "application/x-clojurescript"))

(let ((special-forms
        '("." "def" "do" "fn" "if" "let" "new" "quote" "var" "loop"))
      (declarations
        '("def-" "defn" "defn-" "defmacro" "defmulti" "defmethod"
          "defstruct" "defonce" "declare" "definline" "definterface"
          "defprotocol" "defrecord" "deftype" "defproject" "ns"))
      (builtins
        '("*" "+" "-" "->" "/" "<" "<=" "=" "==" ">" ">=" ".."
          "accessor" "agent" "agent-errors" "aget" "alength" "all-ns"
          "alter" "and" "append-child" "apply" "array-map" "aset"
          "aset-boolean" "aset-byte" "aset-char" "aset-double" "aset-float"
          "aset-int" "aset-long" "aset-short" "assert" "assoc" "await"
          "await-for" "bean" "binding" "bit-and" "bit-not" "bit-or"
          "bit-shift-left" "bit-shift-right" "bit-xor" "boolean" "branch?"
          "butlast" "byte" "cast" "char" "children" "class"
          "clear-agent-errors" "comment" "commute" "comp" "comparator"
          "complement" "concat" "conj" "cons" "constantly" "cond" "if-not"
          "construct-proxy" "contains?" "count" "create-ns" "create-struct"
          "cycle" "dec"  "deref" "difference" "disj" "dissoc" "distinct"
          "doall" "doc" "dorun" "doseq" "dosync" "dotimes" "doto"
          "double" "down" "drop" "drop-while" "edit" "end?" "ensure"
          "eval" "every?" "false?" "ffirst" "file-seq" "filter" "find"
          "find-doc" "find-ns" "find-var" "first" "float" "flush" "for"
          "fnseq" "frest" "gensym" "get-proxy-class" "get"
          "hash-map" "hash-set" "identical?" "identity" "if-let" "import"
          "in-ns" "inc" "index" "insert-child" "insert-left" "insert-right"
          "inspect-table" "inspect-tree" "instance?" "int" "interleave"
          "intersection" "into" "into-array" "iterate" "join" "key" "keys"
          "keyword" "keyword?" "last" "lazy-cat" "lazy-cons" "left"
          "lefts" "line-seq" "list*" "list" "load" "load-file"
          "locking" "long" "loop" "macroexpand" "macroexpand-1"
          "make-array" "make-node" "map" "map-invert" "map?" "mapcat"
          "max" "max-key" "memfn" "merge" "merge-with" "meta" "min"
          "min-key" "name" "namespace" "neg?" "new" "newline" "next"
          "nil?" "node" "not" "not-any?" "not-every?" "not=" "ns-imports"
          "ns-interns" "ns-map" "ns-name" "ns-publics" "ns-refers"
          "ns-resolve" "ns-unmap" "nth" "nthrest" "or" "parse" "partial"
          "path" "peek" "pop" "pos?" "pr" "pr-str" "print" "print-str"
          "println" "println-str" "prn" "prn-str" "project" "proxy"
          "proxy-mappings" "quot" "rand" "rand-int" "range" "re-find"
          "re-groups" "re-matcher" "re-matches" "re-pattern" "re-seq"
          "read" "read-line" "reduce" "ref" "ref-set" "refer" "rem"
          "remove" "remove-method" "remove-ns" "rename" "rename-keys"
          "repeat" "replace" "replicate" "resolve" "rest" "resultset-seq"
          "reverse" "rfirst" "right" "rights" "root" "rrest" "rseq"
          "second" "select" "select-keys" "send" "send-off" "seq"
          "seq-zip" "seq?" "set" "short" "slurp" "some" "sort"
          "sort-by" "sorted-map" "sorted-map-by" "sorted-set"
          "special-symbol?" "split-at" "split-with" "str" "string?"
          "struct" "struct-map" "subs" "subvec" "symbol" "symbol?"
          "sync" "take" "take-nth" "take-while" "test" "time" "to-array"
          "to-array-2d" "tree-seq" "true?" "union" "up" "update-proxy"
          "val" "vals" "var-get" "var-set" "var?" "vector" "vector-zip"
          "vector?" "when" "when-first" "when-let" "when-not"
          "with-local-vars" "with-meta" "with-open" "with-out-str"
          "xml-seq" "xml-zip" "zero?" "zipmap" "zipper"))
      (valid-name "(?!#)[\\w!$%*+<=>?/.#-]+"))

  (defstate clojure-lexer :root ()
    ;; the comments - always starting with semicolon
    ;; and going to the end of the line
    (";.*$" :comment.single)

    ;; whitespaces - usually not relevant
    ("[,\\s]+" :text)

    ;; numbers
    ("-?\\d+\\.\\d+" :number.float)
    ("-?\\d+" :number.integer)
    ("0x-?[abcdef\\d]+" :number.hex)

    ;; strings, symbols and characters
    ("\"(\\\\\\\\|\\\\\"|[^\"])*\"" :string)
    (((string+ "'" valid-name)) :string.symbol)
    ("\\\\(.|[a-z]+)" :string.char)

    ;; keywords
    (((string+ "::?#?" valid-name)) :string.symbol)

    ;; special operators
    ("~@|[`\\'#^~&@]" :operator)

    ;; highlight the special forms
    (((words special-forms :suffix " ")) :keyword)

    ;; Technically, only the special forms are 'keywords'. The problem
    ;; is that only treating them as keywords means that things like
    ;; 'defn' and 'ns' need to be highlighted as builtins. This is ugly
    ;; and weird for most styles. So, as a compromise we're going to
    ;; highlight them as Keyword.Declarations.
    (((words declarations :suffix " ")) :keyword.declaration)

    ;; highlight the builtins
    (((words builtins :suffix " ")) :name.builtin)

    ;; the remaining functions
    (((string+ "(?<=\\()" valid-name)) :name.function)

    ;; find the remaining variables
    (valid-name :name.variable)

    ;; Clojure accepts vector notation
    ("(\\[|\\])" :punctuation)

    ;; Clojure accepts map notation
    ("(\\{|\\})" :punctuation)

    ;; the famous parentheses!
    ("(\\(|\\))" :punctuation)))
