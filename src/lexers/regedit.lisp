(defpackage :crylic/lexers/regedit
  (:use :cl :crylic/lexer :crylic/regex-lexer)
  (:export #:regedit-lexer))
(in-package :crylic/lexers/regedit)

(define-lexer regedit-lexer (regex-lexer) ()
  (:title "reg")
  (:description "Lexer for Windows Registry files produced by regedit.")
  (:tags "registry")
  (:filenames "*.reg")
  (:mime-types "text/x-windows-registry"))

(defstate regedit-lexer :root
  ("Windows Registry Editor.*" (:token :text))
  ("\\s+" (:token :text))
  ("[;#].*" (:token :comment.single))
  ("(\\[)(-?)(HKEY_[A-Z_]+)(.*?\\])$"
   (:groups (:keyword :operator :name.builtin :keyword)))
  ;; String keys, which obey somewhat normal escaping
  ("(\"(?:\\\\\"|\\\\\\\\|[^\"])+\")([ \\t]*)(=)([ \\t]*)"
   (:groups (:name.attribute :text :operator :text))
   (:state :value))
  ;; Bare keys (includes @)
  ("(.*?)([ \\t]*)(=)([ \\t]*)"
   (:groups (:name.attribute :text :operator :text))
   (:state :value)))

(defstate regedit-lexer :value
  ("-" (:token :operator) (:state :pop!)) ; delete value
  ("(dword|hex(?:\\([0-9a-fA-F]\\))?)(:)([0-9a-fA-F,]+)"
   (:groups (:name.variable :punctuation :number))
   (:state :pop!))
  ;; As far as I know, .reg files do not support line continuation.
  (".+" (:token :string) (:state :pop!))
  ("" (:state :pop!)))
