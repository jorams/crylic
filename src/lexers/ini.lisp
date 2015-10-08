(defpackage :crylic/lexers/ini
  (:use :cl :crylic/lexer :crylic/regex-lexer)
  (:export #:ini-lexer))
(in-package :crylic/lexers/ini)

(define-lexer ini-lexer (regex-lexer) ()
  (:default-initargs :title "INI"
                     :description "The INI configuration format"
                     :tags (list "ini")
                     :filenames (list "*.ini" "*.INI" "*.gitconfig")
                     :mime-types "text/x-ini"))

(defstate ini-lexer :basic
  ("[;#].*?\\n" (:token 'crylic/tokens:comment))
  ("\\s+"       (:token 'crylic/tokens:text))
  ("\\\\\\n"    (:token 'crylic/tokens:literal/string/escape)))

(defstate ini-lexer :root
  (:mixin :basic)
  ("([\\w.]+)(\\s*)(=)"
   (:state :value
    :groups '(crylic/tokens:name/property
              crylic/tokens:text
              crylic/tokens:punctuation)))
  ("\\[.*?\\]" (:token 'crylic/tokens:name/namespace)))

(defstate ini-lexer :value
  ("\\n" (:token 'crylic/tokens:text :state :pop!))
  (:mixin :basic)
  ("\"" (:token 'crylic/tokens:literal/string
         :state :dq))
  ("'.*?'" (:token 'crylic/tokens:literal/string))
  (:mixin :esc-str)
  ("[^\\\\\\n]+" (:token 'crylic/tokens:literal/string)))

(defstate ini-lexer :dq
  ("\"" (:token 'crylic/tokens:literal/string
         :state :pop!))
  (:mixin :esc-str)
  (("[^\\\\\"]+" :multi-line-mode t)
   (:token 'crylic/tokens:literal/string)))

(defstate ini-lexer :esc-str
  (("\\\\." :multi-line-mode t)
   (:token 'crylic/tokens:literal/string/escape)))
