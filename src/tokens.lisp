;;;; This file defines all token types to be generated during the lexing
;;;; process. The package defined here does not use the COMMON-LISP package, as
;;;; its sole purpose is to contain token types.

(cl:defpackage :crylic/tokens
  (:use :crylic/lexer))
(cl:in-package :crylic/tokens)

(define-tokens
  (text ""
        (whitespace "w"))

  (error "err")
  (other "x")

  (keyword "k"
           (constant "kc")
           (declaration "kd")
           (namespace "kn")
           (pseudo "kp")
           (reserved "kr")
           (type "kt")
           (variable "kv"))

  (name "n"
        (attribute "na")
        (builtin "nb"
                 (pseudo "bp"))
        (class "nc")
        (constant "no")
        (decorator "nd")
        (entity "ni")
        (exception "ne")
        (function "nf")
        (property "py")
        (label "nl")
        (namespace "nn")
        (other "nx")
        (tag "nt")
        (variable "nv"
                  (class "vc")
                  (global "vg")
                  (instance "vi")))

  (literal "l"
           (date "ld")
           (string "s"
                   (backtick "sb")
                   (char "sc")
                   (doc "sd")
                   (double "s2")
                   (escape "se")
                   (heredoc "sh")
                   (interpol "si")
                   (other "sx")
                   (regex "sr")
                   (single "s1")
                   (symbol "ss"))

           (number "m"
                   (float "mf")
                   (hex "mh")
                   (integer "mi")
                   (long "il"))
           (oct "mo")
           (bin "mb")
           (other "mx"))

  (operator "o"
            (word "ow"))

  (punctuation "p"
               (indicator "pi"))

  (comment "c"
           (doc "cd")
           (multiline "cm")
           (preproc "cp")
           (single "c1")
           (special "cs"))

  (generic "g"
           (deleted "gd")
           (emph "ge")
           (error "gr")
           (heading "gh")
           (inserted "gi")
           (output "go")
           (prompt "gp")
           (strong "gs")
           (subheading "gu")
           (traceback "gt")
           (lineno "gl")))

;; The EOF token type is special, in that it should never be instantiated. It
;; can be used by lexers to signify processing should have ended.
(cl:export 'eof)
(cl:defclass eof (token)
  ()
  (:default-initargs
   :short-name (cl:error "EOF-TOKEN should never be instantiated!")))
