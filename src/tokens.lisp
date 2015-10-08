(defpackage :crylic/tokens
  (:use :cl)
  (:export #:short-name
           #:text))
(in-package :crylic/tokens)

;;; A token is a simple unit for denoting a piece of text is of a certain type.
;;; It's stored as a cons cell of which the CAR is the token's keyword name and
;;; the CDR is a string representing the token's textual content.

(defvar *tokens* ())
(defvar *token-short-names* (make-hash-table))

(defun short-name (token)
  (gethash (if (consp token)
               (car token)
               token)
           *token-short-names*))

(defun text (token)
  (cdr token))

(defmacro define-token-type (name short-name)
  `(progn
     (pushnew ',name *tokens*)
     (setf (gethash ',name *token-short-names*)
           ,short-name)))

(defmacro define-token-types (&body tokens)
  `(progn ,@(loop for token in tokens
                  collect (cons 'define-token-type token))))

;;; Concrete token-type definitions -------------------------------------------

;; The EOF token type is special, in that it should never actually be used. It
;; can be used by lexers to signify processing should have ended.
(define-token-type :eof "ERROR")

(define-token-types
  (:token                         "")

  (:text                          "")
  (:whitespace                    "w")
  (:escape                        "esc")
  (:error                         "err")
  (:other                         "x")

  (:keyword                       "k")
  (:keyword.constant              "kc")
  (:keyword.declaration           "kd")
  (:keyword.namespace             "kn")
  (:keyword.pseudo                "kp")
  (:keyword.reserved              "kr")
  (:keyword.type                  "kt")

  (:name                          "n")
  (:name.attribute                "na")
  (:name.builtin                  "nb")
  (:name.builtin.pseudo           "bp")
  (:name.class                    "nc")
  (:name.constant                 "no")
  (:name.decorator                "nd")
  (:name.entity                   "ni")
  (:name.exception                "ne")
  (:name.function                 "nf")
  (:name.property                 "py")
  (:name.label                    "nl")
  (:name.namespace                "nn")
  (:name.other                    "nx")
  (:name.tag                      "nt")
  (:name.variable                 "nv")
  (:name.variable.class           "vc")
  (:name.variable.global          "vg")
  (:name.variable.instance        "vi")

  (:literal                       "l")
  (:literal.date                  "ld")

  (:string                        "s")
  (:string.backtick               "sb")
  (:string.char                   "sc")
  (:string.doc                    "sd")
  (:string.double                 "s2")
  (:string.escape                 "se")
  (:string.heredoc                "sh")
  (:string.interpol               "si")
  (:string.other                  "sx")
  (:string.regex                  "sr")
  (:string.single                 "s1")
  (:string.symbol                 "ss")

  (:number                        "m")
  (:number.bin                    "mb")
  (:number.float                  "mf")
  (:number.hex                    "mh")
  (:number.integer                "mi")
  (:number.integer.long           "il")
  (:number.oct                    "mo")

  (:operator                      "o")
  (:operator.word                 "ow")

  (:punctuation                   "p")

  (:comment                       "c")
  (:comment.hashbang              "ch")
  (:comment.multiline             "cm")
  (:comment.preproc               "cp")
  (:comment.single                "c1")
  (:comment.special               "cs")

  (:generic                       "g")
  (:generic.deleted               "gd")
  (:generic.emph                  "ge")
  (:generic.error                 "gr")
  (:generic.heading               "gh")
  (:generic.inserted              "gi")
  (:generic.output                "go")
  (:generic.prompt                "gp")
  (:generic.strong                "gs")
  (:generic.subheading            "gu")
  (:generic.traceback             "gt"))
