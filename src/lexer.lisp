(defpackage :crylic/lexer
  (:use :cl)
  (:export #:lexer
           #:title
           #:description
           #:tags
           #:file-names
           #:mime-types
           #:state-rules

           #:*lexers*
           #:define-lexer
           #:lex))
(in-package :crylic/lexer)

;;; The first step a piece of input text is going to go through is the lexing
;;; step. During that step a *lexer* is passed over the input text, creating a
;;; list of tokens as it goes.

(defvar *lexers* ())

;;; Lexers have a couple of basic properties, such as a name, associated file
;;; extensions, etc.

(defclass lexer ()
  ((%title :initarg :title
           :initform nil
           :reader title)
   (%description :initarg :description
                 :initform nil
                 :reader description)
   (%tags :initarg :tags
          :initform nil
          :reader tags)
   (%filenames :initarg :filenames
               :initform nil
               :reader filenames)
   (%mime-types :initarg :mime-types
                :initform nil
                :reader mime-types)))

(defmacro define-lexer (name superclasses direct-slots &rest options)
  `(progn
     (defclass ,name ,superclasses ,direct-slots ,@options)
     (pushnew ',name *lexers*)))

;;; The simple construct that performs lexing is the LEX generic function. It
;;; takes a lexer and the input text as input, and generates the list of tokens
;;; as output.

(defgeneric lex (lexer text)
  (:documentation "Lex the given input TEXT using LEXER. The result of this
operation is a list of TOKENs."))
