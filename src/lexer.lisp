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
           #:lex

           #:token
           #:short-name
           #:text

           #:deftoken
           #:define-tokens))
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

;;; A token is a simple unit for denoting a piece of text is of a certain type.
;;; It has a slot, TEXT, containing the text enclosed by the token. It also has
;;; a slot SHORT-NAME, containing a short version of the token type's name.

(defclass token ()
  ((short-name :initarg :short-name
               :reader short-name)
   (text :initarg :text
         :initform nil
         :reader text)))

(defmacro deftoken (name short-name &body sub-tokens)
  "Convenience macro for defining token classes. NAME will be the name of the
new class, SHORT-NAME will be the default value of the SHORT-NAME slot of every
instance (after downcasing) and each item in SUB-TOKENS should look like a
valid invocation of DEFTOKEN, except without the DEFTOKEN.

The top-level class will be named after NAME, and every sub-token will have as
a class name NAME/SUB-TOKEN-NAME."
  `(progn
     (export ',name)
     (defclass ,name (token) ()
       (:default-initargs :short-name ,(string-downcase short-name)))
     ,@(loop for sub-token in sub-tokens
             collect `(deftoken ,(intern (format nil "~A/~A" name (first sub-token)))
                          ,(second sub-token)
                        ,@(cddr sub-token)))))

(defmacro define-tokens (&body tokens)
  `(progn ,@(loop for token in tokens
                  collect (cons 'deftoken token))))
