(defpackage :crylic/lexer
  (:use :closer-common-lisp)
  (:export #:lexer-class
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
;;; extensions, etc. These are properties of the class, so they are implemented
;;; in a metaclass.

(defclass lexer-class (standard-class)
  ((%title :initarg :title
           :initform ()
           :reader title)
   (%description :initarg :description
                 :initform ()
                 :reader description)
   (%tags :initarg :tags
          :initform ()
          :reader tags)
   (%filenames :initarg :filenames
               :initform ()
               :reader filenames)
   (%mime-types :initarg :mime-types
                :initform ()
                :reader mime-types)))

(defmethod validate-superclass ((class lexer-class)
                                (super-class standard-class))
  t)

(defmethod ensure-class-using-class :after ((class lexer-class)
                                            name
                                            &key &allow-other-keys)
  "Sets the title and description slots of the class, which should be a list, to
just their first element. This allows their definition in a DEFCLASS form to
just be (:title \"some-title\"), instead of (:title . \"some-title\")."
  (setf (slot-value class '%title)       (first (title class))
        (slot-value class '%description) (first (description class))))

(defmacro define-lexer (name superclasses direct-slots &rest options)
  `(progn
     (defclass ,name ,superclasses
       ,direct-slots
       (:metaclass lexer-class)
       ,@options)
     (pushnew ',name *lexers*)))

;;; The simple construct that performs lexing is the LEX generic function. It
;;; takes a lexer and the input text as input, and generates the list of tokens
;;; as output.

(defgeneric lex (lexer text)
  (:documentation "Lex the given input TEXT using LEXER. The result of this
operation is a list of TOKENs."))
