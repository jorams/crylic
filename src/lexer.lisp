(defpackage :crylic/lexer
  (:use :closer-common-lisp)
  (:export #:*lexers*
           #:lex
           #:define-lexer-type

           #:define-lexer
           #:lexer
           #:lexer-class
           #:title
           #:description
           #:tags
           #:file-names
           #:mime-types))
(in-package :crylic/lexer)

;;; The first step a piece of input text is going to go through is the lexing
;;; step. During that step a *lexer* is passed over the input text, creating a
;;; list of tokens as it goes.

(defvar *lexers* ())

;;; The simple construct that performs lexing is the LEX generic function. It
;;; takes a lexer and the input text as input, and generates the list of tokens
;;; as output.

(defgeneric lex (lexer text)
  (:documentation "Lex the given input TEXT using LEXER. The result of this
operation is a list of TOKENs."))

(defmacro define-lexer-type (definer
                             (metaclass meta-superclasses meta-slots
                              &rest meta-options)
                             (class superclasses slots
                              &rest options))
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (defclass ,metaclass ,meta-superclasses ,meta-slots ,@meta-options)
     (defmethod validate-superclass ((class ,metaclass)
                                     (super-class standard-class))
       t)
     (defclass ,class ,superclasses ,slots
       (:metaclass ,metaclass)
       ,@options)
     (defmacro ,definer (name superclasses direct-slots &rest options)
       `(progn
          (eval-when (:compile-toplevel :load-toplevel :execute)
            (defclass ,name ,(or superclasses '(,class))
              ,direct-slots
              (:metaclass ,',metaclass)
              ,@options))
          (pushnew ',name *lexers*)))))

;;; Lexers have a couple of basic properties, such as a name, associated file
;;; extensions, etc. These are properties of the class, so they are implemented
;;; in a metaclass.

(define-lexer-type define-lexer
    (lexer-class (standard-class)
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
    (lexer () ()))

(defmethod ensure-class-using-class :after ((class lexer-class)
                                            name
                                            &key &allow-other-keys)
  "Sets the title and description slots of the class, which should be a list, to
just their first element. This allows their definition in a DEFCLASS form to
just be (:title \"some-title\"), instead of (:title . \"some-title\")."
  (setf (slot-value class '%title)       (first (title class))
        (slot-value class '%description) (first (description class))))
