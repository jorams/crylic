(defpackage :crylic/filter
  (:use :cl)
  (:export #:define-filter
           #:filter))
(in-package :crylic/filter)

(defvar *filters* ())

(defmacro define-filter (name superclasses direct-slots &rest options)
  `(progn
     (defclass ,name ,superclasses ,direct-slots ,@options)
     (pushnew ',name *filters*)))

(defgeneric filter (filter tokens &key &allow-other-keys)
  (:documentation "This generic function takes a filter and a list of tokens,
and returns a new list of tokens."))
