(defpackage :crylic/renderer
  (:use :cl)
  (:export #:renderer
           #:title
           #:description

           #:*renderers*
           #:define-renderer
           #:render

           #:render-to-string))
(in-package :crylic/renderer)

(defvar *renderers* ())

(defclass renderer ()
  ((%title :initarg :title
           :initform nil
           :reader title)
   (%description :initarg :description
                 :initform nil
                 :reader description)))

(defmacro define-renderer (name superclasses direct-slots &rest options)
  `(progn
     (defclass ,name ,superclasses ,direct-slots ,@options)
     (pushnew ',name *renderers*)))

(defgeneric render (renderer tokens stream)
  (:documentation "Render the list of TOKENS to STREAM using RENDERER."))

(defun render-to-string (renderer tokens)
  (with-output-to-string (out)
    (render renderer tokens out)))
