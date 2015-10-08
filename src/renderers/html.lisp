(defpackage :crylic/renderers/html
  (:use :cl :crylic/renderer)
  (:import-from :crylic/lexer
                #:short-name
                #:text)
  (:import-from :cl-ppcre
                #:regex-replace-all)
  (:export #:html-renderer))
(in-package :crylic/renderers/html)

(define-renderer html-renderer (renderer)
  ((wrap-p :initarg :wrap-p
           :initform t
           :reader wrap-p)
   (wrapper-css-class :initarg :wrapper-css-class
                      :initform "highlight"
                      :reader wrapper-css-class))
  (:default-initargs :title "HTML"
                     :description "Render tokens to HTML"))

(defun render-span (token stream)
  (let ((text (regex-replace-all "[&<>]"
                                 (text token)
                                 (lambda (match)
                                   (case (char match 0)
                                     (#\& "&amp;")
                                     (#\< "&lt;")
                                     (#\> "&gt;")))))
        (short-name (short-name token)))
    (if (zerop (length short-name))
        (format stream "~A" text)
        (format stream "<span class=\"~A\">~A</span>"
                short-name
                text))))

(defmethod render ((renderer html-renderer) (tokens list) (stream stream))
  (when (wrap-p renderer)
    (format stream "<pre class=\"~A\"><code>" (wrapper-css-class renderer)))
  (dolist (token tokens)
    (render-span token stream))
  (when (wrap-p renderer)
    (format stream "</code></pre>~%")))
