(defpackage :crylic/renderers/html
  (:use :cl :crylic/renderer :crylic/tokens)
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
  (let ((text (regex-replace-all "[&<>'\"]"
                                 (token-text token)
                                 (lambda (match &rest ignored)
                                   (declare (ignore ignored))
                                   (ecase (char match 0)
                                     (#\& "&amp;")
                                     (#\< "&lt;")
                                     (#\> "&gt;")
                                     (#\" "&quot;")
                                     (#\' "&#39;")))
                                 :simple-calls t))
        (short-name (token-short-name token)))
    (if (zerop (length short-name))
        (format stream "~A" text)
        (format stream "<span class=\"~A\">~A</span>"
                short-name
                text))))

(defmethod render ((renderer html-renderer) (tokens list) (stream stream))
  (when (wrap-p renderer)
    (format stream "<div class=\"~A\"><pre>" (wrapper-css-class renderer)))
  (dolist (token tokens)
    (render-span token stream))
  (when (wrap-p renderer)
    (format stream "</pre></div>~%")))
