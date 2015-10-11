(defpackage :crylic/filters/split-multiline
  (:use :cl :split-sequence :crylic/filter)
  (:export #:split-multiline-filter))
(in-package :crylic/filters/split-multiline)

(define-filter split-multiline-filter () ())

(defmethod filter ((filter split-multiline-filter) (tokens list)
                   &key &allow-other-keys)
  (loop for (type . text) in tokens
        append (loop for (part . rest) on (split-sequence #\Newline text)
                     ;; KLUDGE: This weird UNLESS clause exists to emulate
                     ;; Pygments' behaviour.
                     unless (and (zerop (length part))
                                 (not rest))
                       collect (cons type part)
                     when rest
                       ;; Every part but the last ended with a newline
                       collect (cons :text (string #\Newline)))))
