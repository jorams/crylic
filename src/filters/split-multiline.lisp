(defpackage :crylic/filters/split-multiline
  (:use :cl :split-sequence :crylic/filter)
  (:export #:split-multiline-filter))
(in-package :crylic/filters/split-multiline)

(define-filter split-multiline-filter () ())

(defmethod filter ((filter split-multiline-filter) (tokens list)
                   &key &allow-other-keys)
  (loop for (type . text) in tokens
        ;; KLUDGE: This weird construct where empty tokens are not kept unless
        ;; they're the first part of a token (meaning the first character is a
        ;; newline), exists only to emulate Pygments. Ideally empty parts would
        ;; just not be kept.
        append (loop for first-p = t then nil
                     for (part . rest) on (split-sequence #\Newline text)
                     unless (and (not first-p)
                                 (zerop (length part)))
                       collect (cons type part)
                     when rest
                       ;; Every part but the last ended with a newline
                       collect (cons :text (string #\Newline)))))
