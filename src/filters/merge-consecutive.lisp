(defpackage :crylic/filters/merge-consecutive
  (:use :cl :crylic/filter)
  (:export #:merge-consecutive-filter))
(in-package :crylic/filters/merge-consecutive)

(define-filter merge-consecutive-filter () ())

(defmethod filter ((filter merge-consecutive-filter) (tokens list)
                   &key &allow-other-keys)
  (flet ((combine (type texts)
           (cons type
                 (apply #'concatenate 'string (reverse texts)))))
    (loop with previous-type = nil
          with previous-texts = (list)
          for ((type . text) . rest) on tokens
          if (or (null previous-type)
                 (eq previous-type type))
            ;; We're either at the start or at a token of the same type as the
            ;; previous one
            do (setf previous-type type)
               (push text previous-texts)
          else
            ;; Token type changed, collect and reset
            collect (combine previous-type previous-texts)
            and do (setf previous-type type
                         previous-texts (list text))
          end
          unless rest
            ;; We're at the end, collect the last token
            collect (combine previous-type previous-texts))))
