;;;; This file implements modified versions of some regular expression
;;;; constructs from CL-PPCRE. These versions have been adjusted to work more
;;;; like Python's regular expressions do.
;;;;
;;;; Specifically, this file implements modified anchors, lookbehind and word
;;;; boundaries that don't act as if the part of the string before a specified
;;;; :START doesn't exist. This means that, for example, a ^ only matches if
;;;; the position is actually at the start of the full string or just after a
;;;; newline. It doesn't take into account the specified :START to determine
;;;; this.
;;;;
;;;; These modified constructs have been implemented in a way that doesn't
;;;; interfere with normal functionality. They don't have a syntactic
;;;; equivalent in a regex string and their keyword name is prefixed with
;;;; CRYLIC- to distinguish them from the normal version.

(defpackage :crylic/cl-ppcre-extensions
  (:use :cl)
  (:export #:create-scanner))
(in-package :crylic/cl-ppcre-extensions)

;;; Classes -------------------------------------------------------------------

(defclass anchor (ppcre::anchor) ())
(defclass lookbehind (ppcre::lookbehind) ())
(defclass word-boundary (ppcre::word-boundary) ())

(defun convert-to-crylic-tree (tree)
  (list :sequence :modeless-start-anchor
        (subst
         :crylic-negative-lookbehind :negative-lookbehind
         (subst
          :crylic-positive-lookbehind :positive-lookbehind
          (subst
           :crylic-non-word-boundary :non-word-boundary
           (subst
            :crylic-word-boundary :word-boundary
            (subst
             :crylic-modeless-start-anchor :modeless-start-anchor
             (subst
              :crylic-start-anchor :start-anchor
              tree))))))))

(in-package :cl-ppcre)

;;; Object instantiation ------------------------------------------------------

(defun crylic/cl-ppcre-extensions::create-scanner
    (regex-string &key case-insensitive-mode
                    multi-line-mode
                    single-line-mode
                    extended-mode
                    destructive)
  (declare #.*standard-optimize-settings*)
  (declare (ignore destructive))
  ;; parse the string into a parse-tree and then call CREATE-SCANNER
  ;; again
  (let* ((*extended-mode-p* extended-mode)
         (quoted-regex-string
           (if *allow-quoting*
               (quote-sections (clean-comments regex-string extended-mode))
               regex-string))
         (*syntax-error-string* (copy-seq quoted-regex-string)))
    ;; wrap the result with :GROUP to avoid infinite loops for
    ;; constant strings
    (create-scanner
     (cons :group
           (list (crylic/cl-ppcre-extensions::convert-to-crylic-tree
                  (parse-string quoted-regex-string))))
     :case-insensitive-mode case-insensitive-mode
     :multi-line-mode multi-line-mode
     :single-line-mode single-line-mode
     :destructive t)))

(defmethod ppcre::convert-simple-parse-tree
    ((parse-tree (eql :crylic-start-anchor)))
  ;; Perl's "^"
  (declare #.*standard-optimize-settings*)
  (declare (special flags))
  (make-instance 'crylic/cl-ppcre-extensions::anchor
                 :startp t
                 :multi-line-p (multi-line-mode-p flags)))

(defmethod ppcre::convert-simple-parse-tree
    ((parse-tree (eql :crylic-modeless-start-anchor)))
  ;; Perl's "\A"
  (declare #.*standard-optimize-settings*)
  (make-instance 'crylic/cl-ppcre-extensions::anchor :startp t))

(defmethod ppcre::convert-simple-parse-tree
    ((parse-tree (eql :crylic-word-boundary)))
  (declare #.*standard-optimize-settings*)
  (make-instance 'crylic/cl-ppcre-extensions::word-boundary :negatedp nil))

(defmethod ppcre::convert-simple-parse-tree
    ((parse-tree (eql :crylic-non-word-boundary)))
  (declare #.*standard-optimize-settings*)
  (make-instance 'crylic/cl-ppcre-extensions::word-boundary :negatedp t))

(defmethod convert-compound-parse-tree
    ((token (eql :crylic-positive-lookbehind)) parse-tree &key)
  "The case for \(:POSITIVE-LOOKBEHIND <regex>)."
  (declare #.*standard-optimize-settings*)
  (declare (special flags accumulate-start-p))
  ;; keep the effect of modifiers local to the enclosed regex and stop
  ;; accumulating into STARTS-WITH
  (setq accumulate-start-p nil)
  (let* ((flags (copy-list flags))
         (regex (convert-aux (second parse-tree)))
         (len (regex-length regex)))
    (declare (special flags))
    ;; lookbehind assertions must be of fixed length
    (unless len
      (signal-syntax-error
       "Variable length look-behind not implemented \(yet): ~S." parse-tree))
    (make-instance 'crylic/cl-ppcre-extensions::lookbehind
                   :regex regex
                   :positivep t
                   :len len)))

(defmethod convert-compound-parse-tree
    ((token (eql :crylic-negative-lookbehind)) parse-tree &key)
  "The case for \(:NEGATIVE-LOOKBEHIND <regex>)."
  (declare #.*standard-optimize-settings*)
  ;; do the same as for positive look-behinds and just switch afterwards
  (let ((regex (convert-compound-parse-tree :crylic-positive-lookbehind
                                            parse-tree)))
    (setf (slot-value regex 'positivep) nil)
    regex))

;;; Anchor implementation -----------------------------------------------------

(defmethod create-matcher-aux ((anchor crylic/cl-ppcre-extensions::anchor)
                               next-fn)
  (declare #.*standard-optimize-settings*)
  (declare (function next-fn))
  (let ((startp (startp anchor))
        (multi-line-p (multi-line-p anchor)))
    (cond ((no-newline-p anchor)
           ;; this must be an end-anchor and it must be modeless, so
           ;; we just have to check whether START-POS equals
           ;; *END-POS*
           (lambda (start-pos)
             (declare (fixnum start-pos))
             (and (= start-pos *end-pos*)
                  (funcall next-fn start-pos))))
          ((and startp multi-line-p)
           ;; a start-anchor in multi-line-mode: check if we're at
           ;; 0 or if the last character was #\Newline
           (lambda (start-pos)
             (declare (fixnum start-pos))
             (and (or (zerop start-pos)
                      (and (<= start-pos *end-pos*)
                           (plusp start-pos)
                           (char= #\Newline
                                  (schar *string* (1- start-pos)))))
                  (funcall next-fn start-pos))))
          (startp
           ;; a start-anchor which is not in multi-line-mode, so just
           ;; check whether we're at 0
           (lambda (start-pos)
             (declare (fixnum start-pos))
             (and (zerop start-pos)
                  (funcall next-fn start-pos))))
          (multi-line-p
           ;; an end-anchor in multi-line-mode: check if we're at
           ;; *END-POS* or if the character we're looking at is
           ;; #\Newline
           (lambda (start-pos)
             (declare (fixnum start-pos))
             (and (or (= start-pos (length *string*))
                      (and (< start-pos (length *string*))
                           (char= #\Newline
                                  (schar *string* start-pos))))
                  (funcall next-fn start-pos))))
          (t
           ;; an end-anchor which is not in multi-line-mode, so just
           ;; check if we're at *END-POS* or if we're looking at
           ;; #\Newline and there's nothing behind it
           (lambda (start-pos)
             (declare (fixnum start-pos))
             (and (or (= start-pos *end-pos*)
                      (and (= start-pos (1- *end-pos*))
                           (char= #\Newline
                                  (schar *string* start-pos))))
                  (funcall next-fn start-pos)))))))

;;; Lookbehind implementation -------------------------------------------------

(defmethod create-matcher-aux
    ((lookbehind crylic/cl-ppcre-extensions::lookbehind) next-fn)
  (declare #.*standard-optimize-settings*)
  (let ((len (len lookbehind))
        ;; create a closure which just checks for the inner regex and
        ;; doesn't care about NEXT-FN
        (test-matcher (create-matcher-aux (regex lookbehind) #'identity)))
    (declare (function next-fn test-matcher)
             (fixnum len))
    (if (positivep lookbehind)
        ;; positive look-behind: check success of inner regex (if we're
        ;; far enough from the start of *STRING*), then call NEXT-FN
        (lambda (start-pos)
          (declare (fixnum start-pos))
          (and (>= start-pos len)
               (funcall test-matcher (- start-pos len))
               (funcall next-fn start-pos)))
        ;; negative look-behind: check failure of inner regex (if we're
        ;; far enough from the start of *STRING*), then call NEXT-FN
        (lambda (start-pos)
          (declare (fixnum start-pos))
          (and (or (< start-pos len)
                   (not (funcall test-matcher (- start-pos len))))
               (funcall next-fn start-pos))))))

;;; Word boundary implementation ----------------------------------------------

(defun crylic/cl-ppcre-extensions::word-boundary-p (start-pos)
  "Check whether START-POS is a word-boundary within *STRING*."
  (declare #.*standard-optimize-settings*)
  (declare (fixnum start-pos))
  (let ((1-start-pos (1- start-pos)))
    ;; either the character before START-POS is a word-constituent and
    ;; the character at START-POS isn't...
    (or (and (or (= start-pos *end-pos*)
                 (and (< start-pos *end-pos*)
                      (not (word-char-p (schar *string* start-pos)))))
             (and (< 1-start-pos *end-pos*)
                  (<= 0 1-start-pos)
                  (word-char-p (schar *string* 1-start-pos))))
        ;; ...or vice versa
        (and (or (zerop start-pos)
                 (and (< 1-start-pos *end-pos*)
                      (<= 0 1-start-pos)
                      (not (word-char-p (schar *string* 1-start-pos)))))
             (and (< start-pos *end-pos*)
                  (word-char-p (schar *string* start-pos)))))))

(defmethod create-matcher-aux
    ((word-boundary crylic/cl-ppcre-extensions::word-boundary) next-fn)
  (declare #.*standard-optimize-settings*)
  (declare (function next-fn))
  (if (negatedp word-boundary)
      (lambda (start-pos)
        (and (not (crylic/cl-ppcre-extensions::word-boundary-p start-pos))
             (funcall next-fn start-pos)))
      (lambda (start-pos)
        (and (crylic/cl-ppcre-extensions::word-boundary-p start-pos)
             (funcall next-fn start-pos)))))
