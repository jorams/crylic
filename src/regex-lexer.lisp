(defpackage :crylic/regex-lexer
  (:use :cl :crylic/lexer)
  (:export #:regex-lexer
           #:defstate))
(in-package :crylic/regex-lexer)

(defclass regex-lexer (lexer) ())

(defgeneric %process (lexer state))
(defgeneric process (lexer state))

(defvar *input* "")
(defvar *tokens* ())

;; During the matching process, whenever a regex matches starting at the index
;; *match-end-position*, *capture-end-position* is set to the value of
;; *match-end-position* and *match-end-position* is set to the index of the end
;; of the match. *capture-end-position* then points to the start of the new
;; match, which is the end of the previous match, and *match-end-position*
;; points to the end of the new match.
;;
;; If the previous match resulted in the same type of token as the current
;; match, nothing else is done. The match is treated as a continuation of the
;; previous match.
;;
;; If, however, the newly-matched token type is not the same as the previous
;; one, a new token is collected for the previous match. This token gets as
;; contents the text between *capture-start-position* and
;; *capture-end-position*. Afterwards *capture-start-position* is set to the
;; previous value of *capture-end-position*.
(defvar *match-end-position* 0)
(defvar *capture-start-position* 0)
(defvar *capture-end-position* 0)
(defvar *last-token-class* nil)

(defmethod lex ((lexer regex-lexer) (text string))
  (let ((*input* text)
        (*tokens* ())
        (*match-end-position* 0)
        (*capture-start-position* 0)
        (*capture-end-position* 0)
        (*last-token-class* nil))
    (process lexer :root)
    (reverse *tokens*)))

(defun capture-token (class capture-start capture-end)
  "This function is responsible for collecting new tokens. It maintains the
*LAST-TOKEN-CLASS* variable in order to join identical consecutive tokens
together."
  (cond
    ((eq class *last-token-class*)
     ;; Do nothing
     nil)
    ((null *last-token-class*)
     (setf *last-token-class* class)
     nil)
    ;; Entered a different token class
    (t
     (prog1 t
       (push (cons *last-token-class*
                   (subseq *input*
                           capture-start
                           capture-end))
             *tokens*)
       (setf *last-token-class* class)))))

(defun progress-token (class)
  "Capture a token and update capture positions."
  (when (capture-token class *capture-start-position* *capture-end-position*)
    ;; Update the capture to point to the new match
    (setf *capture-start-position* *capture-end-position*)))

(defun progress-groups (groups reg-start reg-end)
  "Capture a token for every matched group and update capture positions."
  (loop for rstart across reg-start
        for rend across reg-end
        for token in groups
        do (when (capture-token token *capture-start-position* rstart)
             (setf *capture-start-position* rstart))))

(defun enter-state (lexer name)
  "Continue processing input using the new state."
  (if (eq name :pop!)
      (throw :pop! t)
      (process lexer name)))


(defun try-progress (lexer regex instructions)
  "Try to match REGEX and, if it matches, move forward by collecting tokens
and/or entering a new state."
  (multiple-value-bind (start end reg-start reg-end)
      (ppcre:scan regex *input* :start *match-end-position*)
    (when (and start end)
      ;; Update the capture end to point to the previous match end, and the
      ;; match end to point to the newly found match end.
      (setf *capture-end-position* *match-end-position*
            *match-end-position* end)
      (dolist (instruction instructions)
        (destructuring-bind (operator argument) instruction
          (case operator
            (:token (progress-token argument))
            (:groups (progress-groups argument reg-start reg-end))
            (:state (enter-state lexer argument))
            (t (funcall operator argument)))))
      (throw :restart t))))

(defmacro %rule (lexer-sym pattern &body instructions)
  (let* ((regex (format nil "^~A"
                        (if (consp pattern)
                            (first pattern)
                            pattern)))
         (scanner `(ppcre:create-scanner
                    ,@(if (consp pattern)
                          (cons regex (cdr pattern))
                          (list regex))
                    :single-line-mode nil
                    :multi-line-mode t)))
    (unless (null instructions)
      `(try-progress ,lexer-sym ,scanner ',instructions))))

(defmacro defstate (lexer name &body rules)
  (let* ((lexer-sym (gensym))
         (state-sym (gensym))
         (rules (loop for rule in rules
                      if (eq :mixin (first rule))
                        collect `(%process ,lexer-sym ,(second rule))
                      else
                        collect (append `(%rule ,lexer-sym)
                                        rule)
                      end)))
    `(defmethod %process ((,lexer-sym ,lexer) (,state-sym (eql ,name)))
       (or ,@rules
           (%rule ,lexer-sym "$"
             (:token :eof)
             (:state :pop!))))))

(defmethod process ((lexer regex-lexer) state)
  (catch :pop!
    (loop (catch :restart
            (%process lexer state)))))
