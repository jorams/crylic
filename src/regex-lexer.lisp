(defpackage :crylic/regex-lexer
  (:use :cl :crylic/lexer)
  (:export #:regex-lexer
           #:defstate))
(in-package :crylic/regex-lexer)

(defclass regex-lexer () ()
  (:metaclass lexer-class))

(defgeneric %process (lexer state))
(defgeneric process (lexer state))

(defvar *input* "")
(defvar *tokens* ())

;; During the matching process, whenever a regex matches at the index
;; *position*, appropriate actions are carried out and *position* is set to the
;; end of the match.
(defvar *position* 0)

(defmethod lex ((lexer regex-lexer) (text string))
  (let ((*input* text)
        (*tokens* ())
        (*position* 0))
    (process lexer :root)
    (reverse *tokens*)))

(defun capture-token (type capture-start capture-end)
  "This function is responsible for collecting new tokens."
  (unless (= capture-start capture-end)
    (push (cons type
                (subseq *input*
                        capture-start
                        capture-end))
          *tokens*)))

(defun progress-token (class start end)
  "Capture a token and update capture positions."
  (capture-token class start end)
  ;; Update the capture to point to the new match
  (setf *position* end))

(defun progress-groups (groups reg-start reg-end)
  "Capture a token for every matched group and update capture positions."
  (loop for rstart across reg-start
        for rend across reg-end
        for token in groups
        do (capture-token token rstart rend)
           (setf *position* rend)))

(defun enter-state (lexer name)
  "Continue processing input using the new state."
  (if (eq name :pop!)
      (throw :pop! t)
      (process lexer name)))


(defun try-progress (lexer regex instructions)
  "Try to match REGEX and, if it matches, move forward by collecting tokens
and/or entering a new state."
  (multiple-value-bind (start end reg-start reg-end)
      (ppcre:scan regex *input* :start *position*)
    (when (and start end (= start *position*))
      ;; Update the capture end to point to the previous match end, and the
      ;; match end to point to the newly found match end.
      (dolist (instruction instructions)
        (destructuring-bind (operator argument) instruction
          (case operator
            (:token (progress-token argument start end))
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
             (:state :pop!))))))

(defmethod process ((lexer regex-lexer) state)
  (catch :pop!
    (loop (catch :restart
            (%process lexer state)))))
