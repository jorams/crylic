(defpackage :crylic/regex-lexer
  (:use :cl :crylic/lexer)
  (:export #:regex-lexer
           #:defstate
           ;; Utilities
           #:words))
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
        do (when (and rstart rend)
             (capture-token token rstart rend)
             (setf *position* rend))))

(defun enter-state (lexer name)
  "Continue processing input using the new state."
  (let ((options (when (consp name) (rest name)))
        (name (if (consp name)
                  (first name)
                  name)))
    (if (eq name :pop!)
        ;; the pop! count is specified here as the number of pops left. Since
        ;; one pop is being executed here, the option should immediately be
        ;; decremented.
        (throw :pop! (or (when (first options)
                           (1- (first options)))
                         0))
        (process lexer name))))


(defun try-progress (lexer regex instructions)
  "Try to match REGEX and, if it matches, move forward by collecting tokens
and/or entering a new state."
  (multiple-value-bind (start end reg-start reg-end)
      (ppcre:scan regex *input* :start *position*)
    (when (and start end)
      ;; Update the capture end to point to the previous match end, and the
      ;; match end to point to the newly found match end.
      (loop for (operator argument) on instructions by #'cddr
            do (case operator
                 (:token (progress-token argument start end))
                 (:groups (progress-groups argument reg-start reg-end))
                 (:state (enter-state lexer argument))
                 (t (funcall operator argument))))
      (throw :restart t))))

(defmacro %rule (lexer-sym pattern &body instructions)
  (unless (null instructions)
    `(try-progress ,lexer-sym ,pattern ',instructions)))

;; This function is wrapped in an EVAL-WHEN because it's used by an invocation
;; of DEFSTATE later on.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun rule-scanner-definition (pattern state-flags)
    (let* ((regex `(format nil "\\A(?:~A)"
                           ,(if (consp pattern)
                                (first pattern)
                                pattern))))
      `(ppcre:create-scanner
        ,@(if (consp pattern)
              (cons regex (rest pattern))
              (list regex))
        ,@state-flags
        :single-line-mode nil
        :multi-line-mode t))))

(defmacro defstate (lexer name (&rest state-flags)
                    &body rules)
  (let* ((lexer-sym (gensym))
         (state-sym (gensym)))
    ;; We're going to loop over all specified rules and collect two things:
    ;; 1. A list of macro calls to %RULE, with as their second argument a newly
    ;;    gensym'd symbol.
    ;; 2. A list of LET-bindings for around the method definition from those
    ;;    symbols to calls to PPCRE:CREATE-SCANNER.
    (multiple-value-bind (rules let-bindings)
        (loop for (regex . instructions) in rules
              for let-sym = (gensym "RULE-REGEX")
              if (eq :include regex)
                ;; This is not a normal rule, so the names REGEX and
                ;; INSTRUCTIONS are not relevant. Instead, the form looks like
                ;; the following: (:include STATE-NAME).
                collect `(%process ,lexer-sym ,(first instructions))
                  into rules
              else
                collect (append `(%rule ,lexer-sym ,let-sym)
                                instructions)
                  into rules
                  and collect
                      (list let-sym (rule-scanner-definition regex
                                                             state-flags))
                into bindings
              end
              finally (return (values rules bindings)))
      `(let (,@let-bindings)
         (defmethod %process ((,lexer-sym ,lexer) (,state-sym (eql ,name)))
           (or ,@rules))))))

(defstate regex-lexer 'end-of-state ()
  ;; Because of the following two rules every state automatically stops at the
  ;; end of the file, and all errors (when no rule matches) are caught.
  ("\\Z" :state :pop!)
  ("." :token :error))

(defmethod process ((lexer regex-lexer) state)
  (let ((pops-left (catch :pop!
                     (loop (catch :restart
                             (%process lexer state)
                             (%process lexer 'end-of-state))))))
    (when (and (numberp pops-left)
               (plusp pops-left))
      (throw :pop! (1- pops-left)))))

;;; Utilities -----------------------------------------------------------------

(defun words (list &key (prefix "") (suffix ""))
  (format nil "~A(~{~A~^|~})~A"
          prefix
          list
          suffix))
