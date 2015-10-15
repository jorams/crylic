(defpackage :crylic/regex-lexer
  (:use :cl :crylic/lexer)
  (:export #:define-regex-lexer
           #:regex-lexer
           #:defstate
           ;; Utilities
           #:words))
(in-package :crylic/regex-lexer)

(define-lexer-type define-regex-lexer
    (regex-lexer-class (lexer-class)
                       ((%flags :initarg :flags
                                :initform ()
                                :reader flags)))
    (regex-lexer (lexer) ()))

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

(defun delegate-lex (lexer text)
  (let ((*input* text)
        (*position* 0))
    (process lexer :root))
  (incf *position* (length text)))

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
      (loop for (operator argument) on instructions by #'cddr
            do (case operator
                 (:token (progress-token argument start end))
                 (:groups (progress-groups argument reg-start reg-end))
                 (:state (enter-state lexer argument))
                 (:using (delegate-lex (make-instance argument)
                                       (subseq *input* start end)))
                 (t (funcall operator argument))))
      (throw :restart t))))

(defmacro %rule (lexer-sym pattern &body instructions)
  (unless (null instructions)
    `(try-progress ,lexer-sym ,pattern ',instructions)))

;; This is wrapped in an EVAL-WHEN because it's used by an invocation of
;; DEFSTATE later on.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun rule-scanner-definition (pattern lexer-flags)
    (let* ((regex `(princ-to-string ,(if (consp pattern)
                                         (first pattern)
                                         pattern))))
      `(crylic/cl-ppcre-extensions:create-scanner
        ,@(if (consp pattern)
              (cons regex (rest pattern))
              (list regex))
        ,@lexer-flags
        :single-line-mode nil
        :multi-line-mode t)))

  (defun expand-regex-rule (lexer-sym rule lexer-regex-flags)
    (destructuring-bind (regex &rest instructions)
        rule
      (let ((let-sym (gensym "RULE-REGEX")))
        (list (append `(%rule ,lexer-sym ,let-sym)
                      instructions)
              (list let-sym
                    (rule-scanner-definition regex lexer-regex-flags))))))

  (defun expand-rule (lexer-sym rule lexer-regex-flags)
    (check-type rule cons)
    (case (first rule)
      (:include (list `(%process ,lexer-sym ,(second rule))
                      nil))
      (t (expand-regex-rule lexer-sym rule lexer-regex-flags)))))

(defmacro defstate (lexer name ()
                    &body rules)
  (let* ((lexer-sym (gensym))
         (state-sym (gensym))
         (lexer-flags (flags (find-class lexer))))
    ;; We're going to loop over all specified rules and collect two things:
    ;; 1. A list of macro calls to %RULE, with as their second argument a newly
    ;;    gensym'd symbol.
    ;; 2. A list of LET-bindings for around the method definition from those
    ;;    symbols to calls to PPCRE:CREATE-SCANNER.
    ;; One special rule type is specified as (:include :state-name), causing
    ;; the state's rules to be called in place of a normal rule definition.
    (multiple-value-bind (rules let-bindings)
        (loop for rule in rules
              for (expansion binding) = (expand-rule lexer-sym
                                                     rule
                                                     lexer-flags)
              when expansion
                collect it into expansions
              when binding
                collect it into bindings
              finally (return (values expansions bindings)))
      `(let (,@let-bindings)
         (defmethod %process ((,lexer-sym ,lexer) (,state-sym (eql ,name)))
           (or ,@rules))))))

(defstate regex-lexer 'end-of-state ()
  ;; This state is automatically processed after every normal state, to stop
  ;; processing at the end of the file, to catch any unmatched newlines and to
  ;; catch errors.
  ("\\z" :state :pop!)
  ("\\n" :token :text)
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

(defun words (list &key (prefix "") (suffix "") (capturing t))
  (format nil "~A(~A~{~A~^|~})~A"
          prefix
          (if capturing "" "?:")
          list
          suffix))
