(defpackage :crylic/regex-lexer
  (:use :closer-common-lisp :crylic/lexer)
  (:export #:define-regex-lexer
           #:regex-lexer
           #:defstate
           ;; Instructions
           #:state
           #:groups
           #:using
           ;; Utilities
           #:words))
(in-package :crylic/regex-lexer)

;;; The regex lexer -----------------------------------------------------------

(define-lexer-type define-regex-lexer
    (regex-lexer-class (lexer-class)
                       ((%flags :initarg :flags
                                :initform ()
                                :reader flags)
                        (%default-state :initarg :default-state
                                        :initform :root
                                        :reader default-state)))
    (regex-lexer (lexer) ()))

(defmethod ensure-class-using-class :after ((class regex-lexer-class)
                                            name
                                            &key &allow-other-keys)
  (when (consp (default-state class))
    (setf (slot-value class '%default-state)
          (first (default-state class)))))


;;; Processing ----------------------------------------------------------------

;; Processing happens through the generic function %PROCESS, whose methods are
;; responsible for doing any actual work. They are usually defined using
;; DEFSTATE.
(defgeneric %process (lexer state))

;; PROCESS essentially wraps %PROCESS in machinery to take care of popping the
;; process stack and making sure the matching process ends.
(defun process (lexer state)
  (let ((pops-left (catch :pop!
                     (loop (catch :restart
                             (%process lexer state)
                             (%process lexer 'end-of-state))))))
    (when (and (numberp pops-left)
               (plusp pops-left))
      (throw :pop! (1- pops-left)))))

;; *lexer* is, during the matching process, bound to the lexer being used for
;; *matching.
(defvar *lexer*)

;; *input* contains the text that is being processed
(defvar *input* "")

;; During the matching process, whenever a regex matches at the index
;; *position*, it is set to the end of the match and appropriate instructions
;; are carried out.
(defvar *position* 0)

;; During the matching process, all tokens are collected into *tokens* in
;; reverse order
(defvar *tokens* ())

(defun inner-lex (lexer text)
  "Makes a lexer go process the input TEXT. *TOKENS* is ignored by this
function, so it's the caller's responsibility to bind it and do something with
the contents after processing ends."
  (let ((*input* text)
        (*lexer* lexer)
        (*position* 0))
    (process lexer (default-state (class-of lexer)))))

(defmethod lex ((lexer regex-lexer) (text string))
  (let ((*tokens* ()))
    (inner-lex lexer text)
    (reverse *tokens*)))


;;; Instructions --------------------------------------------------------------

(defun capture-token (type capture-start capture-end)
  "If the text between CAPTURE-START and CAPTURE-END is not empty, collect its
contents as a new token of type TYPE."
  (unless (= capture-start capture-end)
    (push (cons type
                (subseq *input*
                        capture-start
                        capture-end))
          *tokens*)))

;; Every regex comes with a set of instructions. These instructions, after
;; evaluation, come in two forms: keywords and functions. The keywords are
;; interpreted as token types, causing the current match to be collected as a
;; token. The functions are called with the match details (start, end, vectors
;; with register start- and end positions). Every such function can in turn
;; call APPLY-INSTRUCTION to take care of nested instructions.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun apply-instruction (instruction start end reg-start reg-end)
    (etypecase instruction
      (keyword (capture-token instruction start end))
      ((or function symbol)
       (funcall instruction start end reg-start reg-end))))

  (defun groups (&rest instructions)
    (lambda (start end reg-start reg-end)
      (declare (ignore start end))
      (loop for rstart across reg-start
            for rend across reg-end
            for instruction in instructions
            do (when (and rstart rend)
                 (apply-instruction instruction rstart rend nil nil)))))

  (defun using (class-name &rest options)
    (lambda (start end reg-start reg-end)
      (declare (ignore reg-start reg-end))
      ;; We just call INNER-LEX to take care of the matching process. *TOKENS*
      ;; is already taken care of by the enclosing lexing operation, so all
      ;; output of this new lexer will go to the existing *TOKENS* list.
      (inner-lex (apply #'make-instance class-name options)
                 (subseq *input* start end))))

  (defun state (name &optional argument)
    (lambda (start end reg-start reg-end)
      (declare (ignore start end reg-start reg-end))
      (if (eq name :pop!)
          ;; The pop! count is specified here as the number of pops left. Since
          ;; one pop is being executed here, the option should immediately be
          ;; decremented.
          (throw :pop! (or (when argument (1- argument))
                           0))
          (process *lexer* name)))))


;;; DEFSTATE ------------------------------------------------------------------

(defmacro %rule (regex &body instructions)
  "Check whether REGEX matches at the current processing position and if so,
set *POSITION* to the new start, process all INSTRUCTIONS and restart the
current state."
  (unless (null instructions)
    (let ((start (gensym "START"))
          (end (gensym "END"))
          (reg-start (gensym "REG-START"))
          (reg-end (gensym "REG-END")))
      `(multiple-value-bind (,start ,end ,reg-start ,reg-end)
           (ppcre:scan ,regex *input* :start *position*)
         (when (and ,start ,end)
           (setf *position* ,end)
           ,@(loop for instruction in instructions
                   collect `(apply-instruction ,instruction
                                               ,start ,end
                                               ,reg-start ,reg-end))
           (throw :restart t))))))

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

  (defun expand-regex-rule (rule lexer-regex-flags)
    (destructuring-bind (regex &rest instructions)
        rule
      (let ((let-sym (gensym "RULE-REGEX")))
        (list (append `(%rule ,let-sym)
                      instructions)
              (list let-sym
                    (rule-scanner-definition regex lexer-regex-flags))))))

  (defun expand-rule (lexer-sym rule lexer-regex-flags)
    (check-type rule cons)
    (case (first rule)
      (:include (list `(%process ,lexer-sym ,(second rule))
                      nil))
      (t (expand-regex-rule rule lexer-regex-flags)))))


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
  ;; This state is automatically processed after every normal state (by
  ;; PROCESS), to stop processing at the end of the file, to catch any
  ;; unmatched newlines and to catch errors.
  ("\\z" (state :pop!))
  ("\\n" :text)
  ("." :error))


;;; Utilities -----------------------------------------------------------------

(defun words (list &key (prefix "") (suffix "") (capturing t))
  (format nil "~A(~A~{~A~^|~})~A"
          prefix
          (if capturing "" "?:")
          list
          suffix))
