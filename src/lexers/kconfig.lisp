(defpackage :crylic/lexers/kconfig
  (:use :cl :crylic/regex-lexer)
  (:export #:kconfig-lexer))
(in-package :crylic/lexers/kconfig)

(define-regex-lexer kconfig-lexer () ()
  (:title "Kconfig")
  (:description "Lexer for Linux-style Kconfig files.")
  (:tags "kconfig" "menuconfig" "linux-config" "kernel-config")
  (:filenames "Kconfig" "*Config.in*" "external.in*" "standard-modules.in")
  (:mime-types "text/x-kconfig"))

(defun indent (level)
  (let* ((tab-width 8)
         (space-repeat
           (if (= tab-width 2)
               "+"
               (format nil "{1,~A}" (1- tab-width))))
         (level-repeat
           (if (= level 1)
               ""
               (format nil "{~A}" level))))
    (format nil "(?:\\t| ~A\\t| {~A})~A.*\\n"
            space-repeat
            tab-width
            level-repeat)))

(defstate kconfig-lexer :root ()
  ("\\s+" :text)
  ("#.*?\\n" :comment.single)
  (((words '("mainmenu" "config" "menuconfig" "choice" "endchoice"
             "comment" "menu" "endmenu" "visible if" "if" "endif"
             "source" "prompt" "select" "depends on" "default"
             "range" "option")
           :suffix "\\b"))
   :keyword)
  ("(---help---|help)[\\t ]*\\n" :keyword
                                 (state :help))
  ("(bool|tristate|string|hex|int|defconfig_list|modules|env)\\b"
   :name.builtin)
  ("[!=&|]" :operator)
  ("[()]" :punctuation)
  ("[0-9]+" :number.integer)
  ("'(''|[^'])*'" :string.single)
  ("\"(\"\"|[^\"])*\"" :string.double)
  ("\\S+" :text))

;; Help text is indented, multi-line and ends when a lower indentation level is
;; detected.
(defstate kconfig-lexer :help ()
  ;; Skip blank lines after help token, if any
  ("\\s*\\n" :text)
  (((indent 7)) :string.doc (state :indent7))
  (((indent 6)) :string.doc (state :indent6))
  (((indent 5)) :string.doc (state :indent5))
  (((indent 4)) :string.doc (state :indent4))
  (((indent 3)) :string.doc (state :indent3))
  (((indent 2)) :string.doc (state :indent2))
  (((indent 1)) :string.doc (state :indent1))
  ("" (state :pop!)))

(defmacro define-indent-state (name level)
  ;; Print paragraphs of indentation level >= LEVEL as :string.doc, ignoring
  ;; blank lines. Then return to :root state.
  `(defstate kconfig-lexer ,name ()
     (((indent ,level)) :string.doc)
     ("\\s*\\n" :text)
     ("" (state :pop! 2))))

(define-indent-state :indent7 7)
(define-indent-state :indent6 6)
(define-indent-state :indent5 5)
(define-indent-state :indent4 4)
(define-indent-state :indent3 3)
(define-indent-state :indent2 2)
(define-indent-state :indent1 1)
