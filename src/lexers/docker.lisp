(defpackage :crylic/lexers/docker
  (:use :cl :crylic/regex-lexer)
  (:export #:docker-lexer))
(in-package :crylic/lexers/docker)

(define-regex-lexer docker-lexer () ()
  (:title "Docker")
  (:description "Lexer for Docker configuration files.")
  (:tags "docker" "dockerfile")
  (:filenames "Dockerfile" "*.docker")
  (:mime-types "text/x-dockerfile-config")
  (:flags :case-insensitive-mode t :multi-line-mode t))

(let ((keywords
        (string+ "(?:FROM|MAINTAINER|CMD|EXPOSE|ENV|ADD|ENTRYPOINT|"
                 "VOLUME|WORKDIR)")))
  (defstate docker-lexer :root ()
    (((format nil "^(ONBUILD)(\\s+)(~A)\\b" keywords))
     (groups :name.keyword :whitespace :keyword))
    (((format nil "^(~A)\\b(.*)" keywords))
     (groups :keyword :string))
    ("#.*" :comment)
    ("RUN" :keyword)                    ; Rest of line falls through
    ("(.*\\\\\\n)*.+" (using 'crylic/lexers/bash:bash-lexer))))
