(defpackage :crylic/lexers/docker
  (:use :cl :crylic/regex-lexer)
  (:export #:docker-lexer))
(in-package :crylic/lexers/docker)

(define-regex-lexer docker-lexer () ()
  (:title "Docker")
  (:description "Lexer for Docker configuration files.")
  (:tags "docker" "dockerfile")
  (:filenames "Dockerfile" "*.docker")
  (:mime-types "text/x-dockerfile-config"))

(let ((keywords
        (concatenate 'string
                     "(?:FROM|MAINTAINER|CMD|EXPOSE|ENV|ADD|ENTRYPOINT|"
                     "VOLUME|WORKDIR)")))
  (defstate docker-lexer :root (:case-insensitive-mode t
                                :multi-line-mode t)
    (((format nil "^(ONBUILD)(\\s+)(~A)\\b" keywords))
     :groups (:name.keyword :whitespace :keyword))
    (((format nil "^(~A)\\b(.*)" keywords))
     :groups (:keyword :string))
    ("#.*" :token :comment)
    ("RUN" :token :keyword)             ; Rest of line falls through
    ("(.*\\\\\\n)*.+" :using crylic/lexers/bash:bash-lexer)))
