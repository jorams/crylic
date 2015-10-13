(defpackage :crylic-test/vs-pygments
  (:use :cl :crylic-test/framework)
  (:import-from :crylic/lexer
                #:lex)
  (:import-from :crylic/filter
                #:filter)
  (:import-from :crylic/renderer
                #:render-to-string)
  (:import-from :crylic/renderers/html
                #:html-renderer)
  (:import-from :crylic/filters/split-multiline
                #:split-multiline-filter)
  (:import-from :crylic/filters/merge-consecutive
                #:merge-consecutive-filter))
(in-package :crylic-test/vs-pygments)


;;; Infrastructure ------------------------------------------------------------

(define-test-framework *tests-vs* test-vs run-vs)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *examples-directory*
    (make-pathname
     :defaults (merge-pathnames #p"examples/"
                                #.(or *compile-file-truename* *load-truename*))
     :type nil
     :name nil)))


;; The settings used here should be roughly equivalent to what Pygments does
(defun render-to-html (lexer-class string)
  (render-to-string
   (make-instance 'html-renderer)
   (filter (make-instance 'merge-consecutive-filter)
           (filter (make-instance 'split-multiline-filter)
                   (filter (make-instance 'merge-consecutive-filter)
                           (lex (make-instance lexer-class)
                                string))))))

(defmacro test-compare-html (name lexer-class pygments-lexer file)
  (let ((path (merge-pathnames file *examples-directory*)))
    `(test-vs ,name
       (is (string= (render-to-html ',lexer-class
                                    (uiop:read-file-string ,path))
                    (uiop:run-program
                     (format nil "pygmentize -f html -l ~A ~A"
                             ,pygments-lexer
                             (uiop:native-namestring ,path))
                     :output :string))))))

;;; Tests ---------------------------------------------------------------------

(test-compare-html ini
                   crylic/lexers/ini:ini-lexer
                   "ini"
                   "test.ini")

(test-compare-html properties
                   crylic/lexers/properties:properties-lexer
                   "properties"
                   "java.properties")

(test-compare-html regedit
                   crylic/lexers/regedit:regedit-lexer
                   "registry"
                   "example.reg")

(test-compare-html cfengine3
                   crylic/lexers/cfengine3:cfengine3-lexer
                   "cfengine3"
                   "example.cf")

(test-compare-html apache
                   crylic/lexers/apache:apache-lexer
                   "apacheconf"
                   "apache2.conf")

(test-compare-html nginx
                   crylic/lexers/nginx:nginx-lexer
                   "nginx"
                   "nginx.conf")

(test-compare-html lighttpd
                   crylic/lexers/lighttpd:lighttpd-lexer
                   "lighty"
                   "lighttpd.conf")

(test-compare-html squid
                   crylic/lexers/squid:squid-lexer
                   "squidconf"
                   "squid.conf")

(test-compare-html kconfig
                   crylic/lexers/kconfig:kconfig-lexer
                   "kconfig"
                   "Config.in.cache")

(test-compare-html bash
                   crylic/lexers/bash:bash-lexer
                   "bash"
                   "example.sh")

(test-compare-html docker
                   crylic/lexers/docker:docker-lexer
                   "docker"
                   "docker.docker")
