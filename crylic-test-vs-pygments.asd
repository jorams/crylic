(asdf:defsystem :crylic-test-vs-pygments
  :description "Tests comparing Crylic to Pygments"
  :author "Joram Schrijver <i@joram.io>"
  :serial t
  :pathname "test"
  :depends-on (:crylic :crylic-test :uiop)
  :components ((:file "vs-pygments"))
  :perform (test-op :after (op component)
                    (funcall (intern "RUN-VS" :crylic-test/vs-pygments))))
