;;;; queue.asd

(asdf:defsystem #:queue
  :description "Describe queue here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :components ((:file "package")
               (:file "queue"))
  :in-order-to ((test-op (test-op queue/tests))))

(asdf:defsystem #:queue/tests
  :depends-on (:queue :prove)
  :defsystem-depends-on (:prove-asdf)
  :components ((:file "tests/package")
               (:test-file "tests/main"))
  :perform
  (test-op :after (op c)
           (funcall (intern #.(string :run) :prove) c)))
