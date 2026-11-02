;; Copyright (c) 2026 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

(in-package :cl-user)
(defpackage :step-grapher.test-asd
  (:use :cl :asdf))
(in-package :step-grapher.test-asd)

(asdf:defsystem #:step-grapher.test
  :description "Test step-grapher"
  :author "Jeremiah LaRocco <jeremiah_larocco@fastmail.com>"
  :license "ISC"
  :version "0.0.1"
  :serial t

  :depends-on ( :step-grapher
                :fiveam)

  :components ((:module "t"
                :components
                ((:file "package"))))
  :perform (test-op
            :after (op c)
            (eval
             (read-from-string
              "(every #'fiveam::TEST-PASSED-P
                     (5am:run :step-grapher))"))))
