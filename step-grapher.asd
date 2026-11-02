;; Copyright (c) 2026 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

(asdf:defsystem #:step-grapher
  :description "Use GraphViz to show a STEP file's reference structure."
  :author "Jeremiah LaRocco <jeremiah_larocco@fastmail.com>"
  :license  "ISC"
  :version "0.0.1"
  :serial t

  :depends-on (#:alexandria
               #:uiop
               #-sbcl #:cl-unicode
               #:cl-libre-translate
               #:cl-argparse
               #:cl-ppcre
               #:str
               )

  :components ((:file "package")

               (:file "utilities")
               (:file "step-file")
               (:file "step-header")
               (:file "step-statement")
               (:file "step-entity")
               (:file "step-grapher")
               (:file "main"))

  :in-order-to ((test-op (test-op
                          step-grapher.test))))
