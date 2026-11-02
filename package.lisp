;; Copyright (c) 2026 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

(defpackage :step-grapher

  (:nicknames :sg)

  (:use #:cl #:alexandria)
  (:export #:main
           #:find-step-file
           #:*step-file-dirs*

           #:read-step
           #:read-step-file

           #:graph-step-file

           #:translate-language
           #:*target-language*
           #:*source-language*

           #:show-inspector
           #:inspect-entities
           #:step-file
           #:dereference
           #:show-inspector
           #:summarize
           #:step-statement
           #:step-header
           #:header
           #:entity-map
           #:entity-type-map
           #:comes-from
           #:statements
           #:get-entities

           #:entity-types
           #:entity-ids

           #:step-entity
           #:id
           #:index
           #:entity-type
           #:statement
           #:statement-at
           #:references
           #:entity-id-integer

           #:step-statement
           #:tokens
           #:token-count
           #:token-at
           #:read-step-statement

           #:entity
           #:entity-name
           #:entities-of-type
           #:entity-references
           #:entities-referenced-by
           #:related-entities
           ))
