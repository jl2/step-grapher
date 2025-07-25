;; package.lisp
;; Copyright (c) 2024 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

;; Permission to use, copy, modify, and/or distribute this software for any
;; purpose with or without fee is hereby granted, provided that the above
;; copyright notice and this permission notice appear in all copies.

;; THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
;; WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
;; MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
;; ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
;; WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
;; ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
;; OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

(defpackage :step-grapher

  (:nicknames :sg)

  (:use #:cl #:alexandria)
  (:export #:main
           #:find-step-file
           #:*step-file-dirs*

           #:read-step-file
           
           #:graph-step-file


           #:step-file
           #:summarize
           #:step-statement
           #:step-header
           #:header
           #:entity-map
           #:entity-type-map
           #:comes-from
           #:statements

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
           #:compute-related-entities
           ))
