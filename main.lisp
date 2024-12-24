;; main.lisp
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

(in-package :step-grapher)

(defun main (args)
  (handler-case
      (let* ((parser (cl-argparse:create-main-parser
                         (main-parser "Display the structure of a STEP file as a Directed Acyclic Graph using GraphViz." "step-grapher")
                       (cl-argparse:add-positional main-parser
                                                   :name "step-file"
                                                   :help (format nil  "The STEP file to use.  If not a full path, searches in directories ~a." *step-file-dirs*))
                       (cl-argparse:add-flag main-parser
                                             :var "open-it"
                                             :long "no-open"
                                             :short "no")
                       (cl-argparse:add-flag main-parser
                                             :var "no-skip"
                                             :long "no-skip"
                                             :short "ns")
                       (cl-argparse:add-optional  main-parser
                                                  :var "output-type"
                                                  :help"The output file type: svg, pdf, png, etc.  Must be supported by GraphViz."
                                                  :long "type"
                                                  :short "t"
                                                  :default "svg")
                       
                       ))
             (pargvs (cl-argparse:parse parser (cdr args))))
        (graph-step-file (cl-argparse:get-value "step-file" pargvs)
                         :output-type (cl-argparse:get-value "output-type" pargvs)
                         :open-file (not (cl-argparse:get-value "open-it" pargvs))
                         :skip-list (if (cl-argparse:get-value "no-skip" pargvs)
                                        nil
                                        '("CARTESIAN_POINT"
                                       ;; "ORIENTED_EDGE"
                                       "DIRECTION"
                                       "LINE"
                                       "VECTOR"
                                       "VERTEX_POINT"
                                       ;; "EDGE_LOOP"
                                       ))))
    (cl-argparse:cancel-parsing-error (e)
      (format t "~a~%" e)))
  0)
