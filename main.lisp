;; Copyright (c) 2026 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

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
