;; step-grapher.lisp
;; Copyright (c) 2025 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

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


(defun elide-text (text max-length)
  "Shorten text to max-length.  If the string is longer than max-length, shortens to
 (- max-length 3) and appends \"...\" "
  (let ((elide-text (> (length text)  max-length)))
    (if elide-text
        (concatenate 'string
                     (subseq text 0
                             (max 0 (- max-length 3)))
                     "...")
        text)))

(defun compute-related-entities (step-file entity-ids
                                 &key
                                   (direction :up-and-down)
                                   (radius most-positive-fixnum)
                                   (skip-list '("CARTESIAN_POINT"
                                                ;;"ORIENTED_EDGE"
                                                "DIRECTION"
                                                "LINE"
                                                "VECTOR"
                                                "VERTEX_POINT"
                                                "EDGE_LOOP"
                                                ;; "EDGE_CURVE"
                                                )))
  "Create a hashtable who's keys are the entity IDs of the entities that are related ~
to the entities given in entity-ids. ~
direction controls how parent and child entities are included.
:up - only parent entities and their parents will be included 
:down - only the child entities and their children will be included
:all - all parents and all of their children, recursively in both directions
:up-and-down - :up for parents and :down for children
:down-and-up - :down for parents and :up for children
"
  (let ((result-table (make-hash-table :test 'equal)))
    (labels
        ((add-all-refs (eids dir radius)
           ;;(format t "Add-all-refs ~a ~a ~a ~a~%" eids dir radius (hash-table-keys result-table))
           (loop 
             :with new-radius = (- radius 1)
             :for eid :in eids
                     
             :when (and (not (zerop new-radius))
                        (not (find (entity-type (entity step-file eid))
                                   skip-list :test #'string=))
                        (null (gethash eid result-table)))
               :do
                  (setf (gethash eid result-table) t)
                          
                  (when (and (find dir '(:all :down :up-and-down :down-and-up))
                             (>= radius 0))
                    (add-all-refs (entity-references step-file eid )
                                  (case dir
                                    (:all :all)
                                    (t :down))
                                  new-radius
                                  ))
                  (when (and (find dir '(:all :up :up-and-down :down-and-up))
                             (>= radius 0))
                            
                    (add-all-refs (entities-referenced-by step-file eid)
                                  (case dir
                                    (:all :all)
                                    (t :up))
                                  new-radius
                                  )))))
      (add-all-refs entity-ids direction radius)
      result-table)))

(defun graph-step-file (step-file
                        &key
                          (step-file-pathname (if (eq (type-of step-file) 'step-file)
                                                  #p"step-file.stp"
                                                  (find-step-file step-file)))
                          (entities-of-interest nil)
                          (direction :up-and-down)
                          (radius most-positive-fixnum)
                          (dot-file-name (merge-pathnames (make-pathname
                                                           :name (when entities-of-interest
                                                                   (format nil "~a(~{~a~^_~})"
                                                                           (pathname-name step-file-pathname)
                                                                           entities-of-interest))
                                                                         
                                                           :type "dot")
                                                          step-file-pathname))
                          (output-type "svg")
                          (out-file-name (merge-pathnames (make-pathname
                                                           :name (when entities-of-interest
                                                                   (format nil "~a(~{~a~^_~})"
                                                                           (pathname-name step-file-pathname)
                                                                           entities-of-interest))
                                                           :type output-type)
                                                          step-file-pathname))
                          (open-file t)
                          (keep-dot t)
                          (node-sep 0.4)
                          (rank-sep 2.0)
                          (spline-type "true")
                          (graph-cmd "dot")
                          (wait-for-dot open-file)
                          (skip-list '("CARTESIAN_POINT"
                                       ;;"ORIENTED_EDGE"
                                       "DIRECTION"
                                       "LINE"
                                       "VECTOR"
                                       "VERTEX_POINT"
                                       "EDGE_LOOP"
                                      ;; "EDGE_CURVE"
                                       )))
  "Create (and optionally display) a graph of the STEP file named by step-file-name. ~
entities-of-interest is a list of entity IDs to filter on.  When non-nil, only these ~
entities and, recursively, the entities they go to or that go to them will be graphed. ~
Entity types listed in skip-list will not be represented in the output graph unless ~
they are explicitly in entities-of-interest - this is not done recursively. ~
graph-cmd, spline-type, node-sep control a few Dot parameters and how Dot is invoked. ~
output-type, dot-file-name, and out-file-name control the Dot output.  Default is SVG, ~
using the same name as the STEP file, but with .svg and .dot extensions. ~
If open-file is t, the out-file-name graph image will be opened in a sensible viewer. ~
If open-file is a string, it should be the name of a program to open out-file-name with."

  (let* ((step-file (typecase step-file
                      (step-file step-file)
                      (t (read-step-file (find-step-file step-file)))))
         
         (real-entities-of-interest (if entities-of-interest
                                        (compute-related-entities step-file
                                                                  entities-of-interest
                                                                  :direction direction
                                                                  :skip-list skip-list
                                                                  :radius radius)
                                        (entity-map step-file))))
    ;; Generate the .dot file
    (with-output-to-file (dots dot-file-name)
      (format dots
              ;; TODO: Allow more customization here
              "digraph ~s { rankdir=\"LR\"~%~%ranksep=~a~%nodesep=~a~%overlap=false~%splines=~a~%"
              (namestring step-file-pathname)
              rank-sep
              node-sep
              spline-type)

      (loop
        :for ent-id :being :the :hash-keys :of real-entities-of-interest
        :for entity = (gethash ent-id (entity-map step-file))
        :for should-show = (and
                            (not (find (entity-type entity) skip-list :test #'string=))
                            (gethash ent-id real-entities-of-interest))
        :for ent-type = (entity-type entity)
        :when should-show
          :do
             (format dots
                     "~a [label=\"~a ~a\" tooltip=\"~a\"];~%"
                     ent-id
                     ent-id
                     (elide-text (format nil "~a" (entity-type entity)) 512)
                     (elide-text (format nil "~a" (statement entity)) 512))
             (loop
               :with from = ent-id
               :for goes-to :in (references entity)
               :for goes-to-type = (entity-type (entity step-file goes-to))
               :for should-show-ref = (and
                                   (gethash goes-to real-entities-of-interest)
                                   (not (find goes-to-type skip-list :test #'string=))
                                   )
               :when should-show-ref
                 :do
                    (format dots
                            "~s -> ~s;~%"
                            from
                            goes-to
                            )))
      
      
      (format dots "}~%")))

  ;; Use GraphViz to generate the graph
  (let ((cmd (format nil
                     "~a -T~a -o ~s ~s"
                     graph-cmd
                     output-type
                     (namestring out-file-name)
                     (namestring dot-file-name))))
    (format t "Running: ~s~%" cmd)
    ;; Run dot in the background unless we have to wait for it to finish -
    ;; either to open the file it creates, or delete the .dot file
    (if (or wait-for-dot (not keep-dot))
        (uiop:run-program cmd :output t :error-output t :force-shell t)
        (uiop:launch-program cmd :output t :error-output t :force-shell t))

    (when (not keep-dot)
      (delete-file dot-file-name))

    ;; Check if and how to open the graph image
    (cond ((stringp open-file)
           (uiop:launch-program (format nil "~a ~s &" open-file (namestring out-file-name))))
          (open-file
           (let ((open-program (assoc-value '(("pdf" . "mupdf")
                                              ("svg" . "firefox")
                                              ("png" . "xnview")
                                              ("jpg" . "xnview"))
                                            output-type :test #'string-equal)))
             (when open-program
               (uiop:launch-program (format nil "~a ~s" open-program (namestring out-file-name)))))))))
