;; step-grapher.lisp
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

(defparameter *step-file-dirs* (list
                               (asdf:system-relative-pathname :step-grapher "step-files/")
                               "~/data/3d-models/step-files/")
  "Directories in which to look for STEP files.  Used to search if a full path is not used.")

(defun find-step-file (fname)
  "Search for a  file name (like \"vice.stp\" in th *step-file-dires*"
  (ctypecase fname
    (string
     (let ((path (probe-file fname)))
       (if path
           path
           (loop
             :for path :in *step-file-dirs*
             :do
                (when-let (the-file (probe-file (merge-pathnames fname path)))
                  (return the-file))
             :finally (error "Could not find ~a" fname)))))
    (pathname
     (probe-file fname))))



(defun strip-leading-whitespace (string)
  "Return a new string with leading whitespace removed."
  (flet ((count-leading-whitespace ()
           (loop :for char :across string
                 :for i :from 0
                 :while char
                 :until (not (whitespace-p char))
                 :finally (return i))))
    (subseq string (count-leading-whitespace))))

(defun read-entity-type (step-statement)
  "Try to find the entity type of step-statement.~
In the form \"#1 = CARTESIAN_POINT(...);\"~ the entity type is \"CARTESIAN_POINT\".~
Statements of the form\"#1 = (...)\" are not supported, and the type \"unsupported-constraint\" is returned.~
This function is a hack, and needs to be improved."
  (let ((right-hand-side
          (if (find #\= step-statement)
              ;; TODO: What if = is in a string? or occurs multiple times?
              (subseq step-statement (1+ (search "=" step-statement)))
              step-statement)))
    (cond ((and (char= (aref right-hand-side 0) #\()
                (not 
                     (alpha-char-p (aref right-hand-side 1))))
           "unsupported-constraint")
          ((char= (aref right-hand-side 0) #\()
           (symbol-name (read-from-string (subseq right-hand-side 1))))
          (t
           (symbol-name (read-from-string right-hand-side))))))

(defun parse-simple-entity (step-statement)
  "Create a step-entity from a step-statement of the form \"#123 = some_thing(..., #4, #45, #23, ...);\""
  (loop :with entity-type = (read-entity-type step-statement)
        :with entity-ids list = nil
        :with cur-entity-id fixnum = 0
        :with in-string = nil
        :with in-entity-id = nil

        :for char :across step-statement

        :when (char= char #\')
          :do (setf in-string (not in-string))

        :when (and (not in-string)
                   (char= char #\#))
          :do (setf in-entity-id t)

        :when (and in-entity-id (numeric-p char))
          :do (setf cur-entity-id (+ (* cur-entity-id 10)
                                     (- (char-code char)
                                        (char-code #\0))))
        :when (and in-entity-id
                   (not (numeric-p char))
                   (> cur-entity-id 0))
          :do
             (push cur-entity-id entity-ids)
             (setf cur-entity-id 0
                   in-entity-id nil)

        :finally
           (return
             (when entity-ids
               (let ((result (reverse entity-ids)))
                       
                 (make-instance 'step-entity
                                :id (first result)
                                :entity-type entity-type
                                :references (rest result)
                                :text step-statement))))))
(defun parse-statement (step-statement)
  "Create a step-entity from a string."
  (cond
    ;; Check for statements starting with #number = ...
    ((char= #\# (aref step-statement 0))
     (parse-simple-entity step-statement))

    ;; A real parser would parse other things, like headers,
    ;; but this tool doesn't use that data, so , so create a dummy node
    (t
     (make-instance 'step-entity
                    :id 0
                    :references nil
                    :entity-type (read-entity-type step-statement)
                    :text step-statement))))

(defun read-step-statement (stream)
  "Read the next statement from stream and try to parse it into a step-entity.~
Return nil at end of file."
  (let ((chars
          (loop
            :for previous-was-whitespace = nil :then is-whitespace
            :for nc = (peek-char t stream nil nil nil)
              :then (peek-char nil stream nil nil nil)
            :while nc
            :for is-whitespace = (whitespace-p nc)
            :until (char= nc #\;)
            :when (and is-whitespace
                       (not previous-was-whitespace))
              :collect #\space
            :when is-whitespace
              :do (read-char stream nil nil)
            :when (not is-whitespace)
              :collect (read-char stream nil nil)))
        (semi-colon (read-char stream nil nil)))
    (declare (ignorable semi-colon))
    (if (null chars)
        nil
        (parse-statement
         (strip-leading-whitespace
          (coerce chars 'string))))))


(defun whitespace-p (char)
  "Return t if a character is a whitespace, nil otherwise."
  (declare (type character char))
  #+sbcl (sb-unicode:whitespace-p char)
  #+clisp (find char (list #\space #\tab #\newline) :test #'char=)
  #-(or sbcl clisp) (cl-unicode:has-property char "whitespace"))

(defun numeric-p (char)
  "Check if char is a digit 0-9."
  (find char #(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9) :test #'char=))

(defclass step-entity ()
  ((id :type fixnum
       :initarg :id
       :documentation "The entity ID.")
   (entity-type :type string
                :initarg :entity-type
                :documentation "The entity type (the identifier following the #123 = <entity-type> ( ... );)")
   (references :type list
               :initarg :references
               :documentation "The entity IDs that this entity references.")
   (text :type string
         :initarg :text
         :documentation "The text from the STEP file defining this entity."))
  (:documentation "A dumbed down STEP entity."))

(defmethod cl:print-object ((object step-entity) stream)
  (with-slots (id entity-type) object
    (format stream "(step-entity ~a ~a)" id entity-type)))

(defun read-step-file (fname)
  (with-input-from-file (ins (find-step-file fname))
    (loop :for statement = (read-step-statement ins)
          :while statement
          :collect statement)))

(defun elide-text (text max-length)
  "Shorten text to max-length.  If the string is longer than max-length, shortens to (- max-length 3) and appends \"...\" "
  (let ((elide-text (> (length text)  max-length)))
    (if elide-text
        (concatenate 'string
                     (subseq text 0
                             (max 0 (- max-length 3)))
                     "...")
        text)))

(defun graph-step-file (step-file-name
                        &key
                          (dot-file-name (merge-pathnames (make-pathname :type "dot")
                                                          (find-step-file step-file-name)))
                          (output-type "svg")
                          (out-file-name (merge-pathnames (make-pathname :type output-type)
                                                          (find-step-file step-file-name)))
                          (open-file t)
                          (node-sep 0.4)
                          (spline-type "true")
                          (graph-cmd "dot")
                          (wait-for-dot open-file)
                          (skip-list '("CARTESIAN_POINT"
                                       ;; "ORIENTED_EDGE"
                                       "DIRECTION"
                                       "LINE"
                                       "VECTOR"
                                       "VERTEX_POINT"
                                       ;; "EDGE_LOOP"
                                       )))
  "Create (and optionally display) a graph of the STEP file named by step-file-name.~
Entity types listed in skip-list will not be represented in the output graph. ~
graph-cmd, spline-type, node-sep control a few Dot parameters and how Dot is invoked.~
output-type, dot-file-name, and out-file-name control the Dot output.  Default is SVG, ~
using the same name as the STEP file, but with .svg and .dot extensions. ~
If open-file is t, the out-file-name graph image will be opened in a sensible viewer. ~
If open-file is a string, it should be the name of a program to open out-file-name with."

  (let ((step-pathname (find-step-file step-file-name)))
    ;; Generate the .dot file
    (with-output-to-file (dots dot-file-name)
      (format dots
              "digraph ~s { rankdir=\"LR\"~%nodesep=~a~%overlap=false~%splines=~a~%"
              (namestring step-pathname)
              node-sep
              spline-type)
      (with-input-from-file (ins step-pathname)
        (let ((step-table (make-hash-table)))
          ;; First build up a table of ID -> step-entity
          (loop :for entity = (read-step-statement ins)
                :while entity
                :for this-id = (slot-value entity 'id)
                :when (and entity
                           (not (zerop this-id)))
                  :do
                     (setf (gethash this-id step-table) entity))
          ;; Next, write the dot file by iterating over the hashtable
          ;; and writing node labels followed by edges to its references,
          ;; ID0 -> ID1; ID0 -> ID2; ID0 -> #ID3; ...
          ;; Skip entity types that are in skip-list
          (loop
            :for id :being :the :hash-keys :of step-table
              :using (hash-value entity)
            :do
               (with-slots (entity-type references text) entity
                 (when (not (find entity-type skip-list :test #'string-equal))
                   (format dots
                           "~s [label=\"~a(~a)\" tooltip=\"~a\"];~%"
                           id
                           (elide-text entity-type 512)
                           id
                           (elide-text text 512))
                   (loop
                     :with from = id
                     :for goes-to :in references
                     :for goes-to-type = (slot-value (gethash goes-to step-table) 'entity-type)
                     :for should-skip = (find goes-to-type skip-list :test #'string-equal)
                     :when (not should-skip) :do
                       (format dots "~s -> ~s;~%" from goes-to)))))
          
          ))
      (format dots "}~%")))

  ;; Use GraphViz to generate the graph
  (let ((cmd (format nil
                     "~a -T~a -o ~s ~s"
                     graph-cmd
                     output-type
                     (namestring out-file-name)
                     (namestring dot-file-name))))
    (format t "Running: ~s~%" cmd)
    (if wait-for-dot
        (uiop:run-program cmd :output t :error-output t :force-shell t)
        (uiop:launch-program cmd :output t :error-output t :force-shell t))
    
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
