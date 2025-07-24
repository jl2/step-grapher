;; step-file.lisp
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

(defclass step-file ()
  ((header
    :accessor header
    :type step-header
    :initarg :header
    :documentation "The STEP header for the STEP file.")

   (entity-map
    :accessor entity-map
    :type hashtable
    :initarg :entity-map
    :initform (make-hash-table :test 'equal)
    :documentation "A hashtable mapping entity IDs to corresponding entity-statments.")

   (entity-type-map
    :accessor entity-type-map
    :type hashtable
    :initarg :entity-type-map
    :initform (make-hash-table :test 'equal)
    :documentation "A hashtable mapping entity-types to a list of entity IDs of that type.")

   (comes-from
    :accessor comes-from
    :initform (make-hash-table :test 'equal)
    :initarg :comes-from
    :type hashtable
    :documentation "A hashtable mapping entity IDs (of the form \"#NNNN\") to a list of entity IDs of ~
the entities that reference them.  If #NNN is in (references entity) then (gethash #NNN comes-from) ~
will contain (entity-id entity)")

   (statements
    :accessor statements
    :type vector
    :initarg :statements
    :documentation "Original STEP statements as read from the file. step-entities use an index into this vector to get the original statement text.")))

(defmethod cl:print-object ((object step-file) stream)
  (with-slots (statements) object
    (format stream "~{~a~^~%~}" (coerce statements 'list))))


(defun entity (step-file entity-id)
  (gethash entity-id (entity-map step-file)))

(defun entity-references (step-file entity-id)
  (references (entity step-file entity-id)))

(defun entities-referenced-by (step-file entity-id)
  (gethash entity-id (comes-from step-file)))

(defun entities-of-type (step-file entity-type)
  (gethash (entity-type-map step-file) entity-type))

(defun statement-at (step-file idx)
  (aref (statements step-file) idx))

(defparameter *step-file-dirs* (list
                                (asdf:system-relative-pathname :step-grapher "step-files/")
                                (asdf:system-relative-pathname :step-grapher "step-files/large/")
                                (asdf:system-relative-pathname :step-grapher "step-files/nist-ap203/")
                                (asdf:system-relative-pathname :step-grapher "step-files/NIST-PMI-STEP-Files/")
                                (asdf:system-relative-pathname :step-grapher "step-files/NIST-PMI-STEP-Files/AP203 geometry only/")
                                (asdf:system-relative-pathname :step-grapher "step-files/NIST-PMI-STEP-Files/AP203 with PMI/")
                                "~/data/3d-models/step-files/")
  "Directories in which to look for STEP files.  Used to search if a full path is not used.")

(defun find-step-file (fname)
  "Search for a  file name (like \"vice.stp\" in th *step-file-dirs*"
  (ctypecase fname

    ;; If a string is given, search for the file.
    (string

     ;; First probe for it to see if it's an absolute path,
     ;; or a relative path that exists in the current directory
     (if-let ((path (probe-file fname)))
       path

       ;; It wasn't, so search *step-file-dirs* for it.
       (loop
         :for path :in *step-file-dirs*
         :do
            (when-let (the-file (probe-file (merge-pathnames fname path)))
              (return the-file))
         :finally (error "Could not find ~a" fname))))

    ;; If a pathname was given, just probe it to make sure it exists
    (pathname
     (probe-file fname))))

(defun read-step-file (fname &key (encoding :utf-8))
  "Read a step file into a list of entities.  The list will include *all* entities, even header
 and data entries."
  (with-input-from-file (ins (find-step-file fname) :external-format encoding)
    (loop
      :with header-statements = nil
      :with entities = (make-hash-table :test 'equal)
      :with comes-from = (make-hash-table :test 'equal)
      :with entity-types = (make-hash-table :test 'equal)
      :with in-header = nil
      :with in-data = nil
      :for statement = (read-step-statement ins)
      :for current-idx :from 0
      :while statement
      :collect statement :into all-statements

      ;; In the HEADER section, collect the statement indices in order to create a step-header.
      ;; This just allows quick access to those statements later on... they're not looked at too
      ;; carefully yet...
      :when (string= "HEADER" (token-at statement 0)) 
        :do
           (setf in-header t)
      :when in-header
        :do
           (push statement header-statements)
      :when (and in-header
                 (string= "ENDSEC" (token-at statement 0)))
        :do
           (setf in-header nil)

           ;; In the DATA section build up entities and add them to the entities map.
           ;; These will always have #ID = ...
      :when (and in-data
                 (string= "ENDSEC" (token-at statement 0)))
        :do
           (setf in-data nil)

      :when (and (not (string= "DATA" (token-at statement 0)))
                 in-data
                 (not (zerop (token-count statement))))
        :do
           (let* ((entity (create-entity statement current-idx))
                  (ent-type (entity-type entity))
                  (ent-id (entity-id entity) ))
             (setf (gethash ent-id entities) entity)
             (if (not (null (gethash ent-type entity-types)))
                      (push ent-id (gethash ent-type entity-types))
                      (setf (gethash ent-type entity-types) (list ent-id)))
             (loop
               :for goes-to-id :in (references entity)
               :do
                  (if (not (null (gethash goes-to-id comes-from)))
                      (push ent-id (gethash goes-to-id comes-from))
                      (setf (gethash goes-to-id comes-from) (list ent-id)))))
      :when (string= "DATA" (token-at statement 0))
        :do
           (setf in-data t)

           
           
      :finally (return (make-instance 'step-file
                                      :header (make-instance 'step-header :statements (coerce  (nreverse header-statements) 'vector))
                                      :statements all-statements
                                      :comes-from comes-from
                                      :entity-map entities
                                      :entity-type-map entity-types
                                      )))))

