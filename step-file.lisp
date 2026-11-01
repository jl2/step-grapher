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

(defparameter *print-statements* nil
  "If nil print only the step header.  If non-nil print statements as well.")

(defclass step-file ()
  ((pathname
    :accessor step-pathname
    :type pathname
    :initarg :pathname
    :documentation "The pathname of the STEP file this object was read from.")

   (header
    :accessor header
    :type step-header
    :initarg :header
    :documentation "The STEP header for the STEP file.")

   (entity-map
    :accessor entity-map
    :type hashtable
    :initarg :entity-map
    :initform (make-hash-table)
    :documentation "A hashtable mapping entity IDs to corresponding entity-statments.")

   (entity-type-map
    :accessor entity-type-map
    :type hashtable
    :initarg :entity-type-map
    :initform (make-hash-table :test 'eq)
    :documentation "A hashtable mapping entity-types to a list of entity IDs of that type.")

   (comes-from
    :accessor comes-from
    :initform (make-hash-table)
    :initarg :comes-from
    :type hashtable
    :documentation "A hashtable mapping entity IDs (of the form \"#NNNN\") to a list of entity IDs of ~
the entities that reference them.  If #NNN is in (references entity) then (gethash #NNN comes-from) ~
will contain (entity-id entity)")

   (statements
    :accessor statements
    :type vector
    :initarg :statements
    :documentation "Original STEP statements as read from the file. step-entities use an index into this vector to get the original statement text.")
   (dereference-cache
    :accessor dereference-cache
    :type hashtable
    :initform (make-hash-table :test 'eq)
    :documentation "A cache of 'dereferenced' entities.")))

(defmethod cl:print-object ((object step-file) stream)
  (if *print-statements*
    (loop
      :for statement :across (statements object)
      :do (format stream "~a~%" statement))
    (format stream "~a" (header object))))


(defun entity (step-file entity-id)
  "Return the entity with the specified ID."
  (gethash entity-id (entity-map step-file)))

(defun entity-references (step-file entity-id)
  "Return the IDs of entities referenced by the entity with the specified ID."
  (references (entity step-file entity-id)))

(defun entities-referenced-by (step-file entity-id)
  "Return the IDs of entities that reference the specified entity."
  (gethash entity-id (comes-from step-file)))

(defun entities-of-type (step-file entity-type)
  "Return a list of entity IDs of the specified type."
  (gethash entity-type (entity-type-map step-file)))

(defun get-entities (step-file entity-id-list)
  "Multiple value version of `entity`."
  (with-slots (entity-map) step-file
    (mapcar (lambda (x)
              (gethash x entity-map))
            (ensure-list entity-id-list))))

(defun statement-at (step-file idx)
  "Return a statement by index."
  (aref (statements step-file) idx))

(defun entity-types (step-file)
  (with-slots (entity-type-map) step-file
    (loop :for entity-type :being :the hash-keys
            :of entity-type-map
          :collecting entity-type)))
(defun entity-ids (step-file)
  (with-slots (entity-map) step-file
    (loop :for entity-id :being :the hash-keys
            :of entity-map
          :collecting entity-id)))
(defun fetch-entities-of-type (step-file entity-type)
  (loop :for ent :in (entities-of-type step-file entity-type)
        :collecting (entity step-file ent)))

(defun dereference (step-file entity-id &key (depth 100))
  "Lookup entity-id in the step-file and recursively expand its references up to depth levels."
  (with-slots (dereference-cache) step-file
    (multiple-value-bind (value existed) (gethash entity-id dereference-cache)
      (cond (existed
             value)
            (t
             (let ((the-value
                     (with-slots (tokens) (slot-value (entity step-file entity-id) 'statement)
                       (loop
                         :for token :across  (subseq tokens 2)
                         :for is-ref = (step-reference-p token)
                         :for next-collected-entity
                           = (cond

                               ;; If it's a reference and above depth then recurse
                               ((and is-ref (not (zerop depth)))
                                (dereference step-file
                                             is-ref
                                             :depth (max 0
                                                         (- depth 1))))

                               ;; :true and :false enums
                               ;; Should these be special cased?
                               ;; Or should they be special cased even *more* into 't and 'nil ?
                               ((string= token ".T.")
                                :true)
                               ((string= token ".F.")
                                :false)

                               ;; Enumerations other than .T. and .F.
                               ((char= #\. (aref token 0))
                                (intern (subseq token 1 (- (length token) 1)) :keyword))

                               ;; A string
                               ((char= #\' (aref token 0))
                                (subseq token 1 (- (length token) 1)))

                               ;;
                               ((string= token "*")
                                (intern token :keyword))
                               ((and (not is-ref)
                                     (or (zerop depth)
                                         (null is-ref))
                                     (not (string= "(" token))
                                     (not (string= ")" token))
                                     (not (string= "," token)))
                                (let* ((key-str (substitute #\-
                                                            #\_
                                                            token
                                                            :test #'char=))
                                       (val (read-from-string key-str)))
                                  (if (symbolp val)
                                      (intern key-str
                                              'keyword)
                                      val))))
                         :when next-collected-entity
                           :collect next-collected-entity))))
               (setf (gethash entity-id dereference-cache) the-value)
               the-value))))))


(defun translate-language (step-file-path
                           &key
                             (input-encoding :utf-8)
                             (output-encoding :utf-8)
                             (source-language :en)
                             (target-language :ja)

                             )
  "Translate the STEP entities in step-file between human languages."
  (let ((output-file-path (make-pathname :defaults step-file-path
                                         :name (format nil "~a-~a" (pathname-name step-file-path)
                                                       target-language))))
    (alexandria:with-output-to-file (outf output-file-path :external-format output-encoding)
      (alexandria:with-input-from-file (inf step-file-path :external-format input-encoding)
        (let ((sg::*target-language* target-language)
              (sg::*source-language* source-language)
              (sg::*print-statements* t))
          (print (sg:read-step inf step-file-path) outf))))))

(defun summarize (step-file
                  &key
                    (entity-count nil)
                    (full t)
                    (stream t)
                    )
  "Write some information about step-file to stream."
  (with-slots (header entity-map entity-type-map comes-from) step-file
    (let ((sortted-entity-list (mapcar #'translate-token (sort (hash-table-keys entity-type-map)
                                                               #'>
                                                               :key (lambda (key)
                                                                      (length (gethash key entity-type-map)))))))
      (format stream "Step file has:~%")
      (format stream "  ~a entities~%" (hash-table-count entity-map))
      (format stream "  ~a entity types~%" (hash-table-count entity-type-map))
      (cond ((and entity-count full)

             (format stream "Top ~a Entity types by popularity:~%~{  ~a~^~%~}~%"
                     entity-count
                     (subseq sortted-entity-list
                             0
                             entity-count)))
            (full
             (format stream "Entity types by popularity:~%~{  ~a~^~%~}~%"
                     sortted-entity-list))))))

(defparameter *step-file-dirs* (list
                                (asdf:system-relative-pathname :step-grapher "step-files/")
                                (asdf:system-relative-pathname :step-grapher "step-files/large/")
                                (asdf:system-relative-pathname :step-grapher "step-files/nist-ap203/")
                                (asdf:system-relative-pathname :step-grapher "step-files/NIST-PMI-STEP-Files/")
                                (asdf:system-relative-pathname :step-grapher "step-files/NIST-PMI-STEP-Files/AP203 geometry only/")
                                (asdf:system-relative-pathname :step-grapher "step-files/NIST-PMI-STEP-Files/AP203 with PMI/")
                                "~/data/3d-models/step-files/")
  "Directories in which to look for STEP files.  Used to search if a full path is not used.")

(defun find-step-file (file-name)
  "Return a pathname of a STEP file named file-name.~
If file-name is a string, search for it in *step-file-dirs* and return a pathname if found.~
If file-name is a pathname, returns (probe-file file-name)."
  (ctypecase file-name

    ;; If a string is given, search for the file.
    (string

     ;; First probe for it to see if it's an absolute path,
     ;; or a relative path that exists in the current directory
     (if-let ((path(probe-file file-name)))
       path

       ;; It wasn't, so search *step-file-dirs* for it.
       (loop
         :for path :in *step-file-dirs*
         :do
            (when-let (the-file (probe-file (merge-pathnames file-name path)))
              (return the-file))
         :finally (error "Could not find ~a" file-name))))

    ;; If a pathname was given, just probe it to make sure it exists
    (pathname
     (probe-file file-name))))

(defun read-step (stream
                  &optional (file-path (make-pathname :name "file"
                                                      :type "step")))
  "Read a STEP file from "
  (loop
    ;; Build some data structures to help navigate the STEP file
    ;; List of statements in the header
    :with header-statements = nil
    ;; map of ID -> entity
    :with entities = (make-hash-table)
    ;; map of ID -> list IDs that reference it
    :with comes-from = (make-hash-table)
    ;; map of entity type -> list ID of entities of that type
    :with entity-types = (make-hash-table :test 'equal)

    ;; Parser states
    ;; Is parser currently in header?
    :with in-header = nil

    ;; Is parser currently in data section?
    :with in-data = nil

    ;; Loop over the statements with the index (aka count of current item)
    :for statement = (read-step-statement stream)
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
                                    :pathname file-path
                                    :header (make-instance 'step-header :statements (coerce  (nreverse header-statements) 'vector))
                                    :statements (coerce  all-statements 'vector)
                                    :comes-from comes-from
                                    :entity-map entities
                                    :entity-type-map entity-types
                                    ))))

(defun read-step-file (fname &key (encoding :utf-8))
  "Read a step file into a step-file object."
  (let ((pname (find-step-file fname)))
    (with-input-from-file (ins pname :external-format encoding)
      (read-step ins pname))))

(defun show-inspector (thing &key (in-emacs t) (wait nil))
  (declare (ignorable wait))
  (let ((has-swank (and #+swank t #-swank nil)))
    (cond ((and in-emacs has-swank)
           #+swank (swank:inspect-in-emacs thing
                                           :wait wait)
           #-swank (inspect thing))
          (t (inspect thing)))))

(defun inspect-entities (step-file &rest object-types)
  (show-inspector
   (apply #'concatenate 'list
          (mapcar (lambda (ot)
                    (mapcar
                     (lambda (ref)
                       (dereference step-file ref))
                     (entities-of-type step-file ot)))
                  (ensure-list object-types)))
   :wait nil))
