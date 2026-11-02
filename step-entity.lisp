;; Copyright (c) 2026 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

(in-package :step-grapher)


;; This class models a generic STEP entity from the DATA section.
;; Typically these will be of the form:
;; #<NNN> = SOME_ENTITY_TYPE ( 'maybe name' , 3.4 , #123 )
;; #<NNN> is the entity ID of the entity
;; The elements between the outer-most '(' and ')', separated by ',' are
;; the parameters of the entity.
;; The first parameter after a '(' is *sometimes* the name of the entity.

(defclass step-entity ()
  ((id
    :accessor entity-id
    :type fixnum
    :initarg :id
    :documentation "The entity ID.")

   (index
    :accessor entity-index
    :type fixnum
    :initarg :index
    :documentation "The entity's index into the step-file's step-statement vector.")

   (entity-type
    :accessor entity-type
    :type symbol
    :initarg :entity-type
    :documentation "The entity type, converted to a Common Lisp keyword.")

   (statement
    :accessor statement
    :type step-statement
    :initarg :statement
    :documentation "The original statement that created this entity.")

   (references
    :accessor references
    :type list
    :initarg :references
    :documentation "The entity IDs that this entity references."))

  (:documentation "STEP entity base class."))

(defmethod print-object ((object step-entity) stream)
  (format stream "~a" (statement object)))

(defun entity-id-integer (ent-id-string)
  "Read the integer value of a STEP reference ID in a string of the form #NNN"
  (declare (type string ent-id-string)
           (optimize (speed 3)))
  (with-input-from-string (ins (subseq ent-id-string 1))
    (the fixnum (read ins))))


(defun decide-entity-type (statement)
  "The entity type (the identifier string following the ID.
  #123 = <entity-type> ( ... );)"
  (intern (substitute #\-
                      #\_
                      (if (string= "(" (token-at statement 2))
                          (token-at statement 3)
                          (token-at statement 2))
                      :test #'char=)
          'keyword))

(defun step-reference-p (token)
  (when (and
         (> (length token) 1)
         (char= (aref token 0) #\#)
         (numeric-p (aref token 1)))
    (entity-id-integer token)))

(defun create-entity (statement index)
  "Parse the sequence of string tokens in statement into a semantic STEP entity.

index is this entity's index in the owning step-file's statements vector."
  (make-instance
   'step-entity
   :id (entity-id-integer (token-at statement 0))
   :index index
   :statement statement
   :entity-type (decide-entity-type statement)
   ;; (skip the '=')
   :references
   (loop :for idx :from 0
         :for token :across (slot-value statement 'tokens)
         :for is-ref = (and
                        (>  idx 0)
                        (step-reference-p token))
         :when is-ref
           :collect is-ref)))

(defun entity-name (entity)
  (let ((statement (statement entity)))
    (if (and (> (token-count statement))
             (char= #\' (aref (token-at statement 4) 0)))
        (token-at statement 4)
        nil)))
