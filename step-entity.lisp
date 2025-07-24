;; step-entity.lisp
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

(defun entity-id-integer (ent-id)
  (with-input-from-string (ins (subseq ent-id 1))
    (the integer (read ins))))

(defun entity-type (entity)
  "The entity type (the identifier string following the ID.
  #123 = <entity-type> ( ... );)"
  (with-slots (statement) entity
    (if (string= "(" (token-at statement 2))
        (token-at statement 3)
        (token-at statement 2))))

(defun create-entity (statement index)
  (make-instance
   'step-entity
   :id (token-at statement 0)
   :index index
   :statement statement
   ;; (skip the '=')
   :references (loop :for idx :from 0
                     :for token :across (slot-value statement 'tokens)
                     :when (and
                            (>  idx 0)
                            (> (length token) 1) 
                            (char= (aref token 0) #\#)
                            (numeric-p (aref token 1)))
                       :collect token)))

(defun entity-name (entity)
  (let ((statement (statement entity)))
    (if (and (> (token-count statement))
             (char= #\' (aref (token-at statement 4) 0)))
        (token-at statement 4)
        nil)))
