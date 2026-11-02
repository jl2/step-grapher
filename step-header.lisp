;; Copyright (c) 2026 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

(in-package :step-grapher)

(defclass step-header ()
  ((statements
    :accessor statements
    :type vector
    :initarg :statements
    :documentation "A vector of fixnum indices into the owning STEP file's statement index.")
   ))

(defmethod print-object ((obj step-header) stream)
  (loop
    :for statement :across (statements obj)
    :do
       (format stream "~a~%" statement)))
