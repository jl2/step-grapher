;; Copyright (c) 2026 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

(in-package :cl-user)
(defpackage :step-grapher.test
  (:use :cl
        :fiveam
        :alexandria
        :step-grapher))

(in-package :step-grapher.test)

(def-suite :step-grapher)
(in-suite :step-grapher)

(test read-step-iso-header
  (with-input-from-string (ins "ISO-10303-21;")
    (let ((result (sg:read-step-statement ins)))
      (is-true (string= (slot-value result 'entity-type)
                        "ISO-10303-21"))))
  
  )

(test read-entity
  (with-input-from-string (ins "#10=GEOMETRICALLY_BOUNDED_WIREFRAME_SHAPE_REPRESENTATION(
'nist_ftc_06_asme1_rd-None',(#95),#4991);
")
    (let ((result (sg:read-step-statement ins)))
      (is (string= (slot-value result 'entity-type)
                        "GEOMETRICALLY_BOUNDED_WIREFRAME_SHAPE_REPRESENTATION"))
      (is (= 10 (slot-value result 'id)))
      (is (= 2 (length (slot-value result 'references))))
      (is-true (find 95 (slot-value result 'references)))
      (is-true (find 4991 (slot-value result 'references)))
      )))
