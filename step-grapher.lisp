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

(defun count-leading-whitespace (string)
  (loop :for char :across string
        :for i :from 0
        :until (not (whitespace-p char))
        :finally (return i)))

(defun strip-leading-whitespace (string)
  (subseq string (count-leading-whitespace string)))

(defun read-entity-type (step-statement)
  (let ((right-hand-side (if (find #\= step-statement)
                             (subseq step-statement (1+ (search "=" step-statement)))
                             step-statement)))
    (cond ((char= (aref right-hand-side 0) #\()
           "unsupported-constraint")
          (t
           (symbol-name (read-from-string right-hand-side))))))

(defun parse-statement (step-statement)
  
  (cond
    ;; Check for statements starting with #number = ...
    ((char= #\# (aref step-statement 0))
     (loop :with entity-ids list = nil
           :with cur-entity-id fixnum = 0
           :with entity-type = (read-entity-type step-statement)
           :with in-string = nil
           :with in-entity-id = nil
           :for char :across step-statement
           :do
              (cond ((char= char #\')
                     (setf in-string (not in-string)))

                    ((and (not in-string)
                          (char= char #\#))
                     (setf in-entity-id t))
                    ((and in-entity-id (numeric-p char))
                     (setf cur-entity-id (+ (* cur-entity-id 10)
                                            (- (char-code char)
                                               (char-code #\0)))))
                    ((and in-entity-id
                          (> cur-entity-id 0))
                     (push cur-entity-id entity-ids)
                     (setf cur-entity-id 0
                           in-entity-id nil)))
           :finally (return (when entity-ids
                              (let ((result (reverse entity-ids)))
                                    
                                (make-instance 'step-entity
                                               :id (first result)
                                               :entity-type entity-type
                                               :references (rest result)))))))
    (t
     (make-instance 'step-entity
                    :id 0
                    :references nil
                    :entity-type (read-entity-type step-statement)))))

(defun read-step-statement (stream)
  (let ((chars (loop
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
        (parse-statement (strip-leading-whitespace (coerce chars 'string))))))


(defun whitespace-p (char)
  "Return t if a character is a whitespace, nil otherwise."
  (declare (type character char))
  #+sbcl (sb-unicode:whitespace-p char)
  #-sbcl (cl-unicode:has-property char "whitespace"))

(defun numeric-p (char)
  (find char #(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9) :test #'char=))

(defclass step-entity ()
  ((id :type fixnum
       :initarg :id
       :documentation "The entity ID.")
   (entity-type :type symbol
                :initarg :entity-type
                :documentation "The entity type (the identifier following the #123 = <entity-type> ( ... );)")
   (references :type list
               :initarg :references
               :documentation "The entity IDs that this entity references."))
  (:documentation "A dumbed down STEP entity."))

(defun substring-to (str char start &optional (skip-whitespace t))
  (let* ((real-start (if (not skip-whitespace)
                         start
                         (loop :for i = start
                               :until (or (> (length str) i)
                                          (not (whitespace-p (aref str i)))))))
         (idx (find char str :start real-start :test #'char=)))
    (subseq str real-start idx)))

(defun read-step-file (fname)
  (with-input-from-file (ins (find-step-file fname))
    (loop :for statement = (read-step-statement ins)
          :while statement
          :collect statement)))

(defun graph-step-file (step-file-name
                        &key
                          (dot-file-name (merge-pathnames (make-pathname :type "dot")
                                                          (sg:find-step-file step-file-name)))
                          (output-type "svg")
                          (out-file-name (merge-pathnames (make-pathname :type output-type)
                                                          (sg:find-step-file step-file-name)))
                          (open-file t)
                          (node-sep 0.4)
                          (spline-type "true")
                          (graph-cmd "dot")
                          (skip-list '("CARTESIAN_POINT"
                                       ;; "ORIENTED_EDGE"
                                       "DIRECTION"
                                       "LINE"
                                       "VECTOR"
                                       "VERTEX_POINT"
                                       ;; "EDGE_LOOP"
                                       )))
  (let ((step-pathname (find-step-file step-file-name)))
    (with-output-to-file (dots dot-file-name)
      (format dots "digraph ~s { rankdir=\"LR\"~%nodesep=~a~%overlap=false~%splines=~a~%" (namestring step-pathname) node-sep spline-type)
      (time
       (with-input-from-file (ins step-pathname)
         (let ((step-table (make-hash-table)))
           (loop :for entity = (read-step-statement ins)
                 :while entity
                 :for this-id = (slot-value entity 'id)
                 :when (and entity
                            (not (zerop this-id)))
                   :do
                      (setf (gethash this-id step-table) entity))
           (loop :for id :being :the :hash-keys :of step-table
                   :using (hash-value entity)
                 :do
                    (with-slots (id entity-type references) entity
                      (when (not (find entity-type skip-list :test #'string-equal))
                        (format dots "~s [label=\"~a(~a)\"];" id entity-type id)
                        (loop
                          :with from = id
                          :for goes-to :in references
                          :for goes-to-type = (slot-value (gethash goes-to step-table) 'entity-type)
                          :for should-skip = (find goes-to-type skip-list :test #'string-equal)
                          :when (not should-skip) :do
                            (format dots "~s -> ~s;~%" from goes-to)))))
           
           )))
      (format dots "}~%")))
  (let ((cmd (format nil
                     "~a -T~a -o ~s ~s"
                     graph-cmd
                     output-type
                     (namestring out-file-name)
                     (namestring dot-file-name))))
    (format t "Running: ~s~%" cmd)
    (time (uiop:run-program cmd :output t :error-output t :force-shell t))
    (cond ((stringp open-file)
           (uiop:run-program (format nil "~a ~s &" open-file (namestring out-file-name))))
          (open-file
           (let ((open-program (assoc-value '(("pdf" . "mupdf")
                                              ("svg" . "firefox")
                                              ("png" . "xnview")
                                              ("jpg" . "xnview"))
                                            output-type :test #'string-equal)))
             (when open-program
               (uiop:run-program (format nil "~a ~s" open-program (namestring out-file-name)))))))))
