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
                               "~/data/3d-models/")
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



(parseq:defrule step-opening () "ISO-10303-21")
(parseq:defrule step-closing () "END-ISO-10303-21")
(parseq:defrule header () "HEADER")
(parseq:defrule data () "DATA")
(parseq:defrule end-sec () "ENDSEC")
(parseq:defrule semi-colorn () #\;)
(parseq:defrule step-string () (and #\' :string #\' ))
(parseq:defrule step-boolean () (or ".T." ".F."))
(parseq:defrule step-number () number)
(parseq:defrule entity-id () (and #\# integer))
(parseq:defrule definition () (and entity-id #\=))
(parseq:defrule statement () (and (or header data end-sec)
                                  ";"))
(parseq:defrule step-file () (and step-opening semi-colon
                                  header semi-colon
                                  end-sec semi-colon
                                  data semi-colon
                                  
                                  end-sec semi-colon
                                  step-closingsemi-colon) (rep * statement))

(defun read-step-file (fname)
  (let ((text-of-file (alexandria:read-file-into-string (find-step-file fname))))
    (values (parseq:parseq 'step-file text-of-file)
            text-of-file)))
