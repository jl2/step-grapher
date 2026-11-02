;; Copyright (c) 2026 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

(in-package :step-grapher)

(defun whitespace-p (char)
  "Return t if a character is a whitespace, nil otherwise."
  (declare (type character char))
  #+sbcl (sb-unicode:whitespace-p char)
  #+clisp (find char (list #\space #\tab #\newline) :test #'char=)
  #-(or sbcl clisp) (cl-unicode:has-property char "whitespace"))

(defun numeric-p (char)
  "Check if char is a digit 0-9."
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (find char #(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9) :test #'char=))
