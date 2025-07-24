;; step-statement.lisp
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

(defclass step-statement ()
  ((tokens
    :accessor tokens
    :type vector
    :initarg :tokens
    :documentation "The string tokens that make up the statement.."
    )))

(defmethod cl:print-object ((object step-statement) stream)
  (with-slots (tokens) object
    (format stream "~{~a~^ ~}" (coerce tokens 'list))))

(defun token-count (statement)
  (length (slot-value statement 'tokens)))

(defun token-at (statement n)
  (aref (slot-value statement 'tokens) n))


(defun read-step-statement (stream)
  "Read the next statement from stream and try to parse it into a step-statement. ~
Returns nil at end of file. ~
Condenses contiguous white space in the statement down to one space."
  (declare (optimize (speed 3)))
  (let* (
         (+token-seperators+ '(
                               #\(
                               #\)
                               #\=
                               ;;#\#
                               #\,
                               #\;
                               ))
         (+statement-end+ #\;)
         (tokens
           (loop
             :with current-token = (make-array 0 :adjustable t :fill-pointer 0 :element-type 'character)
             ;; previous-was-whitespace and is-whitespace are used to track
             ;; whitespace/non-whitespace boundaries.
             :for previous-was-whitespace :of-type (or t nil) = nil
               :then is-whitespace
             
             :for prev-char = nil
               :then next-char

             ;; Initially tell peek-char to skip white space so we don't have to.
             ;; After that we need to condense it into single spaces, so read it and handle it below
             ;; EOF isn't an error
             ;; Return nil at eof instead.
             ;; And it's not a recursive call to read.
             :for next-char = (peek-char t stream nil nil nil)
               :then (peek-char nil stream nil nil nil)

             ;; Quit when the input is finished
             :while next-char

             ;; Keep track of string literals
             :for was-in-string = nil :then in-string

             ;; Swap in-string state if there's a ' that isn't quoted with a slash
             :for in-string = nil
               :then (if (and (char= next-char #\')
                              (not (char= prev-char #\\)))
                         (not in-string)
                         in-string)

             :for in-comment = nil
               :then  (and (or (and (char= prev-char #\/)
                                    (char= next-char #\*))
                               in-comment)
                           (not end-comment))
             :for end-comment = nil
               :then (and in-comment
                          (char= prev-char #\*)
                          (char= next-char #\/))
             
             ;; Keep track of white space boundaries
             :for is-whitespace = (or
                                   in-comment
                                   (whitespace-p next-char)
                                   )

             ;; Was the previous character a token seperator?
             ;; If so, this is a new token
             :for was-token-sep = nil :then (or token-sep
                                                ;; in-comment
                                                )

             ;; Is this character a token seperator?
             ;; If so, then this token needs to be collected
             :for token-sep = (or
                               ;;in-comment
                               (not
                                (null
                                 (find next-char +token-seperators+))
                                ))


             ;; Statements are terminated by semicolons outside of a string literal
             :for the-end = (and (not in-string)
                                 (char= next-char +statement-end+))

             ;; Tokens are terminated when:
             ;; * A string is entered or exited
             ;; * Whitespace occurs outside of a string adjacent to non-whitespace
             ;; * A token seperator is found and the previous token wasn't a seperator
             ;; * A token seperator *was* found, but the current one isn't
             :for token-end = (or (and (not was-in-string)
                                       in-string)
                                  ;; in-comment
                                  (and is-whitespace
                                       (not in-string)
                                       (not previous-was-whitespace))
                                  (and 
                                   token-sep (not was-token-sep))
                                  was-token-sep)

             ;; :when in-comment
             ;;   :do
             ;;      (loop 
             ;;        :for pc = next-char
             ;;          :then nc
                    
             ;;        :for nc = (peek-char t stream nil nil nil)
             ;;          :then (peek-char nil stream nil nil nil)
             ;;        :until (and (char= pc #\*)
             ;;                    (char= nc #\/))
             ;;        :do
             ;;           (read-char stream nil nil))
             ;;      (read-char stream nil nil)

                  ;; Collect the last token at the end, if it's non-zero length.
             :when (and the-end
                        ;; (not in-comment)
                        (not (zerop (length current-token)))
                        (not end-comment))
                 :collect (coerce current-token 'string)

             ;; Stop at the end
             :until the-end

             ;; Collect the previous token if it's non-zero length
             :when (and token-end
                         (not in-comment)
                        (not (zerop (length current-token)))
                        (not end-comment))
               :collect (coerce current-token 'string)

             ;; :when in-comment
             ;;   :do (format t "in-comment: ~a~%")
               ;; :do
               ;;    (format t "in-comment: ~a pc ~a nc ~a~%" in-comment prev-char next-char)
               ;; Condense whitespace
               ;; Collect a single space if the current character is white space, but
               ;; the previous character wasn't
               :when token-end
               :do
                  (setf current-token (make-array 0 :adjustable t :fill-pointer 0 :element-type 'character))

             ;; After peeking, read over whitespace unless it's in a string or comment.
             :when (and
                        (not in-string)
                        is-whitespace)
               :do (read-char stream nil nil)
             ;; :when (not (and in-comment end-comment))
             ;;   :do
             ;;      (read-char stream nil nil)
                  ;; Otherwise, collect all characters inside of string literals, and any other
                  ;; non-white space characters
             :when (or in-string
                       (not is-whitespace))
               :do
                  ;; We've peeked and *know* there is something here, so error if there isn't for some reason.
                  (let ((next-char (read-char stream)))
                    (vector-push-extend next-char current-token))
             :finally (read-char stream nil nil))))
    (when (not (zerop (length tokens)))
      (make-instance 'step-statement :tokens (coerce tokens 'vector)))))

