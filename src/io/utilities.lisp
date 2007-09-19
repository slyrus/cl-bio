;;; Parsing utilities
;;;
;;; Copyright (c) 2006 Cyrus Harmon (ch-lisp@bobobeach.com)
;;; All rights reserved.
;;;
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:
;;;
;;;   * Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.
;;;
;;;   * Redistributions in binary form must reproduce the above
;;;     copyright notice, this list of conditions and the following
;;;     disclaimer in the documentation and/or other materials
;;;     provided with the distribution.
;;;
;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;

(in-package :bio-io)

(defun check-end-of-sequence (stream &key (end-char #\>))
  (let ((char (peek-char t stream nil :eof)))
    (when (or (equal char end-char)
              (equal char :eof))
      :eof)))

(defun read-until-char (stream end-char)
  (flet ((read-char-if-not (stream end-char)
           (unless (eq (check-end-of-sequence stream :end-char end-char) :eof)
             (read-char stream))))
    (let ((l))
      (do ((char (read-char-if-not stream end-char) (read-char-if-not stream end-char)))
          ((null char))
        (push char l))
      (coerce (nreverse l) 'string))))

(defun check-end-of-file (stream)
  (equal (peek-char t stream nil :eof) :eof))

(defparameter *line-ending-chars* '(#\Newline #\Return))

(defun read-dos-line (stream)
  (let ((l))
    (do ((char (read-char stream) (read-char stream)))
        ((or (null char)
             (member char *line-ending-chars*)))
      (push char l))
    (let ((p (peek-char nil stream)))
      (cond ((char-equal p #\Newline)
             (read-char stream))))
    (coerce (nreverse l) 'string)))

(defun check-end-of-line (stream)
  (member (peek-char nil stream nil #\Newline) *line-ending-chars*))

(defun remove-initial-spaces (string)
  (string-left-trim '(#\Space #\Tab #\Newline) string))

(defun remove-trailing-spaces (string)
  (string-right-trim '(#\Space #\Tab #\Newline) string))
  
