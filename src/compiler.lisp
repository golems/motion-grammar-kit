;;;; -*- Lisp -*-
;;;;
;;;; Copyright (c) 2012, Georgia Tech Research Corporation
;;;; All rights reserved.
;;;;
;;;; Author(s): Neil T. Dantam <ntd@gatech.edu>
;;;; Georgia Tech Humanoid Robotics Lab
;;;; Under Direction of Prof. Mike Stilman
;;;;
;;;; This file is provided under the following "BSD-style" License:
;;;;
;;;;   Redistribution and use in source and binary forms, with or
;;;;   without modification, are permitted provided that the following
;;;;   conditions are met:
;;;;   * Redistributions of source code must retain the above
;;;;     copyright notice, this list of conditions and the following
;;;;     disclaimer.
;;;;   * Redistributions in binary form must reproduce the above
;;;;     copyright notice, this list of conditions and the following
;;;;     disclaimer in the documentation and/or other materials
;;;;     provided with the distribution.
;;;;
;;;;   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND
;;;;   CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
;;;;   INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
;;;;   MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
;;;;   DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR
;;;;   CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;;;   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
;;;;   NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
;;;;   LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
;;;;   HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
;;;;   CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
;;;;   OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE,
;;;;   EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(in-package :motion-grammar-kit)

(defparameter *lexer-symbols*
  (append
   (list #\; #\= #\: #\- #\< #\> #\_ #\Space #\Newline #\Tab)
   (loop for c from (char-code #\a) to (char-code #\z) collect (code-char c))
   (loop for c from (char-code #\A) to (char-code #\Z) collect (code-char c))
   (loop for c from (char-code #\0) to (char-code #\9) collect (code-char c))))

(defun lexer (fa token-terminals)
  (let* ((fa1 (fa-canonicalize fa))
         (mover (dfa-mover fa1))
         (start (fa-start fa1)))
    (lambda (lex-in)
      (if (null (peek-char nil lex-in nil nil))
          (values nil nil)
          (let (token-type)
            (let* ((buffer (make-array 0 :adjustable t :fill-pointer t))
                   (text (with-output-to-string (lex-out)
                           (labels ((emit (q)
                                      (do-finite-set (z token-terminals)
                                        (when (funcall mover q z)
                                          (return-from emit z)))
                                      (error "Not valid token: ~A" buffer))
                                    (tokenize (q)
                                      (let ((c (peek-char nil lex-in nil nil)))
                                        (if (null c)
                                            (emit q)
                                            (let ((q1 (funcall mover q c)))
                                              (if (null q1)
                                                  (emit q)
                                                  (progn
                                                    (let ((c (read-char lex-in)))
                                                      (vector-push-extend c buffer)
                                                      (write-char c lex-out))
                                                    (tokenize q1))))))))
                             (setq token-type (tokenize start))))))
              (values token-type text)))))))


(defmacro deflexer (name &body forms)
  (alexandria:with-gensyms (lexer s)
    `(let ((,lexer
            (lexer (regex->dfa
                    ',(cons :union
                            (loop for (type regex) in forms
                               collect (list :concatenation
                                             (regex-simplify (regex-sweeten regex
                                                                            *lexer-symbols*
                                                                            :concatenate-strings t))
                                             type))))
                   ',(mapcar #'car forms))))
       (defun ,name (,s)
         (multiple-value-bind (type text)
             (funcall ,lexer ,s)
           (if (eq type :skip)
               (progn ;(format t "skip ~A~&" text)
                      (,name ,s))
               (values type text)))))))

(defun lex-stream (lexer stream)
  (loop for x = (multiple-value-list (funcall lexer stream))
     while (car x)
     collect (print x)))

(deflexer bnf-lexer
  (symbol (:+ (:union :alnum-class #\- #\_)))
  (equals (:union "=" ":=" "::=" "->"))
  (newline (:+ #\Newline))
  (alternate #\|)
  (:skip (:union (:+ :blank-class) (:concatenation #\; (:closure (:not #\Newline)) (:closure #\Newline)))))


(defun bnf-parse (stream)
  (let (rules)
    (labels ((next () (bnf-lexer stream))
             (head (type text)
               (assert (eq type 'symbol))
               (multiple-value-bind (equals text) (next)
                 (declare (ignore text))
                 (assert (eq equals 'equals)))
               (stuff type text))
             (stuff (type text)
               (assert (eq type 'symbol))
               (multiple-value-bind (body body-type body-text) (body)
                 (declare (ignore body-text))
                 (push (cons text body) rules)
                 (ecase body-type
                   ((nil) nil)
                   (alternate (stuff type text))
                   (newline
                    (multiple-value-bind (next-type next-text) (next)
                      (ecase next-type
                        (symbol (head next-type next-text))
                        (alternate (stuff type text))
                        ((nil) nil)))))))
             (body ()
               (multiple-value-bind (type text) (next)
                 (ecase type
                   (symbol (multiple-value-bind (next-body next-type next-text) (body)
                             (values (cons text next-body) next-type next-text)))
                   ((newline alternate nil)
                    (values nil type text))))))
      (multiple-value-call #'head (next)))
    (reverse rules)))
