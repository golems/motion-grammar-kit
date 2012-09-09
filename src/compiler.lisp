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
   (list #\" #\' #\; #\= #\: #\- #\< #\> #\_ #\Space #\Newline #\Tab)
   (loop for c from (char-code #\a) to (char-code #\z) collect (code-char c))
   (loop for c from (char-code #\A) to (char-code #\Z) collect (code-char c))
   (loop for c from (char-code #\0) to (char-code #\9) collect (code-char c))))

(defun lexer (fa token-terminals transform-functions)
  (let* ((fa1 (fa-canonicalize fa))
         (mover (dfa-mover fa1))
         (start (fa-start fa1)))
    (lambda (lex-in)
      (if (null (peek-char nil lex-in nil nil))
          (values nil nil)
          (let (token-type token-tf)
            (let* ((buffer (make-array 0 :adjustable t :fill-pointer t))
                   (text (with-output-to-string (lex-out)
                           (labels ((emit (q)
                                      (loop
                                         for z in token-terminals
                                         for tf in transform-functions
                                         do (when (funcall mover q z)
                                              (setq token-type z
                                                    token-tf tf)
                                              (return-from emit)))
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
                             (tokenize start)))))
              (values token-type (funcall token-tf text))))))))


(defmacro deflexer (name &body forms)
  (alexandria:with-gensyms (lexer s)
    `(let ((,lexer
            (lexer (regex->dfa
                    ',(cons :union
                            (loop for (type regex &rest rest) in forms
                               collect (list :concatenation
                                             (regex-simplify (regex-sweeten regex
                                                                            *lexer-symbols*
                                                                            :concatenate-strings t))
                                             type))))
                   ',(mapcar #'car forms)
                   (list ,@(loop for form in forms
                              collect (or (third form) '(symbol-function 'identity)))))))
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
