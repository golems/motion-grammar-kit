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

(defun bnf-chop-ears (string)
  (subseq string 1 (1- (length string))))

(deflexer bnf-lexer
  (terminal (:concatenation #\" (:+ (:union :alnum-class #\- #\_)) #\")
            (symbol-function 'bnf-chop-ears))
  (nonterminal (:concatenation #\< (:+ (:union :alnum-class #\- #\_)) #\>)
               (symbol-function 'bnf-chop-ears))
  (equals (:union "=" ":=" "::=" "->"))
  (newline (:+ #\Newline))
  (alternate #\|)
  (:skip (:union (:+ :blank-class) (:concatenation #\; (:closure (:not #\Newline)) (:closure #\Newline)))))


(defun bnf-parse (stream)
  (let (rules)
    (labels ((next () (bnf-lexer stream))
             (head (type text)
               (assert (eq type 'nonterminal))
               (multiple-value-bind (equals text) (next)
                 (declare (ignore text))
                 (assert (eq equals 'equals)))
               (stuff type text))
             (stuff (type text)
               (assert (or (eq type 'terminal)
                           (eq type 'nonterminal)))
               (multiple-value-bind (body body-type body-text) (body)
                 (declare (ignore body-text))
                 (push (cons text body) rules)
                 (ecase body-type
                   ((nil) nil)
                   (alternate (stuff type text))
                   (newline
                    (multiple-value-bind (next-type next-text) (next)
                      (ecase next-type
                        (nonterminal (head next-type next-text))
                        (alternate (stuff type text))
                        ((nil) nil)))))))
             (body ()
               (multiple-value-bind (type text) (next)
                 (ecase type
                   ((terminal nonterminal)
                    (multiple-value-bind (next-body next-type next-text) (body)
                      (values (cons text next-body) next-type next-text)))
                   ((newline alternate nil)
                    (values nil type text))))))
      (multiple-value-call #'head (next)))
    (reverse rules)))


(defun save-bnf (grammar filespec &key if-exists)
"Save GRAMMAR to a file in Backus-Naur Form.

IF-EXISTS: one of :error, :new-version, :rename, :rename-and-delete,
 :overwrite, :append, :supersede, or nil."
  (with-open-file (s filespec :direction :output :if-exists if-exists)
    (grammar-print grammar :output s)))


(defun load-bnf (filespec)
"Load a grammar from a file in Backus-Naur Form."
  (with-open-file (s filespec :direction :input)
    (bnf-parse s)))
