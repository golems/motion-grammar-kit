;;;; -*- Lisp -*-
;;;;
;;;; Copyright (c) 2013, Georgia Tech Research Corporation
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

;;;; Mangle some names and packages for CLPython

(in-package :motion-grammar-kit)

(defun python-mangle (str)
  (let ((list (loop
                 with str = (string str)
                 with len = (length str)
                 for i from 0 below len
                 for j = (1+ i)
                 for c = (aref str i)
                 collect
                   (cond
                     ((and (eq c #\-)
                           (< j len)
                           (eq (aref str j) #\>))
                      (incf i)
                      #\2)
                     ((eq c #\-)
                      #\_)
                     (t
                      (char-downcase c))))))
    (replace (make-string (length list)) list)))

(defmacro def-clpython-package (lisp-package-name &optional
                                (python-package-name (python-mangle lisp-package-name)))
  (let* ((lisp-package-name (string lisp-package-name))
         (kw (find-package "KEYWORD")))
    `(defpackage ,(intern (string python-package-name) kw)
       (:use ,(intern lisp-package-name kw))
       (:export ,@(let ((e))
                       (do-external-symbols (s (find-package lisp-package-name))
                         (push (intern (python-mangle s) kw) e))
                       e)))))

(defmacro bind-clpython-package (lisp-package-name &optional
                                 (python-package-name (python-mangle lisp-package-name)))
    (let ((stmts)
          (python-package (find-package python-package-name))
          (lisp-package (find-package lisp-package-name)))
      (do-external-symbols (sym lisp-package)
        (let ((lisp-sym (intern (string sym) lisp-package))
              (py-sym (intern (python-mangle sym) python-package)))
          (when (fboundp lisp-sym)
            (push `(symbol-function (quote ,lisp-sym)) stmts)
            (push `(symbol-function (quote ,py-sym)) stmts))))
      `(setf ,@stmts)))

;; TODO: use (swank-backend:arglist) to rewrite keyword argument names
;; so that we can actually use them in python-land

(def-clpython-package motion-grammar-kit "motion_grammar_kit_python")
(bind-clpython-package motion-grammar-kit "motion_grammar_kit_python")
