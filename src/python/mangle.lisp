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

(in-package :motion-grammar-kit-python)

(defun python-mangle (str &optional package)
  "Convert a lisp symbol name to a valid python identifier.
PACKAGE: if given, intern the identifier in this package."
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
    (let ((name (replace (make-string (length list)) list)))
      (if package
          (intern name package)
          name))))

(defmacro def-clpython-package (lisp-package-name &optional
                                (python-package-name lisp-package-name))
"Define a python package that re-exports suitably mangled symbol from LISP-PACKAGE."
  (let* ((lisp-package-name (string lisp-package-name))
         (kw (find-package "KEYWORD")))
    `(defpackage ,(python-mangle python-package-name kw)
       (:use ,(intern lisp-package-name kw))
       (:export ,@(let ((e))
                       (do-external-symbols (s (find-package lisp-package-name))
                         (push (python-mangle s kw) e))
                       e)))))


(defun clpython-translate-arglist (arglist py-package)
"Convert symbols in arglist to valid python identifiers."
  (let ((original-args (make-hash-table)))
    (labels ((add-arg (arglist)
               (when-let (sym (etypecase (car arglist)
                                (symbol (car arglist))
                                (cons (caar arglist))
                                (nil)))
                 (check-type sym symbol)
                 (setf (gethash sym original-args) (python-mangle sym py-package))))
             (translate-expr (expr)
               (cond
                 ((null expr) nil)
                 ((atom expr)
                  (if-let (py-sym (gethash expr original-args))
                    py-sym
                    expr))
                 (t (cons (car expr)
                          (map 'list #'translate-expr (cdr expr))))))
             (translate-basic (arglist)
               (when arglist
                 (case (car arglist)
                   (&key
                    (cons '&key (translate-special (cdr arglist))))
                   ((&optional &rest)
                    (error "Can't translate ~A arguments for python" (car arglist)))
                   (otherwise
                    (add-arg arglist)
                    (cons (python-mangle (car arglist) py-package)
                          (translate-basic (cdr arglist)))))))
             (translate-special (arglist)
               (add-arg arglist)
               (when arglist
                 (cons (etypecase (car arglist)
                         (symbol
                          (python-mangle (car arglist) py-package))
                         (list
                          (list (python-mangle (caar arglist) py-package)
                                (translate-expr (cadar arglist)))))
                       (translate-special (cdr arglist))))))
      (translate-basic arglist))))

(defun clpython-translate-argcall (arglist py-package)
"Convert symbols in arglist to valid python identifiers."
  (labels ((translate-basic (arglist)
             (when arglist
               (case (car arglist)
                 (&key
                  (translate-key (cdr arglist)))
                 ((&optional &rest)
                  (error "Can't tranlate ~A arguments for python" (car arglist)))
                 (otherwise
                  (cons (python-mangle (car arglist) py-package)
                        (translate-basic (cdr arglist)))))))
           (translate-key (arglist)
             (when arglist
               (let ((sym (if (listp (car arglist))
                              (caar arglist)
                              (car arglist))))
                 (check-type sym symbol)
                 `(,(intern (symbol-name sym) (find-package "KEYWORD"))
                    ,(python-mangle sym py-package)
                    ,@(translate-key (cdr arglist)))))))
    (translate-basic arglist)))

(defun bind-clpython-function (lisp-sym py-package)
"Create a mangled binding of LISP-SYM in PY-PACKAGE.

May need to create a new DEFUN to make keyword arguments valid python
identifiers."
  (let ((arglist (swank-backend:arglist lisp-sym))
        (py-sym (python-mangle lisp-sym py-package)))
    (if (mg::finite-set-intersection '(&key) arglist)
        `(progn
           (declaim (inline ,py-sym))
           (defun ,py-sym ,(clpython-translate-arglist arglist py-package)
             (,lisp-sym ,@(clpython-translate-argcall arglist py-package))))
        `(setf (symbol-function (quote ,py-sym))
               (symbol-function (quote ,lisp-sym))))))

(defmacro bind-clpython-package (lisp-package-name &optional
                                 (python-package-name lisp-package-name))
"Bind all symbols exported from LISP-PACKAGE-NAME to suitably mangled
identifiers in PYTHON-PACKAGE-NAME."
    (let ((stmts)
          (python-package (find-package (python-mangle python-package-name)))
          (lisp-package (find-package lisp-package-name)))
      (assert (and python-package lisp-package))
      (do-external-symbols (sym lisp-package)
        (let ((lisp-sym (intern (string sym) lisp-package)))
          (when (fboundp lisp-sym)
            (push (bind-clpython-function lisp-sym python-package) stmts))))
      `(progn ,@stmts)))
