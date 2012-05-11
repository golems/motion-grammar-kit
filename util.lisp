;;;; -*- Lisp -*-
;;;;
;;;; Copyright (c) 2011-2012, Georgia Tech Research Corporation
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

;;; Utils

(in-package :motion-grammar)


(defun intersectionp (a b &optional (test #'eql))
  (map nil (lambda (a)
             (map nil (lambda (b)
                        (when (funcall test a b)
                          (return-from intersectionp t)))
                  b))
       a))



(defun multiple-value-reduce (function sequence &key initial-value-list)
  (let ((result initial-value-list))
    (map nil
         (lambda (&rest rest)
           (setq result (multiple-value-list
                         (apply function (append result rest)))))
         sequence)
    (apply #'values result)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; HIGHER ORDER FUNCTIONS ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; the CL-standards are a bit clumsy for this

(defun curry (function arg0)
"Return a new unary function which applies arg0 as leftmost argument of FUNCTION."
  (lambda (arg1) (funcall function arg0 arg1)))

(defun curry-right (function arg1)
"Return a new unary function which applies arg0 as rightmost argument of FUNCTION."
  (lambda (arg0) (funcall function arg0 arg1)))

(defun curry-list (function &rest initial-args)
"Return a new vararg function which applies INITIAL-ARGS as leftmost arguments of FUNCTION."
  (lambda (&rest final-args) (apply function (append initial-args final-args))))

(defun chain (value &rest functions)
  (if functions
      (apply #'chain
             (funcall (car functions) value)
             (cdr functions))
      value))

(defun fold (function initial-value &rest lists)
  (let ((value initial-value))
    (apply #'map nil
           (lambda (&rest args)
             (setq value (apply function value args)))
           lists)
    value))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; GENERALIZED SYMBOLS ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(defun gsymbol-name (gsym)
  (format nil "~A" gsym))

(defun gsymbol-gen (gsym &optional (unique (gensym)))
  (cons gsym unique))


(defun gsymbol-compare (a b)
  (cond
    ((and (numberp a) (numberp b))
     (< a b))
    ((numberp a) t)
    ((numberp b) nil)
    (t (string< (string a) (string b)))))

(defun gsymbol-list-compare (a b)
  (cond
    ((and (atom a) (atom b)) (gsymbol-compare a b))
    ((atom a) t)
    ((atom b) nil)
    ((null a) t)
    ((null b) t)
    (t (if (equal (car a) (car b))
           (gsymbol-list-compare (cdr a) (cdr b))
           (gsymbol-list-compare  (car a) (car b))))))



;;;;;;;;;
;; I/O ;;
;;;;;;;;;

#+sbcl
(defun output-dot-file (output function lang)
  "Run `dot' on the output of FUNCTION.
OUTPUT: output filename
FUNCTION: (lambda (stream)) => nil, prints dot on STREAM
LANG: language output for dot, (or pdf ps eps png)"
  (let ((p (sb-ext:run-program "dot" (list (concatenate 'string "-T" lang))
                               :wait nil :search t :input :stream :output output
                               :if-output-exists :supersede)))
    (funcall function (sb-ext:process-input p))
    (close (sb-ext:process-input p))
    (sb-ext:process-wait p)
    (sb-ext:process-close p)))

(defun output-dot (output function &optional (lang (and (stringp output) (car (last (ppcre:split "\\." output))))))
  "Produce graphiz output, dispatching on type of OUTPUT.
OUTPUT:  (or filename stream t nil)
FUNCTION: (lambda (stream)) => nil, prints dot text on STREAM
LANG: language output for dot, (or pdf ps eps png)"
  (cond
    ((streamp output)
     (funcall function output))
    ((null output)
     (with-output-to-string (s) (output-dot s function)))
    ((eq output t) (output-dot *standard-output* function))
    ((stringp output)
     (output-dot-file output function lang))
    (t (error "Unknown output: ~A" output))))
