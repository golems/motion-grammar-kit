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

(in-package :motion-grammar)

;; Convert a FA to C Code
;; Each token is given by a C function.
;; Function returns 0 if token matches

(defun csymbol (gsymbol)
  (let ((name (string-downcase (gsymbol-name gsymbol))))
    (with-output-to-string (s)
      (labels ((putchars (c) (princ c s)))
        (loop for c across name
           do (case c
                (#\- (putchars #\_))
                (#\= (putchars "_equal_"))
                (#\Space (putchars "_sp_"))
                (#\( (putchars "_lp_"))
                (#\) (putchars "_rp_"))
                (otherwise (putchars c))))))))



(defun print-c-parser-predicate (stream symbol context)
  (cond
    ((atom symbol)
     (format stream "(~A) -> ~A" context (csymbol symbol)))
    ((and (listp symbol)
          (eq :not (car symbol)))
     (destructuring-bind (not clause) symbol
       (assert (eq not :not))
       (format stream "(!(~A) -> ~A)" context (csymbol clause))))
    (t (error "Unknown predicate ~A" symbol))))


(defun print-c-parser-funcall (stream symbol context)
  (if (and (listp symbol)
           (eq :predicate (car symbol)))
      (print-c-parser-predicate stream (cadr symbol) context)
      (format stream "~A( ~A )" (csymbol symbol) context)))

(defun print-c-parser-stub (stream symbol context-type context)
  (if (and (listp symbol)
           (eq :predicate (car symbol)))
      (format stream "~&/* No code for predicate symbol */~%")
      (format stream "~&static int ~A( ~A ~A ) {~%~%~%}~%~%"
              (csymbol symbol) context-type context)))



(defun fa->c-stub (fa &key
                   output
                   (halt-function "halt")
                   ;(static-functions t)
                   (context-type "void*")
                   (print-stub #'print-c-parser-stub))
  (output-function (lambda (s)
                     (format s "~&/************************************/~&")
                     (format s "~&/* STUB FUNCTIONS FOR MOTION PARSER */~&")
                     (format s "~&/************************************/~&")
                     (format s "~%/* Each function must return 0 for an occuring terminal and nonzero otherwise */~%~%")
                     (do-finite-set (z (finite-set-add (fa-terminals fa) halt-function))
                       (format s "~&/* Terminal Symbol function for \"~A\" */~&" z)
                       (funcall print-stub s z context-type "context")))
                       ;(format s "~&~:[~;static ~]int ~A( ~A context ) {~%~%~%}~%~%" static-functions (csymbol z) context-type)))
                   output))

(defun fa->c-parser (fa &key
                     output
                     (function-name "mgparse")
                     (header "")
                     (halt-function "halt")
                     (context-type "void*"))
  (let* ((fa (fa-canonicalize fa))
         (mover (dfa-mover fa)))
    (output-function (lambda (s)
                       (format s "~&/*****************/~&")
                       (format s "~&/* MOTION PARSER */~&")
                       (format s "~&/*****************/~&")
                       (when header (princ header s))
                       (format s "~&int ~A( ~A context ) {~&" function-name context-type)
                       (format s "~&	goto state~A;~&" (fa-start fa))
                       (do-finite-set (q0 (fa-states fa))
                         (format s "	state~A:~&" q0)
                         (do-finite-set (z (fa-terminals fa))
                           (let ((q1 (funcall mover q0 z)))
                             (when q1
                               (format s "~&		if( 0 == ~A ) goto state~A;~&"
                                       (print-c-parser-funcall nil z "context") q1))))
                         (when (finite-set-inp q0 (fa-accept fa))
                           (format s "~&		if( 0 == ~A( context ) ) return 0;~&" halt-function))
                         (format s "~&		return -1;~&"))
                       (format s "~&}~&"))
                     output)))


(defun fa->c-file (fa pathname &key
                   (function-name "mgparse")
                   (halt-function "halt")
                   (context-type "void*")
                   (print-stub #'print-c-parser-stub)
                   (stub-pathname (make-pathname :directory (pathname-directory pathname)
                                                 :name (concatenate 'string (pathname-name pathname) "-terminal-stub")
                                                 :type "c")))
  (fa->c-stub fa
              :output stub-pathname :halt-function halt-function :context-type context-type
              :print-stub print-stub)
  (fa->c-parser fa :output pathname
                :header (format nil "#include \"~A.c\" ~&" (pathname-name stub-pathname))
                :function-name function-name
                :halt-function halt-function
                :context-type context-type
                ))
