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

(defun ensure-progn (body)
  (if (cdr body)
      (cons 'progn body)
      (car body)))

;; GOAL: rewrite a string/list/expression based on a Context-Sensitive grammar

;; (:not (:not x)) := x
;; (:implies x y) := (:or (:not x) y)


;; Pattern Grammar
;; ---------------
;; Pattern is a test with a set of bindings
;;
;; pattern := symbol                   { bind expresson to SYMBOL }
;;            t                        { true }
;;            nil                      { match nil }
;;            number                   { = NUMBER }
;;            keyword-symbol           { eq KEYWORD-SYMBOL }
;;            (and patterns)           { match all stuff }
;;            (or patterns)            { match any stuff }
;;            (not pattern)
;;            (:predicate function)    { (FUNCTION expression) }
;;            (:pattern patterns)

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; PATTERN COMPILATION ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun pattern-continue (continuation &optional vars)
  (when continuation
    (funcall continuation vars)))


(defun if-pattern-emit (exp then else)
  (declare (type (or symbol list) exp))
  `(if ,exp
       ,(pattern-continue then nil)
       ,(pattern-continue else nil)))

(defun let-pattern-emit (var exp then)
  (declare (type symbol var exp))
  `(let ((,var ,exp))
     ,(pattern-continue then (list var))))

(defun pattern-compile-meta (pattern exp bound-list then else &optional (metas *meta-patterns*))
  (if metas
      (if-let (sub-pattern (funcall (car metas) pattern))
        (pattern-compile sub-pattern exp bound-list then else)
        (pattern-compile-meta pattern exp bound-list then else (cdr metas)))
      (error "Unknown pattern ~A" pattern)))

(defun pattern-compile-predicate (car cdr exp then else)
  (if-pattern-emit `(,car ,exp ,@(when cdr cdr))
                   then else))


;; returns (values (list vars) expression)
;; expression evalutes to (values matches-p vars...)
(defun pattern-compile (pattern exp bound-list then else)
  (declare (type symbol exp))
  (cond
    ((null pattern)
     (pattern-continue else))
    ((eq t pattern)
     (pattern-continue then nil))
    ((numberp pattern)
     (if-pattern-emit `(and (numberp ,exp)
                            (= ,pattern ,exp))
                      then else))
    ((symbolp pattern)
     (cond
       ((keywordp pattern)
        (pattern-compile-predicate 'eq (list pattern) exp then else))
       ((find pattern bound-list)
        (pattern-compile-predicate 'eql (list pattern) exp then else))
       (t
        (let-pattern-emit pattern exp then))))
    ((consp pattern)
     (pattern-compile-exp (car pattern) (cdr pattern) exp bound-list then else))
    (t (pattern-compile-meta pattern exp bound-list then else))))

(defun pattern-compile-pattern (patterns exp bound-list then else)
  (declare (type symbol exp))
  (with-gensyms (first rest else-fun)
    (let* ((call-else (if else (list else-fun) nil))
           (else-lambda (if else (lambda (vars) (declare (ignore vars)) call-else) nil)))
      (labels ((rec (patterns exp bound-list)
                 (cond
                   ((null patterns)
                    (if-pattern-emit exp else then))
                   ((eq '&rest (car patterns))
                    (assert (and (symbolp (cadr patterns))
                                 (null (cddr patterns))))
                    (let-pattern-emit (cadr patterns) exp then))
                   (t
                    (if-pattern-emit `(consp ,exp)
                                     (lambda (var)
                                       (assert (null var))
                                       `(let ((,first (car ,exp))
                                              (,rest (cdr ,exp)))
                                          ,(pattern-compile (car patterns) first bound-list
                                                            (lambda (vars)
                                                              (rec (cdr patterns) rest (append vars bound-list)))
                                                            else-lambda)))
                                     else-lambda)))))
        (if else
            `(flet ((,else-fun () ,(pattern-continue else)))
               ,(rec patterns exp bound-list))
            (rec patterns exp bound-list))))))


(defun pattern-compile-exp (car cdr exp bound-list then else)
  (declare (type symbol exp car))
  (case car
    ;; predicates
    (:predicate
     (pattern-compile-predicate (car cdr) (cdr cdr) exp then else))
    (quote ; atom consp listp equal eq symbolp keywordp numberp)
     (destructuring-bind (symbol) cdr
       (pattern-compile-predicate 'eq `(',symbol) exp then else)))
    ;; nested pattern
    (:pattern (pattern-compile-pattern cdr exp bound-list then else))
    (otherwise
     (pattern-compile-meta (cons car cdr) exp bound-list then else))))


(defmacro if-pattern (pattern exp then &optional else)
  (let* ((exp-sym (if (constantp pattern) pattern
                      (gensym "EXP")))
         (compiled (pattern-compile pattern exp-sym nil
                                    (lambda (vars)
                                      (declare (ignore vars))
                                      then)
                                    (when else
                                      (lambda (vars)
                                        (declare (ignore vars))
                                        else)))))
    (if (constantp pattern)
        ;; no need to rebind constant expressions
        compiled
        ;; we use exp, so rebind i
        `(let ((,exp-sym ,exp))
           ,compiled))))

(defmacro pattern-case (exp &body cases)
  (with-gensyms (exp-sym)
    (labels ((helper (cases)
               (when cases
                 (destructuring-bind ((pattern &rest body) &rest rest-cases) cases
                   `(if-pattern ,pattern ,exp-sym
                                ,(ensure-progn body)
                                ,(helper rest-cases))))))
      `(let ((,exp-sym ,exp))
         ,(helper cases)))))

(defmacro def-meta-pattern (meta-pattern replacement-pattern)
  "During pattern expansion, if a pattern matches META-PATTERN,
   substitute instead REPLACEMENT-PATTERN."
  (with-gensyms (pattern)
    `(push (lambda (,pattern)
             (if-pattern ,meta-pattern ,pattern
                         ',replacement-pattern))
           *meta-patterns*)))


(defmacro with-meta-pattern (meta-pattern replacement-pattern &body body)
  "During pattern expansion, if a pattern matches META-PATTERN,
   substitute instead REPLACEMENT-PATTERN."
  (with-gensyms (pattern)
    `(let ((*meta-patterns*
            (cons (lambda (,pattern)
                    (if-pattern ,meta-pattern ,pattern
                                ',replacement-pattern))
                  *meta-patterns*)))
       ,@body)))
