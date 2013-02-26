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
;;
;; pattern := binding
;;            t
;;            nil
;;            number
;;            keyword-symbol
;;            (:equal value)
;;            (:eq value)
;;            (:predicate function)
;;            (:and patterns)
;;            (:or patterns)
;;            (:not pattern)
;;            (:pattern patterns)


;; (defun eval-pattern-match (pattern exp)
;;   (cond
;;     ((null pattern) (not exp))
;;     ((eq t pattern) t)
;;     ((numberp pattern) (and (numberp exp) (= pattern exp)))
;;     ((symbolp pattern)
;;      (if (keywordp pattern)
;;          (eq pattern exp)
;;          t))
;;     ((consp pattern)
;;      (destructuring-case pattern
;;        ((:equals value)
;;         (equal value exp))
;;        ((:eq value)
;;         (eq value exp))
;;        ((:pattern &rest patterns)
;;         (every #'eval-pattern-match patterns exp))
;;        ((:predicate predicate)
;;         (funcall predicate exp))
;;        ((:and &rest patterns)
;;         (every (lambda (pattern) (eval-pattern-match pattern exp))
;;                patterns))
;;        ((:or &rest patterns)
;;         (some (lambda (pattern) (eval-pattern-match pattern exp))
;;               patterns))
;;        ((:not pattern)
;;         (not (eval-pattern-match pattern exp)))
;;        ((t &rest rest)
;;         (declare (ignore rest))
;;         (error "Unknown pattern ~A" pattern))))
;;     (t (error "Unknown pattern ~A" pattern))))

;; (defmacro cond-pattern-eval (value &body clauses)
;;   (with-gensyms (value-sym)
;;   `(let ((,value-sym ,value))
;;      (cond
;;        ,@(loop for (test . body) in clauses
;;             collect `((eval-pattern-match ',test ,value-sym) ,@body))))))


;; (defun pattern-bindings (pattern exp binding-list)
;;   "Produce bindings for the pattern match.
;; binding-list: (list (list var gensym subexp))"
;;   (cond
;;     ;; no bindings case
;;     ((eval-pattern-match `(:or nil
;;                                (:eq t)
;;                                (:predicate numberp)
;;                                (:predicate keywordp)))
;;      binding-list)
;;     ;; single binding
;;     ((symbolp pattern)
;;      (list pattern (gensym (string pattern)) exp))
;; ))
;; (defmacro bind-pattern (pattern value &body body)
;;   (cond
;;     ;; no bindings case
;;     ((eval-pattern-match `(:or nil
;;                                (:eq t)
;;                                (:predicate numberp)
;;                                (:predicate keywordp)))
;;      (cons 'progn body))
;;     ;; single binding
;;     ((symbolp pattern)
;;      `(let ((,pattern value))
;;         body))

;; (destructuring-bind (a b c d e f g) c
;;   (list b a))


;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; PATTERN COMPILATION ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;



(defparameter *meta-patterns* nil)

(defun if-pattern-emit (vars values-expression then &optional else)
  (with-gensyms (matches)
    (let* ((vars (ensure-list vars))
           (genvars (if else
                        (map 'list (lambda (var)
                                     (gensym (string var)))
                             vars)
                        vars))
           (then-expr (if (and else vars)
                          `(let ,(map 'list #'list vars genvars)
                             ,(funcall then vars))
                          (funcall then vars))))

      `(multiple-value-bind (,matches ,@genvars) ,values-expression
         (if ,matches
             ,then-expr
             ,@(when else (list (funcall else))))))))

(defun pattern-compile-meta (pattern exp bound-list then else &optional (metas *meta-patterns*))
  (if metas
      (if-let (sub-pattern (funcall (car metas) pattern))
        (pattern-compile sub-pattern exp bound-list then else)
        (pattern-compile-meta pattern exp bound-list then else (cdr metas)))
      (error "Unknown pattern ~A" pattern)))

;; returns (values (list vars) expression)
;; expression evalutes to (values matches-p vars...)
(defun pattern-compile (pattern exp bound-list then else)
  (cond
    ((null pattern)
     (if-pattern-emit nil (list 'not exp) then else))
    ((eq t pattern)
     (if-pattern-emit nil t then else))
    ((numberp pattern)
     (if-pattern-emit nil `(and (numberp ,exp)
                                (= ,pattern ,exp))
                      then else))
    ((symbolp pattern)
     (cond
       ((keywordp pattern)
        (if-pattern-emit nil `(eq ,pattern ,exp) then else))
       ((find pattern bound-list)
        (if-pattern-emit nil `(eql ,pattern ,exp) then else)) ;; FIXME: is eql appropriate?
       (t
        (if-pattern-emit pattern `(values t ,exp) then else))))
    ((consp pattern)
     (pattern-compile-exp (car pattern) (cdr pattern) exp bound-list then else))
    (t (pattern-compile-meta pattern exp bound-list then else))))


(defun pattern-compile-predicate (car cdr exp then else)
  `(if (,car ,exp ,@(when cdr cdr))
       ,(funcall then nil)
       ,@(when else
               (list (funcall else)))))


(defun pattern-compile-pattern (patterns exp bound-list then else)
  (with-gensyms (first rest else-fun)
    (let* ((call-else (if else (list else-fun) nil))
           (else-lambda (if else (lambda () call-else) nil)))
      (labels ((rec (patterns exp bound-list)
                 (cond
                   ((null patterns)
                    `(if ,exp
                         ,call-else
                        ,(funcall then bound-list)))
                   ((eq '&rest (car patterns))
                    (assert (and (symbolp (cadr patterns))
                                 (null (cddr patterns))))
                    (if-pattern-emit (cadr patterns)
                                     `(values t ,exp)
                                     (lambda (vars)
                                       (funcall then (append vars bound-list)))
                                     else-lambda))
                   (t
                    `(if (consp ,exp)
                         (let ((,first (car ,exp))
                               (,rest (cdr ,exp)))
                           ,(pattern-compile (car patterns) first bound-list
                                             (lambda (vars)
                                               (rec (cdr patterns) rest (append vars bound-list)))
                                             else-lambda))
                         ,call-else)))))
        (if else
            `(flet ((,else-fun () ,(funcall else)))
               ,(rec patterns exp bound-list))
            (rec patterns exp bound-list))))))


(defun pattern-compile-exp (car cdr exp bound-list then else)
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
  (with-gensyms (exp-sym)
    `(let ((,exp-sym ,exp))
       ,(pattern-compile pattern exp-sym nil
                         (lambda (vars)
                           (declare (ignore vars))
                           then)
                         (when else
                           (lambda ()
                             else))))))

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


(defmacro pattern-lambda (pattern &body body)
  (with-gensyms (exp)
    `(lambda (,exp) (if-pattern ,pattern ,exp
                                (values ,(ensure-progn body) t)
                                (values nil nil)))))

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
