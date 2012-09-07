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



;;;; Token Classes
;;;;  - Interrupt: Asynchronous
;;;;  - Predicate: Synchronous
;;;;  - Semantic:  A function call

(in-package :motion-grammar)

(defun token-interrupt-p (token)
  (and (listp token)
       (eq :interrupt (car token))))

(defun token-predicate-p (token)
  (and (listp token)
       (eq :predicate (car token))))

(defun token-semantic-p (token)
  (and (listp token)
       (eq :semantic (car token))))



;; Aho 2nd (Purple Dragon), p224, Algorithm 4.31
(defun make-predictive-table (grammar &key (duplicate-error-p t))
  "Generate parsing table for a nonrecursive predictive parser.
GRAMMAR: an grammar
DUPLICATE-ERROR-P: if t, require an LL(1) grammar and map each (X a) to a single production
                   if nil, map each (X a) to a finite set of productions
RESULT: (lambda (X a)) => (or production nil (finite-set productions))"
  (let* ((terminals (grammar-terminals grammar))
         (nonterminals (grammar-nonterminals grammar))
         (first (grammar-first-function grammar terminals))
         (follow (grammar-follow-function grammar terminals nonterminals first))
         (hash (make-hash-table :test #'equal))) ; (A . a) => productions
    (grammar-map nil
                 (lambda (head body)
                   (labels ((add-function (a)
                              (unless (eq a :epsilon)
                                (let* ((key (cons head a))
                                       (old-set (gethash key hash))
                                       (new-item (cons head body)))
                                  (setf (gethash key hash)
                                        (cond
                                          ((and old-set
                                                duplicate-error-p)
                                           (error "Duplicate entries for [~A,~A]"
                                                  head a))
                                          ((and (not old-set)
                                                duplicate-error-p)
                                           new-item)
                                          ((and old-set
                                                (not duplicate-error-p))
                                           (finite-set-add old-set new-item))
                                          ((and (not old-set)
                                                (not duplicate-error-p))
                                           (finite-set new-item))
                                          (t (error "Godel was here"))))))))
                     (let ((first-body (grammar-list-first first body)))
                       (finite-set-map nil #'add-function first-body)
                       (when (finite-set-inp :epsilon first-body)
                         (finite-set-map nil #'add-function (funcall follow head))))))
                 grammar)
    (lambda (X a)
      (gethash (cons X a) hash))))


;; Parsing Structures:
;; - Big Table
;; - Per-state structure with
;;   - semantic thingy
;;   - Predicate List
;;   - Successor array / Edge List / Search Tree / (Perfect) Hash table

;; (defun dfa->motion-parser (dfa)
;;   (assert (dfa-p dfa))
;;   (assert (= 1 (length (fa-start dfa))))
;;   (let ((mover (fa-mover dfa)))
;;     (lambda (predicate-function semantic-function interrupt-function)
;;       (labels ((transition (state token)
;;                  ;; go to the next state
;;                  (execute (car (funcall mover state token))))
;;                (execute (state)
;;                  (let ((zeta (block find-token
;;                             ;; check predicates
;;                             (dotimes (i (length (fa-tokens fa)))
;;                               (when (and (token-predicate-p
;;                                           (fa-token-name fa i))
;;                                          (funcall predicate-function i))
;;                                 (return-from find-token i)))
;;                             ;; check for semantics
;;                             (dotimes (i (length (fa-tokens fa)))
;;                               (when (token-semantic-p (fa-token-name fa i))
;;                                 (return-from find-token i)))
;;                             ;; wait for interrupt
;;                             (interrupt-function))))
;;                    (unless (= zeta -1)
;;                      (transition state zeta))))))
;;       (execute (car (fa-start dfa))))))
