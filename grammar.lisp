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



;;; ASSUMPTIONS:
;;;   :EPSILON tokens have an index of zero
;;;   :$ (end of input) tokens have an index of -1


;;; GLOSSARY
;;;  - :TERMINAL :: atomic symbols of the language
;;;  - :TOKEN :: a :TERMINAL plus attributes
;;;  - :NONTERMINAL :: a compound symbol in the grammar, "SYNTAX-VARIABLE"
;;;  - :GSYMBOL :: language theoretic symbol,
;;;       represented as a lisp string, number, symbol or list thereof


;;; Grammar Representation
;;; - Direct List
;;;   - grammar -> ((prod1) (prod2) (prod3)...)
;;;   - prod -> (left-hand-side &rest expansion



(defun grammar-map (result function grammar)
  "Applies function to each production in grammar.
RESULT: (or nil 'list 'vector)
FUNCTION: (lambda (left-hand-side right-hand-side))
GRAMMAR: the BNF grammar"
  (map result (lambda (prod)
                (destructuring-bind (lhs &rest rhs) prod
                  (funcall function lhs rhs)))
       grammar))

(defun grammar-fold (function initial-value grammar)
  "Reduces function across productions of the grammar.
FUNCTION: (lambda (value lhs rhs))
GRAMMAR: the BNF grammar
RESULT: function reduced across grammar"
  (reduce (lambda (value production)
            (destructuring-bind (lhs &rest rhs) production
              (funcall function value lhs rhs)))
          grammar :initial-value initial-value))


(defun grammar-nonterminals (grammar)
  "Return list of nonterminals in the grammar."
  (let ((a nil))
    (grammar-map nil (lambda (l r) (declare (ignore r))
                        (pushnew l a))
                 grammar)
    a))

(defun grammar-terminals (grammar)
  "Return list of terminals in the grammar."
  (let ((nonterms (grammar-nonterminals grammar)))
    (grammar-fold (lambda (terminals lhs rhs)
                    (declare (ignore lhs))
                    (reduce (lambda (terminals gsym)
                              (if (finite-set-member nonterms gsym)
                                  terminals
                                  (finite-set-add terminals gsym)))
                            rhs
                            :initial-value terminals))
                  (make-finite-set)
                  grammar)))


(defun grammar-substitute-terminal-list
    (grammar terminal list)
  "Replace TERMINAL in GRAMMAR with the list of terminals LIST.
GRAMMAR: the BNF grammar
TERMINAL: the terminal to replace
LIST: the list of GSYMBOLS to replace TERMINAL with.
RESULT: A new grammar with substitution performed"
  (grammar-map 'list
               (lambda (lhs rhs)
                 (cons lhs
                       (mapcan (lambda (gsym)
                                 (if (equal gsym terminal)
                                     list
                                     (list gsym)))
                               rhs)))
               grammar))

(defun grammar-right-regular-p (grammar)
  "Is this a right regular grammar?"
  ;; all productions of form
  ;; A -> a
  ;; A -> aB
  ;; A -> :epsilon
  (let ((terminals (grammar-terminals grammar))
        (nonterminals (grammar-nonterminals grammar)))
    (grammar-map nil
                 (lambda (lhs rhs)
                   (unless
                       (and
                        ;; A -> .*
                        (= 1 (length lhs))
                        (or
                         ;; A -> a | :epsilon
                         (and (= 1 (length rhs))
                              (finite-set-member terminals (first rhs)))
                         ;; A -> a B
                         (and (= 2 (length rhs))
                              (finite-set-member terminals (first rhs))
                              (finite-set-member nonterminals (second rhs)))))
                     (return-from grammar-right-regular-p nil)))
                 grammar))
  t)


;; (defun grammar-prune-epsilon (grammar)
;;   (let ((production-table (make-hash-table :test #'equal))
;;         (epsilon-set (make-finite-set)))

;;     (grammar-map nil (lambda (lhs rhs)
;;                        (if (equal rhs '(:epsilon))
;;                            (setq epsilon-set (finite-set-add epsilon-set lhs))
;;                        (pushnew rhs (gethash lhs production-table))))

(defun grammar->right-regular (grammar)
  "Attempt to convert this grammar to right-regular form."
  ;; TODO: prune epsilons, singletons, and redundant nonterminals
  ;;       do this using same steps an CNF conversion
  (let ((new-grammar (make-finite-set))
        (terminals (grammar-terminals grammar))
        (nonterminals (grammar-nonterminals grammar)))
    (labels ((add-production (lhs rhs)
               (setq new-grammar (finite-set-add new-grammar
                                                 (cons lhs rhs))))
             (add-nonterminal (nonterm)
               (setq nonterminals (finite-set-add nonterminals nonterm)))
             (simplify (lhs rhs)
               (cond
                 ;; A -> a | :epsilon
                 ((and (= 1 (length rhs))
                       (finite-set-member terminals (first rhs)))
                  (add-production lhs rhs))
                 ;; A -> a B
                 ((and (= 2 (length rhs))
                       (finite-set-member terminals (first rhs))
                       (finite-set-member nonterminals (second rhs)))
                  (add-production lhs rhs))
                 ;; A -> a b ALPHA
                 ((and (<= 2 (length rhs))
                       (finite-set-member terminals (first rhs))
                       (finite-set-member terminals (second rhs)))
                  (let ((new-nonterm (gensym "GRAMMAR->RIGHT-REGULAR")))
                    (add-production lhs (list (first rhs) new-nonterm))
                    (add-nonterminal new-nonterm)
                    (simplify new-nonterm (rest rhs))))
                 (t (error "Can't handle ~A => ~A" lhs rhs)))))
      ;; simplify initially
      (grammar-map nil #'simplify grammar))
    new-grammar))


