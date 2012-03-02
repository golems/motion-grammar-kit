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


(defun gsymbol-name (gsym)
  (format nil "~A" gsym))

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

(defun grammar-first-function (grammar)
  "Computes first sets of grammar returns a function giving each first set.
GRAMMAR: a grammar
RESULT: (lambda (nonterminal)) => first set of nonterminal"
  (let ((h (make-hash-table :test #'equal))
        (terminals (grammar-terminals grammar))
        did-it)
    ;; init sets
    (grammar-map nil (lambda (head tail)
                       (declare (ignore tail))
                       (setf (gethash head h) (make-finite-set)))
                 grammar)
    ;; add terminals
    (finite-set-map nil (lambda (term) (setf (gethash term h)
                                        (finite-set-add (gethash term h) term)))
                    terminals)
    ;; add nonterminals
    (labels ((visit (head tail)
               (let ((head-set (gethash head h))
                     (tail-set (and (car tail)
                                    (gethash (car tail) h))))
                 ;; nonterminal, new
                 (cond
                   ;; epsilon
                   ((and (null tail)
                         (not (finite-set-member head-set :epsilon)))
                    (setf (gethash head h)
                          (finite-set-add head-set :epsilon))
                    (setq did-it t))
                   ((and tail-set
                         (not (finite-set-subsetp tail-set head-set)))
                    (setf (gethash head h)
                          (finite-set-union head-set tail-set))
                    (setq did-it t)))
                 (when (finite-set-member (gethash (car tail) h) :epsilon)
                   (visit head (cdr tail))))))
      (loop do
           (setq did-it nil)
           (grammar-map nil #'visit grammar)
         while did-it))
    (maphash (lambda (k v)
               (format t "~&~A => ~A~&" k v)) h)
    (curry-right #'gethash h)))






(defun grammar-start-nonterminal (grammar)
"Return the starting nonterminal of GRAMMAR."
  (caar grammar))

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
                   (declare (ignore lhs))
                   (unless
                       (or
                        ;; A -> a | :epsilon
                        (and (= 1 (length rhs)))
                        ;; A -> a B
                        (and (= 2 (length rhs))
                              (finite-set-member terminals (first rhs))
                              (finite-set-member nonterminals (second rhs))))
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
  (let ((terminals (grammar-terminals grammar)))
    (labels ((simplify (grammar)
               (when grammar
                 (destructuring-bind ((lhs &rest rhs) &rest remaining) grammar
                   (cond
                     ;; A -> a | B | :epsilon
                     ((and (= 1 (length rhs)))
                      (cons (car grammar) (simplify (cdr grammar))))
                     ;; A -> a B
                     ((and (= 2 (length rhs))
                           (finite-set-member terminals (first rhs))
                           (not (finite-set-member terminals (second rhs))))
                      (cons (car grammar) (simplify (cdr grammar))))
                     ;; A -> a b ALPHA
                     ((and (<= 2 (length rhs))
                           (finite-set-member terminals (first rhs))
                           (finite-set-member terminals (second rhs)))
                      (let ((new-nonterm (gensym "GRAMMAR->RIGHT-REGULAR")))
                        (simplify `(,(list lhs (first rhs) new-nonterm)
                                     (,new-nonterm ,@(rest rhs))
                                     ,@remaining))))
                     (t (error "Can't handle ~A => ~A" lhs rhs)))))))
      (simplify grammar))))


(defun grammar->fa (grammar)
  "Convert grammar to a finite automata.
GRAMMAR: a right regular grammar (or something close to right regular)
RESULT: a finite automaton"
  (unless (grammar-right-regular-p grammar)
    (setq grammar (grammar->right-regular grammar)))
  (let ((terminals (grammar-terminals grammar))
        (nonterminals (grammar-nonterminals grammar))
        (edges)
        (start (caar grammar))
        (accept (gensym "ACC")))
    (grammar-map nil
                 (lambda (lhs rhs)
                   (cond
                     ;; A -> a
                     ((and (= 1 (length rhs))
                           (finite-set-member terminals (first rhs)))
                      (push (list lhs (first rhs) accept)
                            edges))
                     ;; A -> B
                     ((and (= 1 (length rhs))
                           (finite-set-member nonterminals (first rhs)))
                      (push (list lhs :epsilon (first rhs))
                            edges))
                     ;; A -> a B
                     ((and (= 2 (length rhs))
                           (finite-set-member terminals (first rhs))
                           (finite-set-member nonterminals (second rhs)))
                      (push (list lhs (first rhs) (second rhs))
                            edges))
                     (t
                      (error "Unhandled production: ~A => ~A"
                             lhs rhs))))
                 grammar)
    (make-fa edges start accept)))
