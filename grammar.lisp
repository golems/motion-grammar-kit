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

(in-package :motion-grammar)


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


(defun grammar-index-head (grammar)
  "Indexes productions by head.
GRAMMAR: a grammar
RESULT: (lambda (nonterminal)) => all productions bodies
        that nonterminal expands to."
  (let ((h (grammar-fold (lambda (h head body)
                           (push (gethash head h) body)
                           h)
                         (make-hash-table :test #'equal)
                         grammar)))
    (curry-right #'gethash h)))


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
    (rewrite-grammar
     (lambda (head body)
       (cond
         ((or
           ;; A -> a | B | :epsilon
           (and (= 1 (length body)))
           ;; A -> a B
           (and (= 2 (length body))
                (finite-set-member terminals (first body))
                (not (finite-set-member terminals (second body)))))
          (list (cons head body)))
         ;; A -> a b ALPHA
         ((and (<= 2 (length body))
               (finite-set-member terminals (first body))
               (finite-set-member terminals (second body)))
          (let ((new-nonterm (gensym "GRAMMAR->RIGHT-REGULAR")))
            (list (list head (first body) new-nonterm)
                  (cons new-nonterm (rest body)))))
         (t (error "Can't handle ~A => ~A" head body))))
     grammar)))

    ;; (labels ((simplify (grammar)
    ;;            (when grammar
    ;;              (destructuring-bind ((lhs &rest rhs) &rest remaining) grammar
    ;;                (cond
    ;;                  ;; A -> a | B | :epsilon
    ;;                  ((and (= 1 (length rhs)))
    ;;                   (cons (car grammar) (simplify (cdr grammar))))
    ;;                  ;; A -> a B
    ;;                  ((and (= 2 (length rhs))
    ;;                        (finite-set-member terminals (first rhs))
    ;;                        (not (finite-set-member terminals (second rhs))))
    ;;                   (cons (car grammar) (simplify (cdr grammar))))
    ;;                  ;; A -> a b ALPHA
    ;;                  ((and (<= 2 (length rhs))
    ;;                        (finite-set-member terminals (first rhs))
    ;;                        (finite-set-member terminals (second rhs)))
    ;;                   (let ((new-nonterm (gensym "GRAMMAR->RIGHT-REGULAR")))
    ;;                     (simplify `(,(list lhs (first rhs) new-nonterm)
    ;;                                  (,new-nonterm ,@(rest rhs))
    ;;                                  ,@remaining))))
    ;;                  (t (error "Can't handle ~A => ~A" lhs rhs)))))))
    ;;   (simplify grammar))))


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


(defun make-grammar-from-adjacency (adj)
  (let ((nonterminal-table (make-hash-table :test #'equal)))
    ;; construct nonterminal symbols
    (map nil (lambda (x)
               (assert (= 2 (length x)))
               (map nil (lambda (y)
                          (unless (gethash y nonterminal-table)
                            (format t "adding ~A~%" y)
                            (setf (gethash y nonterminal-table)
                                  (gensym (gsymbol-name y)))))
                    x))
         adj)
    ;; construct grammar
    (map 'list (lambda (x)
                 (destructuring-bind (a b) x
                   (list (gethash a nonterminal-table)
                         b
                         (gethash b nonterminal-table))))
         adj)))


(defmacro walk-grammar (name grammar (head-var body-var rest-var)
                        &body body)
  (alexandria:with-gensyms (grammar-var)
    `(labels ((,name (,grammar-var)
                (when ,grammar-var
                  (destructuring-bind ((,head-var &rest ,body-var)
                                       &rest ,rest-var ) ,grammar-var
                    ,@body))))
       (,name ,grammar))))


(defun apply-rewrite (function grammar)
  "Rewrites GRAMMAR by applying FUNCTION.
FUNCTION: (lambda (term)) => (list new-terms...),
          must return a fresh list as it will be NCONC'ed"
  (let ((visited (make-finite-set :mutable t))   ;; stuff we've already expanded
        (included (make-finite-set :mutable t))) ;; stuff we've already included
    (labels ((visit-term (term)
               ;;(format t "visit ~A~&" term)
               (cond
                 ((finite-set-inp term included)
                  (assert (finite-set-nadd term visited))
                  ;;(format t "included ~A~&" term)
                  nil)
                 ((finite-set-inp term visited)
                  ;;(format t "visited ~A~&" term)
                  (finite-set-nadd included term)
                  (list term))
                 (t ;; new term
                  ;;(format t "new ~A~&" term)
                  (finite-set-nadd visited term) ;; don't re-visit already-rewritten terms
                  (visit-list (funcall function term)))))
             (visit-list (list)
               ;;(mapcan #'visit-term list)
               ;; the loop slightly outperforms mapcan
               (loop for term in list
                  nconc (visit-term term))))
      (visit-list grammar))))

(defun rewrite-grammar (function grammar)
  "Rewrites GRAMMAR by applying FUNCTION.
FUNCTION: (lambda (head body)) => (list new-productions...)"
  (apply-rewrite (lambda (term)
                   (destructuring-bind (head &rest body) term
                     (funcall function head body)))
                 grammar))

(defun grammar-remove-epsilon (grammar)
  ;;(format t "~&START: ~A~&" grammar)
  (let ((epsilon-nonterms (make-finite-set :mutable t))
        (non-epsilon-nonterms (make-finite-set :mutable t))
        (modified))
    ;; FIXME: check for infite loop sometime
    ;; find epsilon nonterminals
    (grammar-map nil
                 (lambda (head body)
                   (finite-set-nadd (if (or (null body)
                                            (equal '(:epsilon) body))
                                        epsilon-nonterms
                                        non-epsilon-nonterms)
                                    head))
                 grammar)
    ;; rewrite the grammar
    (setq grammar
          (rewrite-grammar
           (lambda (head body)
             (cond
               ;; remove epsilon rule
               ((and (finite-set-inp head epsilon-nonterms)
                     (or (null body)
                         (equal '(:epsilon) body)))
                (setq modified t)
                nil)
               ;; head -> A and somewhere A -> :epsilon, but never head -> epsilon
               ((and (= 1 (length body))
                     (finite-set-inp (car body) epsilon-nonterms)
                     (not (finite-set-inp head epsilon-nonterms)))
                (list (cons head body)
                      (list head :epsilon)))
               ;; head -> A and somewhere A -> :epsilon and somewhere head -> :epsilon
               ((and (= 1 (length body))
                     (finite-set-inp (car body) epsilon-nonterms)
                     (finite-set-inp head epsilon-nonterms))
                nil)
               ;; general case
               (t
                (setq body
                      (remove-if (lambda (x)
                                   (and (finite-set-inp x epsilon-nonterms)
                                        (not (finite-set-inp x non-epsilon-nonterms))))
                                 body))
                ;;(format t "~&new-body : ~A => ~A~&" head body)
                (let ((new-prods (loop
                                    for x in body
                                    for suffix on body
                                    for k from 0
                                    when (finite-set-inp x epsilon-nonterms)
                                    collect (cons head
                                                  (append (subseq body 0 k)
                                                          (cdr suffix))))))
                  ;;(format t "~&new-prods : ~A~&" new-prods)
                  (if (null new-prods)
                      (list (cons head body))
                      (cons (cons head body)
                            new-prods))))))
           grammar))
    ;;(format t "~&grams : ~A~&" grammar)
    ;; possibly recurse
    (if modified
        (grammar-remove-epsilon grammar)
        grammar)))


(defun grammar-remove-unit (grammar)
  (let ((nonterms (grammar-nonterminals grammar))
        (unit-prod (make-finite-set :mutable t))
        (unit-body (make-hash-table :test #'equal))
        (modified))
    ;; find unit nonterminals
    (grammar-map nil
                 (lambda (head body)
                   (when (and (= 1 (length body))
                              (finite-set-inp (first body) nonterms))
                     (finite-set-nadd unit-prod (cons head body))
                     (push head (gethash (first body) unit-body ))))
                 grammar)
    ;; rewrite
    (setq grammar
          (rewrite-grammar
           (lambda (head body)
             ;;(format t "rewrite ~A => ~A~&" head body)
             (cond
               ((finite-set-inp (cons head body) unit-prod)
                (setq modified t)
                nil)
               ((and (finite-set-inp head unit-body))
                (cons (cons head body)
                      (loop for A in (gethash head unit-body)
                         for p = (cons A body)
                         unless (finite-set-inp p unit-prod)
                         collect p)))
               (t (list (cons head body)))))
           grammar))
    (if modified
        (grammar-remove-unit grammar)
        grammar)))

(defun grammar-print (grammar &optional (output *standard-output*))
  (grammar-map nil (curry-list #'format output  "~&~A => ~{~A~^ ~}~%") grammar))


(defun grammar->cnf (grammar)
  "Convert grammar to Chomsky Normal Form"
  (let ((terminals (grammar-terminals grammar)))
    (rewrite-grammar
     (lambda (head body)
       (cond
         ;; correct form
         ((or (and (= 1 (length body))
                   (finite-set-inp (car body) terminals))
              (and (= 2 (length body))
                   (not (finite-set-inp (first body) terminals))
                   (not (finite-set-inp (second body) terminals))))
          (list (cons head body)))
         ;; long nonterminal sequence split
         ((and (> (length body) 2)
               (not (finite-set-inp (first body) terminals))
               (not (finite-set-inp (second body) terminals)))
          (let ((xp (gensym (gsymbol-name (second body)))))
            (list (list head (first body) xp)
                  (cons xp (rest body)))))
         (t ;; else, remove the terminals
          (let ((new-body
                 (map 'list (lambda (x)
                              (if (finite-set-inp x terminals)
                                  (gensym (gsymbol-name x))
                                  x))
                      body)))
            (cons (cons head new-body)
                  (fold (lambda (rest new-x old-x)
                          (if (finite-set-inp old-x terminals)
                              (cons (list new-x old-x)
                                    rest)
                              rest))
                               nil new-body body))))))
     grammar)))

    ;; (walk-grammar visit grammar (head body rest)
    ;;   (cond
    ;;     ;; correct form
    ;;     ((or (and (= 1 (length body))
    ;;               (finite-set-inp (car body) terminals))
    ;;          (and (= 2 (length body))
    ;;               (not (finite-set-inp (first body) terminals))
    ;;               (not (finite-set-inp (second body) terminals))))
    ;;      (cons (cons head body)
    ;;            (visit rest)))
    ;;     ;; long nonterminal sequence split
    ;;     ((and (> (length body) 2)
    ;;           (not (finite-set-inp (first body) terminals))
    ;;           (not (finite-set-inp (second body) terminals)))
    ;;      (let ((xp (gensym (gsymbol-name (second body)))))
    ;;        (cons (list head (first body) xp)
    ;;              (visit `((,xp ,@(cdr body))
    ;;                       ,@rest)))))
    ;;     (t ;; else, remove the terminals
    ;;      (let ((new-body
    ;;             (map 'list (lambda (x)
    ;;                          (if (finite-set-inp x terminals)
    ;;                              (gensym (gsymbol-name x))
    ;;                              x))
    ;;                  body)))
    ;;        (visit (cons (cons head new-body)
    ;;                     (fold (lambda (rest new-x old-x)
    ;;                             (if (finite-set-inp old-x terminals)
    ;;                                 (cons (list new-x old-x)
    ;;                                       rest)
    ;;                                 rest))
    ;;                           rest new-body body)))))))))





;; (defun grammar->pda

;; (defun grammar-cross-regular (cfg rrg)
;;   (let ((c-index (grammar-index-head cfg))
;;         (r-index (grammar-index-head rrg))
;;         (r-first (grammar-first-function rrg))
;;         (terminals (finite-set-union (grammar-terminals cfg)
;;                                      (grammar-terminals rrg)))
;;         (hash (make-hash-table :test #'equal)))
;;     (labels ((add (c-nonterm r-nonterm)
;;                (let ((x-nonterm (list c-nonterm r-nonterm)))
;;                  (unless (finite-set-inp x-nonterm hash)
;;                    (setf (gethash x-nonterm hash) nil)
;;                    (finite-set-map nil
;;                                    (lambda (c-body)
;;                                      (simulate c-nonterm r-nonterm nil c-body-2))
;;                                    (funcall c-index c-nonterm)))))
;;              (simulate (c-nonterm r-nonterm c-body-1 c-body-2)
;;                (cond
;;                  ((null c-body-2)
;;                   (push (gethash (list c-nonterm r-nonterm) h)
;;                         c-body-1))
;;                  ((finite-set-inp (first c-body-2) terminals)
;;                   (finite-set-map (lambda (r-body)
;;                                     (when (equal (first c-body)
;;                                                  (first r-body))
;;                                       (simulate c-nonterm (second r-body)
;;                                                 (append c-body-1 (list (first c-body-2)))
;;                                                 (rest c-body-2))))
;;                                   (funcall r-index r-nonterm)))
