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

(in-package :motion-grammar-kit)


(defun grammar-map-list (result function grammar)
  "Applies function to each production in grammar.
RESULT: (or nil 'list 'vector)
FUNCTION: (lambda (production))
GRAMMAR: the BNF grammar"
  (map result function grammar))

(defun grammar-map (result function grammar)
  "Applies function to each production in grammar.
RESULT: (or nil 'list 'vector)
FUNCTION: (lambda (left-hand-side right-hand-side))
GRAMMAR: the BNF grammar"
  (grammar-map-list result (lambda (prod)
                             (destructuring-bind (lhs &rest rhs) prod
                               (funcall function lhs rhs)))
                    grammar))


(defun grammar-fold-list (function initial-value grammar)
  "Reduces function across productions of the grammar.
FUNCTION: (lambda (value production))
GRAMMAR: the BNF grammar
RESULT: function reduced across grammar"
  (reduce function grammar :initial-value initial-value))

(defun grammar-fold (function initial-value grammar)
  "Reduces function across productions of the grammar.
FUNCTION: (lambda (value lhs rhs))
GRAMMAR: the BNF grammar
RESULT: function reduced across grammar"
  (grammar-fold-list (lambda (value production)
                       (destructuring-bind (lhs &rest rhs) production
                         (funcall function value lhs rhs)))
          initial-value grammar ))


(defun grammar-nonterminals (grammar)
  "Return list of nonterminals in the grammar."
  (let ((a nil))
    (grammar-map nil (lambda (l r) (declare (ignore r))
                        (pushnew l a :test #'equal))
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


(defun grammar-terminal-p (terminals nonterminals gsymbol)
  "Is GSYMBOL a terminal?"
  (let ((in-terms (finite-set-inp gsymbol terminals))
        (in-nonterms (finite-set-inp gsymbol nonterminals)))
    (cond
      ((and in-terms in-nonterms)
       (error "Found ~A in both sets" gsymbol) )
      ((and in-terms (not in-nonterms))
       t)
      ((and (not in-terms) in-nonterms)
       nil)
      ((and (not in-terms) (not in-nonterms))
       (error "Found ~A in neither set" gsymbol))
      (t (error "Godel was here")))))

(defun grammar-nonterminal-p (terminals nonterminals gsymbol)
  (not (grammar-terminal-p terminals nonterminals gsymbol)))

(defun grammar-chain-rule-p (terminals nonterminals production)
  "Is PRODUCTION a chain rule (A -> B)?"
  (assert (grammar-nonterminal-p terminals nonterminals (car production)))
  (and (= 2 (length production))
       (grammar-nonterminal-p terminals nonterminals (second production))))



(defun grammar-fixpoint (function grammar &key
                         initial-mapping)
  "Compute a fixpoint mapping for the grammar.
Continue applying FUNCTION to each production of GRAMMAR until no
new updates to key=>set.
FUNCTION: (lambda head body get-set union-set) => nil
GRAMMAR: a list of productions
INITIAL-MAPPING (list (key . set))
RESULT: (lambda (key) => set"
  (let* ((hash (make-hash-table :test #'equal))
         (get-set (lambda (x) (gethash x hash)))
         (modified)
         (union-set (lambda (key set)
                      (let ((old-set (funcall get-set key)))
                        (unless (finite-set-subsetp set old-set)
                          (setf (gethash key hash) (finite-set-union set old-set)
                                modified t))))))
    ;; set any initial mapping
    (loop for (key . set) in initial-mapping
         do (setf (gethash key hash) set))
    ;; iterate over grammar until reaching the fixpoint
    (loop
       do
         (setq modified nil)
         (loop
            for (head . body) in grammar
            do (funcall function head body get-set union-set))
       while modified)
    ;; return mapping
    get-set))


(defun grammar-chainable-function (grammar &optional
                                   (terminals (grammar-terminals grammar))
                                   (nonterminals (grammar-nonterminals grammar)))
  "Compute nonterminals reachable for each nonterminal of the grammar using only chain rules.
RESULT: (lambda nonterminal) => finite-set of chainable child nonterminals"
  (grammar-fixpoint (lambda (head body get-set union-set)
                      (funcall union-set head
                               (finite-set-add (funcall get-set (car body))
                                               (car body))))
                    ;; consider only chain rules
                    (finite-set-filter (lambda (rule)
                                         (grammar-chain-rule-p terminals nonterminals rule))
                                       grammar)))

(defun grammar-chainable-parent-function (grammar &optional
                                          (terminals (grammar-terminals grammar))
                                          (nonterminals (grammar-nonterminals grammar)))
  "Compute nonterminals reachable for each nonterminal of the grammar using only chain rules.
RESULT: (lambda nonterminal) => finite-set of chainable parent nonterminals"
  (grammar-fixpoint (lambda (head body get-set union-set)
                      (funcall union-set (car body)
                               (finite-set-add (funcall get-set head)
                                               head)))
                    ;; consider only chain rules
                    (finite-set-filter (lambda (rule)
                                         (grammar-chain-rule-p terminals nonterminals rule))
                                       grammar)))

(defun grammar-body-function (grammar)
  "Indexes productions by head.
GRAMMAR: a grammar
RESULT: (lambda (nonterminal)) => set of production bodies NONTERMINAL expands to."
  (rcurry #'gethash
          (grammar-fold (lambda (h head body)
                          (setf (gethash head h)
                                (finite-set-add (or (gethash head h)
                                                    (make-finite-set))
                                                body))
                          h)
                        (make-hash-table :test #'equal)
                        grammar)))


(defun grammar-first-nonterminals-function (grammar &optional
                                            (terminals (grammar-terminals grammar))
                                            (nonterminals (grammar-nonterminals grammar)))
  "For each nonterminal A in GRAMMAR, compute the set of nonterminals which my begin A."
  (grammar-fixpoint (lambda (head body get-set union-set)
                      (funcall union-set head
                               (finite-set-add (funcall get-set (car body))
                                               (car body))))
                    ;; only productions with leading nonterminal
                    (finite-set-filter (lambda (rule)
                                         (grammar-nonterminal-p terminals nonterminals (second rule)))
                                       grammar)))


(defun grammar-first-function (grammar &optional
                               (terminals (grammar-terminals grammar)))
  "Computes FIRST sets of grammar and returns a function giving each FIRST set.
GRAMMAR: a grammar
RESULT: (lambda (symbol)) => FIRST set of symbol"
  (grammar-fixpoint (lambda (head body get-set union-set)
                      (funcall union-set head (funcall get-set (car body))))
                    grammar
                    :initial-mapping (loop for a in terminals
                                        collect (cons a (finite-set a)))))

(defun grammar-list-first (first-function list)
  "Compute FIRST set for a list of grammar symbols."
  (labels ((helper (set list)
             (let ((first (funcall first-function (car list))))
               (if (and (finite-set-inp :epsilon first)
                        (cdr list))
                   (helper (finite-set-union set (finite-set-remove first :epsilon))
                           (cdr list))
                   (finite-set-union set first)))))
    (helper (make-finite-set) list)))



(defun grammar-follow-function (grammar &optional
                                (terminals (grammar-terminals grammar))
                                (nonterminals (grammar-nonterminals grammar))
                                (first-function (grammar-first-function grammar terminals)))
  "Computes FOLLOW sets of grammar and returns a function giving each FOLLOW set.
GRAMMAR: a grammar
RESULT: (lambda (symbol)) => FOLLOW set of symbol"
  (grammar-fixpoint (lambda (head body get-set union-set)
                      (mapl (lambda (list)
                              (destructuring-bind (B &rest beta) list
                                (when (grammar-nonterminal-p terminals nonterminals B)
                                  (funcall union-set B
                                           (if beta
                                               (let ((first-beta (grammar-list-first first-function beta)))
                                                 (if (finite-set-inp :epsilon first-beta)
                                                     (finite-set-union (finite-set-remove first-beta :epsilon)
                                                                       (funcall get-set head))
                                                     first-beta))
                                               (funcall get-set head))))))
                            body))
                    grammar
                    :initial-mapping (list (cons (grammar-start-nonterminal grammar)
                                                 (finite-set :$)))))


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
    (every (lambda (production)
             (destructuring-bind (head &rest body) production
               (declare (ignore head))
               (or (= 1 (length body))
                   (and (= 2 (length body))
                        (finite-set-member terminals (first body))
                        (finite-set-member nonterminals (second body))))))
           grammar)))


(defun grammar->right-regular (grammar)
  "Attempt to convert this grammar to right-regular form.
Please note that this operation is not always possible."
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


(defun grammar->fa (grammar &key accept)
  "Convert grammar to a finite automata.
GRAMMAR: a right regular grammar (or something close to right regular)
RESULT: a finite automaton"
  (unless (grammar-right-regular-p grammar)
    (setq grammar (grammar->right-regular grammar)))
  (let ((terminals (grammar-terminals grammar))
        (nonterminals (grammar-nonterminals grammar))
        (edges)
        (start (caar grammar))
        (new-accept (gensym "ACC")))
    (grammar-map nil
                 (lambda (lhs rhs)
                   (cond
                     ;; A -> a
                     ((and (= 1 (length rhs))
                           (finite-set-member terminals (first rhs)))
                      (push (list lhs (first rhs) new-accept)
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
    (make-fa edges start (finite-set-add accept new-accept))))

(defun fa->right-regular-grammar (fa &optional (unique (gensym)))
  "Convert FA to a right-regular grammar."
  (with-dfa (dfa fa)
    (let ((succs (fa-successors dfa))
          (accept (fa-accept dfa))
          (start (fa-start fa))
          (grammar (make-amortized-queue)))
      (labels ((production (q0 z &optional q1)
                 (setq grammar
                       (let ((production `(,(gsymbol-gen q0 unique)
                                            ,z
                                            ,@(when q1 (list (gsymbol-gen q1 unique))))))
                         (if (equal q0 start)
                             (amortized-queue-push grammar production)
                             (amortized-enqueue grammar production))))))
        (do-finite-set (q0 (fa-states dfa))
          (loop for (z q1) in (funcall succs q0)
             do
               (when (funcall succs q1)
                 (production q0 z q1))
               (when (finite-set-inp q1 accept)
                 (production q0 z))))
        (amortized-queue-list grammar)))))


(defun grammar-right-regular-minimize (grammar)
  (fa->right-regular-grammar (fa-canonicalize (grammar->fa grammar))))

(defun grammar-regular-expand-rule-first (rule regular-grammar &optional (unique (gensym)))
  "Expand first body symbol of RULE with REGULAR-GRAMMAR.
Assume no unit or epsilon rules in regular-grammar."
  (destructuring-bind (head regsym &rest rule-rest) rule
    (declare (ignore regsym))
    (let ((regstart (grammar-start-nonterminal regular-grammar)))
      (labels ((make-unique (symbol) (gsymbol-gen symbol unique)))
        (rewrite-grammar-list
         (lambda (regrule)
           (destructuring-bind (q0 z &optional q1) regrule
             (let ((uq0 (make-unique q0))
                   (uq1 (when q1 (make-unique q1))))
               (cond
                 ;; rule for start symbol
                 ((eq q0 regstart)
                  (if q1
                      ;; normal rule
                      (list (list head z uq1)
                            (list uq0 z uq1))
                      ;; accept on start
                      (list `(,head ,z ,@rule-rest)
                            `(,uq0 ,z ,@rule-rest))))
                 ;; normal rule
                 (q1 (list (list uq0 z uq1)))
                 ;; accept rule
                 (t `((,uq0 ,z ,@rule-rest)))))))
         regular-grammar :recursive nil)))))



(defun grammar-from-adjacency (adj &key
                               directed)
  (let ((incoming (make-hash-table :test #'equal))
        (outgoing (make-hash-table :test #'equal))
        (places (make-hash-table :test #'equal)))
    ;; index incoming, outgoing, and places
    (dolist (e adj)
      (destructuring-bind (q0 q1) e
        (push e (gethash q0 outgoing))
        (push e (gethash q1 incoming))
        (unless directed
          (let ((e (list q1 q0)))
            (push e (gethash q1 outgoing))
            (push e (gethash q0 incoming))))
        (setf (gethash q0 places) t
              (gethash q1 places) t)))
    (let ((edges))
      (loop for q being the hash-keys of places
         do
           (dolist (in (gethash q incoming))
             (push (list in q) edges)
             (dolist (out (gethash q outgoing))
               (push (list in q out) edges))))
      (assert (every (lambda (e)
                       (if (= 2 (length e))
                           (destructuring-bind ((q0 q1) z) e
                             (declare (ignore q0))
                             (equal q1 z))
                           (destructuring-bind ((q0 q1) z (q2 q3)) e
                             (declare (ignore q0 q3))
                             (and (equal q1 z) (equal q2 z)))))
                     edges))
      edges)))

(defmacro walk-grammar (name grammar (head-var body-var rest-var)
                        &body body)
  (alexandria:with-gensyms (grammar-var)
    `(labels ((,name (,grammar-var)
                (when ,grammar-var
                  (destructuring-bind ((,head-var &rest ,body-var)
                                       &rest ,rest-var ) ,grammar-var
                    ,@body))))
       (,name ,grammar))))


;; (defun apply-rewrite (function grammar recursive)
;;   "Rewrites GRAMMAR by applying FUNCTION.
;; FUNCTION: (lambda (term)) => (list new-terms...),
;;           must return a fresh list as it will be NCONC'ed"
;;   (let ((visited (make-finite-set :mutable t))   ;; stuff we've already expanded
;;         (included (make-finite-set :mutable t))) ;; stuff we've already included
;;     (labels ((visit-term (term)
;;                ;;(format t "visit ~A~&" term)
;;                (cond
;;                  ((finite-set-inp term included)
;;                   (assert (finite-set-nadd term visited))
;;                   ;;(format t "included ~A~&" term)
;;                   nil)
;;                  ((finite-set-inp term visited)
;;                   ;;(format t "visited ~A~&" term)
;;                   nil)
;;                   ;;(finite-set-nadd included term)
;;                   ;;(list term))
;;                  (t ;; new term
;;                   ;;(format t "new ~A~&" term)
;;                   (finite-set-nadd visited term) ;; don't re-visit already-rewritten terms
;;                   (visit-list (funcall function term)))))
;;              (visit-list (list)
;;                ;;(mapcan #'visit-term list)
;;                ;; the loop slightly outperforms mapcan
;;                (if recursive
;;                    (loop for term in list
;;                       nconc (visit-term term))
;;                    (mapcan function list))))
;;       (visit-list grammar))))



(defun rewrite-grammar-list (function grammar &key
                             (recursive t)
                             (keep-repeated))
  "Rewrites GRAMMAR by applying FUNCTION.
FUNCTION: (lambda (term)) => (list new-terms...),
          must return a fresh list as it will be NCONC'ed"
  (labels ((visit-term (term parent-set)
             ;;(format t "parent ~A~&" parent-set)
             ;;(format t "visit ~A~&" term)
             (cond
               ((and keep-repeated
                     (finite-set-inp term parent-set))
                (list term))
               ((finite-set-inp term parent-set)
                (error "Detected repetition of term ~A" term))
               (t (let ((new-terms (funcall function term)))
                    (if (and new-terms
                             (null (cdr new-terms))
                             (equal term (car new-terms)))
                        ;; Equivalent
                        new-terms
                        ;; Modified
                        (visit-list new-terms (finite-set-add parent-set term)))))))
           (visit-list (list parent)
             (loop for term in list
                nconc (visit-term term parent))))
    (if recursive
        (visit-list grammar (make-finite-set))
        (mapcan function grammar))))


(defun rewrite-grammar (function grammar &key (recursive t) keep-repeated)
  "Rewrites GRAMMAR by applying FUNCTION.
FUNCTION: (lambda (head body)) => (list new-productions...)"
  (rewrite-grammar-list (lambda (term)
                   (destructuring-bind (head &rest body) term
                     (funcall function head body)))
                 grammar
                 :recursive recursive
                 :keep-repeated keep-repeated))

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
           grammar
           :keep-repeated t))
    ;;(format t "~&grams : ~A~&" grammar)
    ;; possibly recurse
    (if modified
        (grammar-remove-epsilon grammar)
        grammar)))


;; Hopcroft '79 p. 91
(defun grammar-remove-unit (grammar)
  "Remove unit productions from the grammar."
  ;; FIXME: sometimes reorders and loses the start symbol
  (let* ((terminals (grammar-terminals grammar))
         (nonterminals (grammar-nonterminals grammar))
         (chainable (grammar-chainable-parent-function  grammar terminals nonterminals)))
    (rewrite-grammar-list
     (lambda (production)
       (unless (grammar-chain-rule-p terminals nonterminals production)
         (let* ((b (car production))
                (alpha (cdr production))
                (a-list (funcall chainable b)))
           (cons production
                 (finite-set-map 'list (lambda (a) (cons a alpha)) a-list)))))
     grammar :recursive nil)))


(defun grammar-print (grammar &key (output *standard-output*) (head-columns 12))
  (let ((terminals (grammar-terminals grammar)))
    (grammar-map nil (lambda (head body)
                       (let ((head-string (format nil "~A" head)))
                         (format output "~&<~A>~A ::=  ~{~A~^ ~}~%"
                                 head-string
                                 (make-string (max 0 (- head-columns (length head-string)))
                                              :initial-element #\Space)
                                 (map 'list (lambda (symbol)
                                              (if (finite-set-inp symbol terminals)
                                                  (format nil "\"~A\"" symbol)
                                                  (format nil "<~A>" symbol)))
                                      body))))
                 grammar)))


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
          (let ((xp (gsymbol-gen (second body))))
            (list (list head (first body) xp)
                  (cons xp (rest body)))))
         (t ;; else, remove the terminals
          (let ((new-body
                 (map 'list (lambda (x)
                              (if (finite-set-inp x terminals)
                                  (gsymbol-gen x)
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


;; Close enough to Hopcroft '79, p. 89
(defun grammar-remove-unreachable (grammar &optional
                                   (terminals (grammar-terminals grammar))
                                   (nonterminals (grammar-nonterminals grammar)))
  "Remove unreachable nonterminals and production sfrom the grammar.
RESULT: reduced grammar"
  (let ((body-function (grammar-body-function grammar))
        (start (grammar-start-nonterminal grammar)))
    (labels ((visit (visited A)
               ;; fold over expansion bodies of A
               (fold-finite-set (lambda (visited body)
                                  ;; fold over symbols of body
                                  (fold (lambda (visited x)
                                          (if (and (grammar-nonterminal-p terminals nonterminals x)
                                                   (not (finite-set-inp x visited)))
                                              (visit (finite-set-add visited x) x)
                                              visited))
                                        visited body))
                                visited (funcall body-function A))))
      (let ((reachable-nonterms (visit (finite-set start) start)))
        (rewrite-grammar (lambda (head body)
                           (when (finite-set-inp head reachable-nonterms)
                             (list (cons head body))))
                         grammar :recursive nil)))))


;; Hopcroft '79, p. 89
(defun grammar-remove-nonsentential (grammar &optional
                                     (terminals (grammar-terminals grammar)))
  "Remove productions which can derive no terminal string."
  (let ((old-v (make-finite-set))
        (new-v (grammar-fold (lambda (v head body)
                               (if (every (rcurry #'finite-set-inp terminals) body)
                                   (finite-set-add v head)
                                   v))
                             (make-finite-set) grammar)))
    (loop
       while (not (finite-set-equal old-v new-v))
       do
         (setq old-v new-v)
         (setq new-v (grammar-fold (lambda (v head body)
                                     (if (every (lambda (x)
                                                  (or (finite-set-inp x terminals))
                                                  (or (finite-set-inp x old-v)))
                                                body)
                                         (finite-set-add v head)
                                         v))
                                   old-v grammar)))
    (let* ((pred (rcurry #'finite-set-inp (finite-set-union terminals new-v))))
      (rewrite-grammar-list (lambda (production)
                              (when (every pred production)
                                (list production)))
                            grammar :recursive nil))))


(defun grammar-remove-useless (grammar &optional
                               (terminals (grammar-terminals grammar))
                               (nonterminals (grammar-nonterminals grammar)))
  "Remove 'useless' symbols from GRAMMAR."
  (grammar-remove-unreachable (grammar-remove-nonsentential grammar terminals)
                              terminals nonterminals))

;; Algorithm by:
;;   Blum, N. and Koch, R.  Greibach normal form transformation revisited.
;;   Information and Computation. 1999."
;; TODO: Implementation follows the Blum-Koch proof.  Efficiency could be improved.

(defun blum-koch-subgrammar (b grammar &optional
                             (terminals (grammar-terminals grammar))
                             (nonterminals (grammar-nonterminals grammar))
                             (chainable-function (grammar-chainable-function grammar terminals nonterminals))
                             (first-nonterminals-function (grammar-first-nonterminals-function grammar
                                                                                               terminals
                                                                                               nonterminals)))
  "Compute G-B.
RETURNS: (values S-B {A-B} P-B)"
  (let ((s-b (gsymbol-gen b 'start))
        (local-nonterms (make-hash-table :test #'equal))
        (chainable-set (finite-set-add (funcall chainable-function b) b))
        (first-nonterm-set (funcall first-nonterminals-function b)))
    ;; create local nonterminals, A-B
    (do-finite-set (a nonterminals)
      (setf (gethash a local-nonterms)
            (gsymbol-gen a b)))
    ;;(format t "~&terminals: ~A" terminals)
    ;;(format t "~&first-nonterms: ~A" first-nonterm-set)
    ;;(format t "~&chainable: ~A" chainable-set)
    ;; Results
    (values s-b
            local-nonterms
            (rewrite-grammar (lambda (head body)
                               (let ((first-terminal-p (grammar-terminal-p terminals nonterminals
                                                                           (car body)))
                                     (head-chainable (finite-set-inp head chainable-set))
                                     (head-first-nonterm (finite-set-inp head first-nonterm-set)))
                                 ;;(format t "~&prod ~A => ~A~&" head body)
                                ; (print first-terminal-p)
                                ; (print head-chainable)
                                ; (print head-first-nonterm)
                                 (append (when (and first-terminal-p head-chainable)
                                           ;; start production
                                           ;;(format t "~&start 1: ~A => ~A~&" head body)
                                           (list (cons s-b body)))
                                         ;; An apparently necessary modification to Blum-Koch:
                                         ;; Only include this start production when HEAD
                                         ;; can appear at the beginning of a leftmost derivation of
                                         ;; B.
                                         (when (and first-terminal-p head-first-nonterm)
                                           ;;(format t "~&start 2: ~A => ~A~&" head body)
                                           (list (cons s-b (append body
                                                                   (list (gethash head local-nonterms))))))
                                         ;; inner
                                         (when (and (not first-terminal-p)
                                                    head-first-nonterm ;; MODIFICATION
                                                    (not (equal b head)))
                                           ;;(format t "~&inner: ~A => ~A~&" head body)
                                           (list `(,(gethash (first body) local-nonterms)
                                                    ,@(rest body)
                                                    ,(gethash head local-nonterms))))
                                         ;; final
                                         (when (and (not first-terminal-p)
                                                    (cdr body)
                                                    head-chainable)
                                           ;;(format t "~&final: ~A => ~A~&" head body)
                                           (list (cons (gethash (first body) local-nonterms)
                                                       (rest body)))))))
                               grammar :recursive nil))))

(defun blum-koch-subgrammar-productions (b grammar)
  (third (multiple-value-list (blum-koch-subgrammar b grammar))))


(defun blum-koch-greibach (grammar)
  "Convert grammar to Greibach Normal Form using the Blum-Koch-Greibach algorithm"
  ;; FIXME: check what happens to start symbol
  (let ((terminals (grammar-terminals grammar))
        (nonterminals (grammar-nonterminals grammar))
        (start (grammar-start-nonterminal grammar)))
    (let ((first-nonterminals-function (grammar-first-nonterminals-function grammar terminals nonterminals))
          (chainable-function (grammar-chainable-function grammar terminals nonterminals))
          (b->s-b (make-hash-table :test #'equal))
          (s-b->b (make-hash-table :test #'equal))
          (b->p-b (make-hash-table :test #'equal)))
      ;; Sub-grammars
      (finite-set-map nil
                      (lambda (b)
                        (multiple-value-bind (s-b a-b p-b) (blum-koch-subgrammar b grammar
                                                                                 terminals
                                                                                 nonterminals
                                                                                 chainable-function
                                                                                 first-nonterminals-function)
                          (declare (ignore a-b))
                          (setf (gethash b b->s-b) s-b
                                (gethash s-b s-b->b) b
                                (gethash b b->p-b) p-b)))
                      nonterminals)
      (let ((h-grammar (finite-set-fold-range #'finite-set-union grammar b->p-b)))
        ;(grammar-print h-grammar)
        (setq h-grammar (rewrite-grammar (lambda (head body)
                                        ;(format t "~&Check: ~A -> ~A~&" head body)
                                           (let ((x-1 (car body)))
                                             (cond
                                               ;; remove head s-b
                                               ((finite-set-inp head s-b->b)
                                        ;(format t "~&  prune~&")
                                                nil)
                                               ;; replace non-greibach with leading s-b
                                               ((finite-set-inp x-1 nonterminals)
                                        ;(format t "~&  non-greibach~&")
                                                (list `(,head ,(gethash x-1 b->s-b) ,@(rest body))))
                                               ;; replace leading s-b with it's alternatives
                                               ((gethash x-1 s-b->b)
                                        ;(format t "~&  lead-s_b~&")
                                                (rewrite-grammar (lambda (head-1 body-1)
                                                                   (when (eq head-1 x-1)
                                                                     (list (cons head (append body-1
                                                                                              (rest body))))))
                                                                 (gethash (gethash x-1 s-b->b) b->p-b)
                                                                 :recursive nil))
                                               ;; otherwise, keep it
                                               (t (list (cons head body))))))
                                         h-grammar :recursive t))
        (setq h-grammar
              (grammar-remove-useless (cons (list 'start start) h-grammar) terminals))
        (grammar-remove-unit h-grammar)))))

(defun grammar-nonterm->terminal (grammar nonterm &optional (unique (gensym)))
  "Convert nonterminal symbol to terminal"
  (let ((new-nonterm (gsymbol-gen nonterm unique)))
    (rewrite-grammar (lambda (head body)
                       (list (cons (if (equal head nonterm)  ; maybe fix head
                                       new-nonterm
                                       head)
                                   ;; replace all occurrences of nonterm in body
                                   (mapcan (lambda (x) (if (equal x nonterm)
                                                      (list x new-nonterm)
                                                      (list x)))
                                           body))))
                     grammar :recursive nil)))

(defun grammar-left-factor (grammar &optional (unique (gensym)))
  "Left factor a grammar"
  (labels ((build (trie head)
             (labels ((traverse (node)
                        (if (cddr node)
                            (destructuring-bind (path &rest children) node
                              (let* ((new-head (gsymbol-gen (list head (car path)) unique))
                                     (new-prod `(,head ,@path ,new-head)))
                                (append (list new-prod) (build children new-head))))
                            (list (cons head (or (car node) '(:epsilon)))))))
               (apply #'append (mapcar #'traverse trie)))))
    (multiple-value-bind (fun keys) (index-finite-set grammar #'car #'cdr :duplicate-type 'list)
      (let* ((solve-group (lambda (head) (build (fold #'trie-insert nil (funcall fun head)) head)))
             (new-grammar (apply #'append (finite-set-map 'list solve-group keys)))
             (predicate (lambda (p1 p2) (equal (car p1) (grammar-start-nonterminal grammar)))))
        (sort new-grammar predicate)))))
