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


(defun grammar-terminalp (terminals nonterminals gsymbol)
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

(defun grammar-nonterminalp (terminals nonterminals gsymbol)
  (not (grammar-terminalp terminals nonterminals gsymbol)))

(defun grammar-chain-rule-p (terminals nonterminals production)
  "Is PRODUCTION a chain rule (A -> B)?"
  (assert (grammar-nonterminalp terminals nonterminals (car production)))
  (and (= 2 (length production))
       (grammar-nonterminalp terminals nonterminals (second production))))


(defun grammar-nonterminal-fixpoint (function grammar)
  "Compute a fixpoint mapping from nonterminals of grammar to some set.
FUNCTION: (lambda head body current-set mapping-function) => next-set of head
GRAMMAR: a list of productions"
  (let ((hash (make-hash-table :test #'equal)))
    (flet ((mapping (x) (gethash x hash)))
      (loop
         for modified = nil
         do (loop
               for (head . body) in grammar
               for current-set = (mapping head)
               for next-set = (funcall function head body current-set #'mapping)
               unless (finite-set-equal current-set next-set)
               do
                 ;(format t "current: ~A~&" current-set)
                 ;(format t "next: ~A~&" next-set)
                 (setf modified t
                        (gethash head hash) next-set))
           while modified)
      #'mapping)))

(defun grammar-chainable-function (grammar &optional
                                   (terminals (grammar-terminals grammar))
                                   (nonterminals (grammar-nonterminals grammar)))
  "Compute nonterminals reachable for each nonterminal of the grammar using only chain rules.
RESULT: (lambda nonterminal) => finite-set of chainable child nonterminals"
  (grammar-nonterminal-fixpoint (lambda (head body head-set mapping-function)
                                  (declare (ignore head))
                                  (finite-set-add (finite-set-union head-set
                                                                    (funcall mapping-function (car body)))
                                                  (car body)))
                                ;; consider only chain rules
                                (finite-set-filter (lambda (rule)
                                                     (grammar-chain-rule-p terminals nonterminals rule))
                                                   grammar)))

(defun grammar-chainable-parent-function (grammar &optional
                                    (terminals (grammar-terminals grammar))
                                    (nonterminals (grammar-nonterminals grammar)))
  "Compute nonterminals reachable for each nonterminal of the grammar using only chain rules.
RESULT: (lambda nonterminal) => finite-set of chainable parent nonterminals"
  (let* ((chainable (grammar-chainable-function grammar terminals nonterminals)))
    (curry-right #'gethash
                 (finite-set-fold (lambda (hash A)
                                    (finite-set-fold (lambda (hash B)
                                                       (setf (gethash b hash)
                                                             (finite-set-add (gethash b hash) a))
                                                       hash)
                                                     hash (funcall chainable a)))
                                  (make-hash-table :test #'equal)
                                  nonterminals))))

(defun grammar-body-function (grammar)
  "Indexes productions by head.
GRAMMAR: a grammar
RESULT: (lambda (nonterminal)) => set of production bodies NONTERMINAL expands to."
  (curry-right #'gethash
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
  (grammar-nonterminal-fixpoint (lambda (head body head-set mapping-function)
                                  (declare (ignore head))
                                  (finite-set-add (finite-set-union head-set
                                                                    (funcall mapping-function (car body)))
                                                  (car body)))
                                ;; only productions with leading nonterminal
                                (finite-set-filter (lambda (rule)
                                                     (grammar-nonterminalp terminals nonterminals (second rule)))
                                                   grammar)))


;; (defun grammar-index-chainable (terminals nonterminals grammar)
;;   "Compute nonterminals reachable for each nonterminal of the grammar using only chain rules.
;; RESULT: (lambda (nonterminal)) => finite-set of chainable nonterminals"
;;   (let ((hash (make-hash-table :size (length nonterminals) :test #'equal))
;;         (chain-rules (finite-set-filter (lambda (rule)
;;                                           (when (grammar-chain-rule-p terminals nonterminals rule)))
;;                                         grammar)))
;;     ;; initialize reachable sets to self
;;     (finite-set-map nil (lambda (nonterm)
;;                           (setf (gethash nonterm hash)
;;                                 (finite-set-add (make-finite-set) nonterm)))
;;                     nonterminals)
;;     ;; iteratively update sets
;;     (loop
;;        for modified = nil
;;        do (grammar-map-list nil
;;                             (lambda (rule)
;;                               (destructuring-bind (a b) rule
;;                                 (let ((a-reachable (gethash a hash))
;;                                       (b-reachable (gethash b hash)))
;;                                 (unless (finite-set-subsetp b-reachable a-reachable)
;;                                   (setf (gethash a hash) (finite-set-union a-reachable b-reachable)
;;                                         modified t)))))
;;                             chain-rules)
;;        while modified)
;;     (lambda (nonterm)
;;       (gethash nonterm hash))))


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
                                  (gsymbol-gen y))))
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
               (finite-set-fold (lambda (visited body)
                                  ;; fold over symbols of body
                                  (fold (lambda (visited x)
                                          (if (and (grammar-nonterminalp terminals nonterminals x)
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
                               (if (every (curry-right #'finite-set-inp terminals) body)
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
    (let* ((pred (curry-right #'finite-set-inp (finite-set-union terminals new-v))))
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
                               (let ((first-terminalp (grammar-terminalp terminals nonterminals
                                                                         (car body)))
                                     (head-chainable (finite-set-inp head chainable-set))
                                     (head-first-nonterm (finite-set-inp head first-nonterm-set)))
                                 ;;(format t "~&prod ~A => ~A~&" head body)
                                ; (print first-terminalp)
                                ; (print head-chainable)
                                ; (print head-first-nonterm)
                                 (append (when (and first-terminalp head-chainable)
                                           ;; start production
                                           ;;(format t "~&start 1: ~A => ~A~&" head body)
                                           (list (cons s-b body)))
                                         ;; An apparently necessary modification to Blum-Koch:
                                         ;; Only include this start production when HEAD
                                         ;; can appear at the beginning of a leftmost derivation of
                                         ;; B.
                                         (when (and first-terminalp head-first-nonterm)
                                           ;;(format t "~&start 2: ~A => ~A~&" head body)
                                           (list (cons s-b (append body
                                                                   (list (gethash head local-nonterms))))))
                                         ;; inner
                                         (when (and (not first-terminalp)
                                                    head-first-nonterm ;; MODIFICATION
                                                    (not (equal b head)))
                                           ;;(format t "~&inner: ~A => ~A~&" head body)
                                           (list `(,(gethash (first body) local-nonterms)
                                                    ,@(rest body)
                                                    ,(gethash head local-nonterms))))
                                         ;; final
                                         (when (and (not first-terminalp)
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
