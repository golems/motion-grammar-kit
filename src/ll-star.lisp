;;;; -*- Lisp -*-
;;;;
;;;; Copyright (c) 2013-2013, Georgia Tech Research Corporation
;;;; All rights reserved.
;;;;
;;;; Author(s): Arash Rouhani <rarash@gatech.edu>
;;;; Georgia Tech Humanoid Robotics Lab
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

(defstruct (dd) ;; dd - A soon to be state in the predictive automaton (not an ATN-state!)
  configurations  ;; Set of configurations contained within the dd
  busy-set ;; Set of configurations that closure already have visited
  recursive-alternatives ;; Set of productions that have ever recursed when closuring
  overflown ;; We set this if we have overflown
  non-regular ;; We set this if we suspect that the grammar is non-regular
  name ;; An optional pretty name for the node
  )

(defstruct (atnconf (:type list)) ;; TODO: Make vector. Requires refactoring where I've assumed list
  state ;; An atn-state - where we believe we currently are
  alternative ;; Fixnum, the index of the production
  stack ;; Represented with a list of atn-states (Turned out inefficient. We often have to reverse it)
  )

(defun dd-equivalent (a b)
  ;; TODO Make it O(nlog(n)) or at least smarter than O(n^2)!
  (finite-set-equivalent #'atnconf-equivalent (dd-configurations a) (dd-configurations b)))

(defun dd-unique-atn-states (ds)
  ;; TODO Can we avoid using remove-duplicates?
  (remove-duplicates (map-finite-set 'list #'atnconf-state (dd-configurations ds)) :TEST #'equal))

(defun fill-atnconf (state alternative stack)
  (make-atnconf
    :state state
    :alternative alternative
    :stack stack))

(defun empty-atnconf-set ()
  (make-finite-set :compare #'atnconf-compare))

(defun singleton-atnconf-set (elem)
  (finite-set-add (empty-atnconf-set) elem))

(defun make-empty-dd (&optional (name "default"))
  (make-dd
    :configurations (empty-atnconf-set)
    :busy-set (empty-atnconf-set)
    :recursive-alternatives (make-finite-set :compare #'fixnum-compare)
    :overflown nil
    :non-regular nil
    :name name))

(defun atnconf-compare (a b)
  (or-compare
    (atn-state-compare (atnconf-state a) (atnconf-state b))
    (fixnum-compare (atnconf-alternative a) (atnconf-alternative b))
    (stack-compare (atnconf-stack a) (atnconf-stack b))))

(defun stack-compare (a b)
  (gsymbol-compare (mapcar #'atn-state-name a) (mapcar #'atn-state-name b)))

(defun atnconf-equivalent (a b)
  (and (atn-state-equal (atnconf-state a) (atnconf-state b))
       (= (atnconf-alternative a) (atnconf-alternative b))
       (stack-equivalent (atnconf-stack a) (atnconf-stack b))))

(defun stack-equivalent (a b)
  (stack-equivalent-helper (reverse a) (reverse b)))

(defun stack-equivalent-helper (a b)
  (cond
    ((null a) t)
    ((null b) t)
    ((atn-state-equal (car a) (car b)) (stack-equivalent (cdr a) (cdr b)))
    (t nil)))

(defmacro replicate (num formula)
  "If formula is a function, the function will be called multiple times"
  `(vector ,@(make-list num :initial-element formula)))

(defmacro dd-configurations-union (state rhs-set)
  `(setf (dd-configurations ,state) (finite-set-union (dd-configurations ,state)
                                                      ,rhs-set)))

(defmacro += (lhs rhs)
  "Take union of lhs and rhs and store in lhs"
  `(setf ,lhs (finite-set-union ,lhs ,rhs)))

(defun nonterminalp (sym)
  (and (symbolp sym) (string-upcase-p (symbol-name sym))))

(defun terminal-string-p (sym)
  "A sensible predicate for when not in mgmode"
  (string-downcase-p (symbol-name sym)))

(defun atn-closure (atn ds atn-config)
  (destructuring-bind (p i gamma) atn-config
    (if (finite-set-inp atn-config (dd-busy-set ds))
      (return-from atn-closure (empty-atnconf-set))
      (setf (dd-busy-set ds) (finite-set-add (dd-busy-set ds) atn-config )))
    (let ((res (singleton-atnconf-set atn-config)))
      (labels ((recurse (new-p new-gamma) (atn-closure atn ds (list new-p i new-gamma))))
        (when (atn-state-final-p p)
          ;(print "wohoo-final")
          (if (car gamma)
            (+= res (recurse (car gamma) (cdr gamma)))
            (mapcar (lambda (ee)
                      (+= res (recurse (second ee) nil))) (atn-ee atn (atn-state-nonterminal p)))))
        (mapcar (lambda (dge)
                  (pattern-case dge
                                ((:pattern :epsilon q)
                                 ;(print "wohoo-eps")
                                 (+= res (recurse q gamma)))
                                ((:pattern sym q) ;; TODO: replace sym with (and) pattern
                                 (when (nonterminalp sym)
                                   ;(print "wohoo-nonterminal-gonna-recurse")
                                   (let ((depth (count q gamma)))
                                     (when (>= depth 1)
                                       ;(print (dd-name ds))
                                       ;(print atn-config)
                                       ;(print (dd-recursive-alternatives ds))
                                       (finite-set-nadd (dd-recursive-alternatives ds) i)
                                       (setf (dd-recursive-alternatives ds) (finite-set-add (dd-recursive-alternatives ds) i))
                                       (when (> (finite-set-length (dd-recursive-alternatives ds)) 1)
                                         (setf (dd-non-regular ds) t)
                                         (error (format nil "Likely not a LL regular grammar, these alternatives have visited ~A"
                                                        (dd-recursive-alternatives ds))))
                                       (when (> depth 10) ; Some limit
                                         (setf (dd-overflown ds) t)
                                         (error (format nil "Recursion limit overflow in atnconfig ~A" atn-config))
                                         (return-from atn-closure res)))
                                        (+= res (recurse (ATN-START-NAME sym) (cons q gamma)))
                                        )
                                      )
                                    ))) (atn-dge atn p))
        res))))

(defmacro while (test &rest body)
  `(do ()
     ((not ,test))
     ,@body))

(defun find-predicted-alternatives (ds)
  ;(apply #'finite-set (finite-set-map 'list #'atnconf-alternative configs))
  (remove-duplicates (apply #'finite-set (finite-set-map 'list #'atnconf-alternative (dd-configurations ds)))))

(defun atn-move (atn ds terminal &optional (mgmode nil))
  "Which are the reachable configurations from ds if we would consume terminal.
   In mgmode we don't think of it as consuming anymore but more like matching"
  ;; We assume any terminal is satisfiable
  ;; Note: In an atn graph, there can never be more than one terminal edge from any node
  ;; This means that given that the configurations are unique, so will our
  ;; output be even if we don't put it in a set ever
  (labels ((move-terminal-matches (other-symbol) (cond
                ((nonterminalp other-symbol) nil)
                ((not mgmode) (equal terminal other-symbol))
                ((mutatorp other-symbol) t)
                (t (minisat (list 'and (terminal-get-prop other-symbol) terminal))))))

    ;; TODO: using finite-set-list is fail
    (loop for atnconf in (finite-set-list (dd-configurations ds))
          for p = (atnconf-state atnconf)
          for dge = (car (atn-dge atn p))
          when dge
          when (move-terminal-matches (first dge))
          collect (fill-atnconf (second dge) (atnconf-alternative atnconf) (atnconf-stack atnconf)))))

(defun atn-get-terminals (atn terminalp ds)
  "Which are the terminals we are able to take from ds
   TERMINALP: Is parametrized. It should be different if we're in mgmode"
  ;; TODO: Make O(nlogn) but that requires refactoring, we have to kinda pass terminal-comparator rather than terminalp
  ;; But ok, lets not do premature optimization yet
  (remove-duplicates (apply #'append (map-finite-set 'list (lambda (p1)
                                                             (remove-if-not terminalp (mapcar #'first (atn-dge atn p1)))
                                                             ) (dd-unique-atn-states ds))) :test #'gsymbol-equal))

(defun atnconf-conflicts (a b)
  "Yes if a and b are in conflict"
  (and (/= (atnconf-alternative a) (atnconf-alternative b))
       (atn-state-equal (atnconf-state a) (atnconf-state b))
       (stack-equivalent (atnconf-stack a) (atnconf-stack b))))

(defun dd-calc-conflicts (ds)
  (remove-if (compose (curry #'= 1) #'length)
             (partition-finite-set (dd-configurations ds) #'atnconf-conflicts)))

;;;;;
;; Predicates/Mutators as terminal functions
;;;;;

(defun terminal-give-type (terminal)
  (car terminal))

(defun mutatorp (terminal)
  (and (not (nonterminalp terminal)) (gsymbol-equal 'MU (terminal-give-type terminal))))

(defun predicatep (terminal)
  (and (not (nonterminalp terminal)) (gsymbol-equal 'PRED (terminal-give-type terminal))))

(defun terminal-get-prop (terminal)
  (assert (or (predicatep terminal) (kleenep terminal)))
  (second terminal))

(defun terminal-get-effect (terminal)
  (assert (mutatorp terminal))
  (second terminal))

(defun conjunction-is-satisfiable (propositions)
  (minisat (fold (lambda (acc b) (list 'and acc b)) t propositions)))

(defun disjunction-is-satisfiable (propositions)
  (minisat (fold (lambda (acc b) (list 'or acc b)) 'nil propositions)))

(defun disjunction-is-tautology (propositions)
  "Not same as (not disjunction-is-satisfiable)!!"
  (prop-tautology-p (fold (lambda (acc b) (list 'or acc b)) 'nil propositions)))

(defun nonempty-negating-powerset (xs)
  "Return the power set of xs but without the empty set"
  (loop for i from 1 to (1- (expt 2 (length xs))) collect
        (loop for j below (length xs) for x in xs if (logbitp j i) collect x else collect `(not ,x))))

(defun conjoin-propositions (propositions)
  "(conjoin (A B C) ==> (A and B and C)"
  (if (car propositions)
    (fold (lambda (acc b) (list 'and acc b)) (car propositions) (cdr propositions))
    t))

(defun mgmode-power-set-terminals (old-terminals)
  "In the motion grammar mode, we want a set of new terminals. The new
   terminals should pretty much be the power set of the old terminals but only
   if the conjunctions are satisfiable. Also, this function will check that the
   semantically LL(1) property is held."
  ;; TODO: Using powerset explicitly is costly. One should not prove that (p1 & p2 & p3) is satisfiable if (p1 & p2) isn't!
  (let ((mutator (find-if #'mutatorp old-terminals)))
    (if mutator
      (if (notevery (curry #'gsymbol-equal mutator) old-terminals)
        (error (format nil "Not semantically LL(1) because these terminals are considered simultenously: ~A" old-terminals))
        (list mutator))
      ;; Now we also check that their disjunction covers all cases
      ;; Now we also check that their disjunction covers all cases
      (let ((propositions (mapcar #'terminal-get-prop old-terminals)))
        (unless (disjunction-is-tautology propositions)
          ;(format t "Warning: These propositions ~A don't cover all cases" propositions)
          )
        ;; (when (not  (disjunction-is-satisfiable propositions))
        ;;   (error (format nil "The propositions ~A are all (indenpendly!) unproveable" propositions)))
        (finite-set-filter #'minisat (map-finite-set 'list (compose #'prop-simplify #'conjoin-propositions)
                                                     (nonempty-negating-powerset propositions)))))))

(defun atn-create-dfa (atn nonterminal &optional (mgmode t))
  "Create a lookahead dfa. This is the heart algorithm of LL-star.
ATN: The atn. The type of it's terminals will depend on MGMODE or not
NONTERMINAL: The nonterminal who's lookahead dfa you want
MGMODE: If true, think of terminals in your as (or (PRED proposition) (MU name-of-c-function))
        If false, it means our terminals are in |this| |kind| |of| |form|.

   Note: You probably will only use this function directly in production. Hence
   why mgmode is true by default. It might be more convenient to use create-dfa
   while playing around."
  (let* ((atn-state (ATN-start-name nonterminal))
         (ds0 (make-empty-dd "s0"))
         (states nil) ;; TODO Is it possible to use more efficient structure? I doubt it
         (queue nil)
         (final-states (map 'vector #'identity (loop for i from 1 to 1000 collect (make-empty-dd "remove"))))
         ;; TODO: replace 1000 with (length grammar)
         (reachable-alternatives (make-finite-set))
         (edges nil)
         (counter 0) ;; For name generation, cause gensyms are ugly
         (terminalp (if mgmode #'consp #'terminal-string-p)))
    (mapcar (lambda (dge)
                     (+= (dd-configurations ds0)
                         (atn-closure atn ds0 (list (second dge) (atn-state-prod-id (second dge)) nil))))
                  (atn-dge atn atn-state))
    (setf (dd-busy-set ds0) "Forgotten") ;; To make prints more readable
    (assert (not (finite-set-empty-p (dd-configurations ds0))))
    (push ds0 queue)
    (push ds0 states)
    (while (car queue)
           (let* ((ds (pop queue))
                  (terminals (atn-get-terminals atn terminalp ds)))
             (when mgmode
               (setf terminals (mgmode-power-set-terminals terminals)))
             (finite-set-map 'nil
                             (lambda (terminal)
                               (let* ((moveset (atn-move atn ds terminal mgmode))
                                      (ds-new (make-empty-dd (format nil "s~A" (incf counter))))
                                      (clos (curry #'atn-closure atn ds-new)))
                                 (+= (dd-configurations ds-new) (fold-finite-set (lambda (acc atnconf)
                                                                                   (finite-set-union acc (funcall clos atnconf)))
                                                                                 (empty-atnconf-set)
                                                                                 moveset))
                                 (setf (dd-busy-set ds-new) "Forgotten") ;; To make prints more readable
                                 (let ((search-result (find ds-new states :test #'dd-equivalent)))
                                   (if search-result
                                     (setf ds-new search-result)
                                     (progn ;; Here we start to do some checking for ambiguities and overflows etc
                                       (cond
                                         ((dd-overflown ds-new) (error "Recursion limit overflow, couldn't synthesize"))
                                         ((dd-non-regular ds-new) (error "I can't show that the grammar is LL-regular, sorry I can't make a dfa"))
                                         ((car (dd-calc-conflicts ds-new)) (error (format nil "Ambigious grammar, conlict set: ~A"
                                                                                          (dd-calc-conflicts ds-new)))))
                                       (push ds-new states)))
                                   (if-pattern (:pattern j) (find-predicted-alternatives ds-new)
                                               (setf ds-new (aref final-states j)
                                                     (dd-name ds-new) (format nil "final=>~A" j)
                                                     reachable-alternatives (finite-set-add reachable-alternatives j))
                                               (when (not search-result) (push ds-new queue))))
                                 (push (list (dd-name ds) terminal (dd-name ds-new)) edges))
                               ) terminals)))
    (unless (finite-set-equal (find-predicted-alternatives ds0) reachable-alternatives)
      (error "Some productions in ~A are unexpandable, namely these ~A"
             nonterminal
             (finite-set-difference (find-predicted-alternatives ds0) reachable-alternatives)))
    (make-fa edges (dd-name ds0) (remove-if (curry #'string= "remove") (map 'list #'dd-name final-states)))))

(defun create-dfa (grammar nonterminal &optional (mgmode nil))
  "Just an inefficient convenience over atn-create-dfa. Useful when playing
   around and testing"
  (atn-create-dfa (grammar->ATN grammar) nonterminal mgmode))
