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

;;; ATN (Augmented Transition Networks) - A ATN is like a grammar made easily
;;; visualizeable (hence why I insisted to make it an fa so I can keep
;;; (fa-pdf)ing it.) The ATN strictly speaking isn't a finite
;;; automata, but it's a natural way to encode it
;;;
;;; This structure is helpful when creating the lookahead dfa in the LL-star
;;; algorithm but it might be used for other grammar-analyzing purposes
;;;
;;; The atn is also useful because in the LL-star algorithm, it can encode
;;; regular languages within the graph (think backedges in the atn graph). One
;;; might argue that one can do it by hand with the grammar, but that is
;;; incorrect. The dfa construction algorithm can't handle such grammar and
;;; will say that it's unconfidend if the grammar really is regular or not.
;;;
;;; For example this grammar won't work
;;;
;;; ((A |a| B |c|)
;;;  (A |a| B |d|)
;;;  (B :epsilon)
;;;  (B |b| B))
;;;
;;; However by making very small adjustments to the atn-construction algorithm we could support this syntax
;;;
;;; ((A |a| (kleene |b|) |c|)
;;;  (A |a| (kleene |b|) |d|))
;;;

;;;;;;;;;;;;;;;
;; ATN states
;;;;;;;;;;;;;;;

(defstruct (atn-state (:type list)) ;; TODO: Make vector. Requires refactoring where I've assumed list
  name  ;; String description AND unique identifier
  nonterminal ;; The nonterminal the state is within
  type ;; (or 'start 'mid 'final)
  prod-id ;; (or fixnum nil), When type is 'mid, then a fixnum for prodution id, nil otherwise
          ;; More precisely, the prod-id refers to the index in the original "list" of productions
  )

(defun init-atn-state (name nonterminal type pid)
  (make-atn-state
     :name name
     :nonterminal nonterminal
     :type type
     :prod-id pid))

(defun atn-start-name (head)
  (init-atn-state (format nil "p_~A" head) head 'start nil))

(defun atn-final-name (head)
  (init-atn-state (format nil "p_~A'" head) head 'final nil))

(defun atn-numeric-name (int head prod-id)
  (init-atn-state (format nil "p_~A" int) head 'mid prod-id))

(defun atn-state-compare (a b)
  "Compare two atn states"
  (string-compare (atn-state-name a) (atn-state-name b)))

(defun atn-state-equal (a b)
  (zerop (atn-state-compare a b)))

(defun atn-state-final-p (p)
  (equal 'final (atn-state-type p)))

;;;;;;;;;;;;;;;
;; ATN states
;;;;;;;;;;;;;;;

;; An atn should ideally be just a finite automata (or something quite similar
;; to it at least), here however we add stuff to it for efficiency reasons only
(defstruct atn
  "The reasons for the names dge/ee/edg is because they look like edge but
   without one of it's components. Its a nice human mnemonic"
  fa ;; The actual finite automata
  mem-dge ;; lambda (q0) => (list (list z q1)... )
  mem-ee ;; lambda (z) => (list (list q0 q1)... )
  )

(defun fa->atn (fa)
  "Create the atn that looks like the fa"
  (make-atn
    :fa fa
    :mem-dge (fa-index-custom fa (compose #'atn-state-name #'first) #'cdr)
    ;:mem-dge (let ((f (fa-successors fa)) (lambda (q0) (funcall f (atn-state-name q0))))) ;; This is the same but ugly
    :mem-ee (fa-bridges fa)))

(defun atn-dge (atn atn-state)
  "Convience. See mem-dge"
  (funcall (atn-mem-dge atn) (atn-state-name atn-state)))

(defun atn-ee (atn symbol)
  "Convience. See mem-ee"
  (funcall (atn-mem-ee atn) symbol))

(defun kleenep (terminal) ;; TODO find place to put
  (and (not (nonterminalp terminal)) (gsymbol-equal 'KLEENE (terminal-give-type terminal))))

(defun has-proposition-p (symbol) ;; TODO find place to put
  (or (kleenep symbol) (predicatep symbol)))

(defun grammar->ATN (grammar)
  "Return the ATN of the grammar. "
  (let ((state-counter 0)
        (prod-counter -1))
      (fa->atn (make-fa (apply #'append (grammar-map 'list (lambda (head body)
                                    (incf prod-counter)
                                    (labels ((chain (xs)
                                               (pattern-case xs
                                                 ((:pattern (:pattern 'kleene prop) &rest xs)
                                                  (unless (and (equal 'pred (caar xs)) (not (minisat (list 'and prop (cadar xs)))))
                                                    (error "Sorry, restriction of kleene star to have disjoint follow pred
Not because of atn but because you later don't know which terminal to take when you're NOT predicting.")
                                                    )
                                                  (cons (list (numeric state-counter) (list 'pred prop) (numeric state-counter))
                                                        (cons (list (numeric state-counter) :epsilon (numeric (incf state-counter))) (chain xs))
                                                        )
                                                  )
                                                 ((:pattern x &rest xs) (cons (list (numeric state-counter) x (numeric (incf state-counter))) (chain xs)))
                                                 (t nil)
                                                 ))
                                             (numeric (id) (atn-numeric-name id head prod-counter))
                                             )
                                    (let* ((beg (list (ATN-START-NAME head) :epsilon (numeric (incf state-counter))))
                                           (mids (chain body))
                                           (end (list (numeric state-counter) :epsilon (ATN-FINAL-NAME head)))
                                           )
                                      `(,beg ,@mids ,end)
                                      ))
                                    ) grammar)) nil nil))))
