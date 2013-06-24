;;;; -*- Lisp -*-
;;;;
;;;; Copyright (c) 2013, Georgia Tech Research Corporation
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


;; produce per-nonterminal-optimized ATN
;; Handles EBNF
;; Result: hash nonterm => fa
(defun grammar->eatn (grammar)
  (let* ((nonterms (grammar-nonterminals grammar))
         (prods (index-finite-set grammar #'car #'cdr
                                  :duplicate-type 'list))
         (fa-hash (make-hash-table :test #'equal)))
    ;; produce per-nonterminal fa
    (dolist (a nonterms)
      (setf (gethash a fa-hash)
            (regex->dfa (cons :union
                              (map 'list (lambda (e) (cons :concatenation e))
                                   (funcall prods a))))))
    ;; result
    fa-hash))

;; compute first sets for each nonterminal
;; result: hash nonterm => first-set
(defun eatn-first (fa-hash)
  (let* ((nonterms (hash-table-keys fa-hash))
         (term-hash (make-hash-table :test #'equal)) ; nonterm => first
         (nonterm-hash (make-hash-table :test #'equal))) ; nonterm => first
    (dolist (a nonterms)
      ;; initial first terminals
      (setf (gethash a term-hash)
            (fold-fa-edges (lambda (acc p z q)
                             (declare (ignore q))
                             (if (and (= p 0)
                                      (not (finite-set-inp z nonterms)))
                                 (finite-set-add acc z)
                                 acc))
                           nil (gethash a fa-hash)))
      ;; first nonterminals
      (setf (gethash a nonterm-hash)
            (fold-fa-edges (lambda (acc p z q)
                             (declare (ignore q))
                             (if (and (= p 0)
                                      (finite-set-inp z nonterms))
                                 (finite-set-add acc z)
                                 acc))
                           nil (gethash a fa-hash))))
    ;; compute fixpoint
    (labels ((fix ()
               (let ((updated nil))
                 (dolist (a nonterms)
                   (let ((nonterm-first
                          (fold-finite-set
                           (lambda (acc b)
                             (finite-set-union acc (gethash b term-hash)))
                           nil (gethash a nonterm-hash))))
                     (unless (finite-set-subsetp nonterm-first
                                                 (gethash a term-hash))
                       (setq updated t)
                       (setf (gethash a term-hash)
                             (finite-set-union (gethash a term-hash)
                                               nonterm-first)))))
                 updated)))
      (loop while (fix)))
    term-hash))



;; result: (lambda (nonterm-0 state-0 term)) =>
;;     (values nonterm-1 state-nonterm-1 (or :jump :call) state-1
(defun eatn-predictive-table (fa-hash)
  (let* ((nonterms (hash-table-keys fa-hash))
         (firsts (eatn-first fa-hash))
         (movers (make-hash-table :test #'equal))
         (succs (make-hash-table :test #'equal))
         (hash (make-hash-table :test #'equal)))
    ;; index fa moves
    (dolist (a nonterms)
      (setf (gethash a movers) (dfa-mover (gethash a fa-hash))
            (gethash a succs) (fa-successors (gethash a fa-hash))))
    (labels ((key (a q z)
               (list a q z))
             (add-list (a0 q0 z a1 q11 type q01)
               (assert (or (eq :jump type)
                           (eq :call type)))
               (let ((k (key a0 q0 z)))
                 (assert (null (gethash k hash))) ;; LL(1) constraint
                 (setf (gethash k hash)
                       (list a1 q11 type q01))))
             (add-nonterm (a0 q0 a1 q1)
               (let* ((fa0 (gethash a0 fa-hash))
                      (fa1 (gethash a1 fa-hash))
                      (q01 (funcall (gethash a0 movers) q0 a1))
                      ;; if next state in orignal fa
                      ;; is accept and has no successors
                      ;; then jump, else call
                      (type (if (and (finite-set-inp q1 (fa-accept fa0))
                                     (null (funcall (gethash a0 succs) q1)))
                                :jump :call)))
                 ;; for each in first of a1, transition to the successor state
                 (do-finite-set (z (gethash a1 firsts))
                   (add-list a0 q0 z
                             a1 (funcall (gethash a1 movers) (fa-start fa1) z)
                             type q01))))
             (add (a q0 z q1)
               (if (finite-set-inp z nonterms)
                   (add-nonterm a q0 z q1)
                   (add-list a q0 z a q1 :jump q1))))
      (dolist (a nonterms) ;foreach nonterm
        (do-finite-set (q0 (fa-states (gethash a fa-hash))) ;; foreach state
          (loop for (z q1) in (funcall (gethash a succs) q0)
             do (add a q0 z q1))))
      (lambda (nonterm state term)
        (apply #'values (gethash (key nonterm state term) hash))))))

;; TODO: start with start symbol
(defun grammar->c-ell1-parser (grammar &key
                             output
                             supervised
                             (function-name "mgparse")
                             (context-type "void*"))
  (let* ((fa-hash (grammar->eatn grammar))
         (start-symbol (grammar-start-nonterminal grammar))
         (nonterms (cons start-symbol
                         (finite-set-remove (grammar-nonterminals grammar)
                                            start-symbol)))
         (terminals
          (fold (lambda (acc a)
                  (finite-set-union acc
                                    (finite-set-tree (fa-terminals (gethash a fa-hash)))))
                (finite-set-tree nil)
                nonterms))
         (term-number (finite-set-enumerate terminals))
         (pred-table (eatn-predictive-table fa-hash))
         (count-recurse (make-hash-table :test #'equal)) ; a q => n
         (succs (make-hash-table :test #'equal))
         (case-hash (make-hash-table :test #'equal)))
    (print nonterms)
    ;; index successors
    (dolist (a nonterms)
      (setf (gethash a succs)
            (fa-successors (gethash a fa-hash))))
    ;; index recursion targets
    (setf (gethash (list start-symbol
                         (fa-start (gethash start-symbol fa-hash)))
                   count-recurse)
          1)
    (dolist (a nonterms)
      (do-finite-set (q (fa-states (gethash a fa-hash)))
        (do-finite-set (z terminals)
          (multiple-value-bind (b qq type q1)
              (funcall pred-table a q z)
            (declare (ignore q1))
            (when (eq :call type)
              (incf (gethash (list b qq) count-recurse 0)))))))
    ;; index case numbers
    (let ((i -1))
      (do-finite-set (a nonterms)
        (do-finite-set (q (fa-states (gethash a fa-hash)))
          (let ((k (list a q)))
          (when (gethash k count-recurse)
            (setf (gethash k case-hash)
                  (incf i)))))))
    ;; emit
    (labels ((jmp-label (a q)
               (format nil "label_~A_~d"
                       (csymbol a) q))
             (call-idx (a q)
               (gethash (list a q) case-hash))
             (set-super (z)
               (when supervised
                 `((:= (:-> "table" "state")
                       (:call"mg_supervisor_next_state" "table"
                             ,(funcall term-number z))))))
             (recurse-call (i)
               `(:call ,function-name
                       "context"
                       ,@(when supervised (list "table"))
                       ,i ))
             (term-test (z)
               (if supervised
                   `(:&& (:call "mg_supervisor_allow" "table"
                                ,(funcall term-number z))
                         ,(c-parser-test z "context"))
                   (c-parser-test z "context")))
             (accept-p (a q)
               (finite-set-inp q (fa-accept (gethash a fa-hash))))
             (state-jump (a q)
               (if (accept-p a q)
                   '(:goto label-ok)
                   `(:goto ,(jmp-label a q))))
             (state-clauses (a q)
               (if (accept-p a q)
                   '(:goto label-ok)
                   ;(let ((clauses '(:return -1)))
                   (fold-finite-set
                    (lambda (clauses z)
                      (multiple-value-bind (b qq type q1)
                          (funcall pred-table a q z)
                        (if type
                            `(:if ,(term-test z)
                                  (,@(set-super z)
                                     ,(ecase
                                       type
                                       (:jump
                                        (state-jump b qq))
                                       (:call
                                        `(:if ,(recurse-call (call-idx b qq))
                                              ((:goto label-fail))
                                              ((:goto ,(jmp-label a q1)))))))
                                  (,clauses))
                            clauses)))
                    '(:goto label-fail)
                    terminals))))
      (with-c-output output
        (c-indent-format "/*****************/~&")
        (c-indent-format "/* MOTION PARSER */~&")
        (c-indent-format "/*****************/~&")
        (with-c-nest ((c-indent-format "int ~A( ~A context, ~A int i )"
                                       function-name
                                       context-type
                                       (if supervised
                                           "mg_supervisor_table_t *table,"
                                           "")))
          ;; nonterms
          (with-c-nest ((c-indent-format "switch( i )"))
            (do-finite-set (a nonterms)
              (do-finite-set (q (fa-states (gethash a fa-hash)))
                (when (gethash (list a q) count-recurse)
                  (c-gen `(:case ,(call-idx a q)))
                  (c-gen `(:label ,(jmp-label a q)))
                  (c-gen `(:comment ,(format nil "<~A:~d>" a q)))
                  (c-gen (state-clauses a q))))))
          (do-finite-set (a nonterms)
              (do-finite-set (q (fa-states (gethash a fa-hash)))
                (unless (or (gethash (list a q) count-recurse)
                            (accept-p a q))
                  (c-gen `(:label ,(jmp-label a q)))
                  (c-gen `(:comment ,(format nil "<~A:~d>" a q)))
                  (c-gen (state-clauses a q)))))
          (c-gen '(:label label-fail))
          (c-gen '(:return -1))
          (c-gen '(:label label-ok))
          (c-gen '(:return 0)))))))
