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


;; The McNaughton-Yamada-Thompson algorithm
;; Aho 2nd Ed., P 159

(defun regex->nfa (regex &optional terminals)
  "Convert a regular expression to an NFA."
  (let ((state-counter 0)
        (edges nil))
    (labels ((is-op (symbol tree)
               (and (listp tree)
                    (eq symbol (first tree))))
             (new-edge (state-0 token state-1)
               (push (list state-0 token state-1) edges))
             (visit (start tree)
               ;; recursively visit the tree
               (cond
                 ((is-op :concatenation tree)
                  (reduce #'visit (cdr tree)
                          :initial-value start))
                 ((is-op :union tree)
                  (let ((end (incf state-counter)))
                    (map nil (lambda (tree)
                               (new-edge (visit start tree) :epsilon end))
                         (cdr tree))
                    end))
                 ((is-op :closure tree)
                  (assert (= 2 (length tree)))
                  (let* ((start2 (incf state-counter))
                         (end (visit start2 (cadr tree))))
                    (new-edge start :epsilon start2)
                    (new-edge start :epsilon end)
                    (new-edge end :epsilon start2)
                    end))
                 (t (push (list start tree (incf state-counter)) edges)
                    state-counter)
                 ;(error "Unknown tree ~A" tree)
                 )))
      ;; visit the start
      (let ((final (visit 0 regex)))
        (let ((fa (make-fa edges 0 (finite-set final))))
          (when terminals
            (setf (fa-terminals fa) (finite-set-tree terminals)))
          fa)))))

(defun regex->dfa (regex &optional terminals)
  "Convert a regular expression to a DFA."
  (fa-canonicalize (regex->nfa regex terminals)))

(defun regex->dfa-brzozowski (regex &optional terminals)
  "Convert a regular expression to a DFA with brzozowski's minimization algorithm"
  (fa-canonicalize-brzozowski  (regex->nfa regex terminals)))


(defun regex-apply (operator &rest args)
  (let ((args (loop for a in args
                 when (and a (not (eq :epsilon a)))
                 appending (if (and (listp a) (eq operator (car a)))
                               (cdr a)
                               (list a)))))
    (cond
      ((null args) nil)
      ((and (null (cdr args))
            (or (eq operator :union)
                (eq operator :concatenation)))
       (car args))
      (t (cons operator args)))))


(defun regex-simplify (regex)
  (etypecase regex
    (atom regex)
    (list
     (destructuring-bind (op &rest rest) regex
         (case op
           ((:union :concatenation)
            (if (null (cdr rest))
                (regex-simplify (car rest))
                (cons op
                      (loop
                         for e in rest
                         for f = (regex-simplify e)
                         appending (if (and (listp f)
                                            (eq (car f) op))
                                       (cdr f)
                                       (list f))))))
           (:closure
            (assert (null (cdr rest)))
            (list op (regex-simplify (car rest)))))))))


;; GNFAs from Sipser p. 70

(defun dfa->gnfa (dfa)
  (let ((new-start (gensym "START"))
        (new-accept (gensym "ACCEPT"))
        (accept (fa-accept dfa))
        (edges)
        (states (fa-states dfa))
        (hash (make-hash-table :test #'equal)))
    ;; start state
    (push (list new-start :epsilon (fa-start dfa)) edges)
    ;; index edges
    (fa-map-edges nil (lambda (q0 z q1)
                        (push z (gethash (list q0 q1) hash)))
                  dfa)
    ;; build new edges
    (do-finite-set (q0 states)
      ;; maybe accept
      (when (finite-set-inp q0 accept)
        (push (list q0 :epsilon new-accept) edges))
      ;; successor edges
      (do-finite-set (q1 states)
        ;;(print q1)
        (when-let (zz (gethash (list q0 q1) hash))
          (push (list q0 (cons :union zz) q1)
                edges))))
    ;;(print edges)
    ;; create new fa
    (make-fa edges
             new-start
             (list new-accept))))

(defun gnfa-rip (gnfa)
  (let ((start (fa-start gnfa))
        (accept))
    (assert (= 1 (finite-set-length (fa-accept gnfa))))
    (do-finite-set (q (fa-accept gnfa))
      (setq accept q))
    (do-finite-set (q (fa-states gnfa))
      (when (and (not (equal start q))
                 (not (equal accept q)))
        (return-from gnfa-rip q)))
    (error "couldn't find rip state")))

(defun gnfa->regex (gnfa)
  (cond
   ((= 2 (finite-set-length (fa-states gnfa)))
    (assert (= 1 (length (fa-edges gnfa))))
    (regex-simplify (cadar (fa-edges gnfa))))
   (t (let ((q-rip (gnfa-rip gnfa))
            (hash (make-hash-table :test #'equal))) ;; (list q0 q1) -> regex
        ;; index regexes
        (do-fa-edges (q0 z q1) gnfa
          (assert (null (gethash (list q0 q1) hash)))
          (setf (gethash (list q0 q1) hash) z))
        (let ((edges)
              (rip-closure (regex-apply :closure (gethash (list q-rip q-rip) hash))))
          (do-finite-set (q0 (fa-states gnfa))
            (unless (or (eq q0 q-rip)
                        (finite-set-inp q0 (fa-accept gnfa)))
              (do-finite-set (q1 (fa-states gnfa))
                (unless (or (eq q1 q-rip)
                            (eq q1 (fa-start gnfa)))
                  (let ((z1 (gethash (list q0 q-rip) hash)) ;; q0 -> q-rip
                        (z3 (gethash (list q-rip q1) hash)) ;; q-rip -> q1
                        (z4 (gethash (list q0 q1) hash)))   ;; q0 -> q1
                    (when-let (zz (if (and z1 z3)
                                      (regex-apply :union
                                                   z4
                                                   (regex-apply :concatenation z1 rip-closure z3))
                                      z4))
                      (push (list q0 zz q1) edges)))))))
          ;; new-fa
          (gnfa->regex (make-fa edges (fa-start gnfa) (fa-accept gnfa))))))))


(defun fa->regex (fa)
  "Convert FA to a regular expression."
  (with-dfa (dfa fa)
    ;; kludge this
    (let ((regex (gnfa->regex (dfa->gnfa dfa))))
      (if (finite-set-inp (fa-start dfa) (fa-accept dfa))
          `(:union :epsilon ,regex)
          regex))))


(defparameter *regex-lower*
  `(:union ,@(loop for c from (char-code #\a) to (char-code #\z) collect (code-char c))))

(defparameter *regex-upper*
  `(:union ,@(loop for c from (char-code #\A) to (char-code #\Z) collect (code-char c))))

(defparameter *regex-digit*
  `(:union ,@(loop for c from (char-code #\0) to (char-code #\9)
                collect (code-char c))))

(defparameter *regex-alpha*
  (regex-simplify `(:union ,*regex-lower* ,*regex-upper*)))

(defparameter *regex-alnum*
  (regex-simplify `(:union ,*regex-alpha* ,*regex-digit*)))

(defparameter *regex-blank* '(:union #\Space #\Tab))

(defun regex-sweeten (regex terminals &key
                      concatenate-strings)
  "Apply some syntactic sugar to REGEX.
Supports the following operators:
:complement : match complement
:not : any symbol except this
:+ : A A*
:? : A | epsilon
:. : match anything

REGEX: An extended regular expression.
TERMINALS: Set of all terminal symbols in the language.
"
  ;; FIXME: what if :NOT produces an empty match?
  (let ((dot (cons :union (finite-set-list terminals))))
    (labels ((rec (regex)
               (etypecase regex
                 (atom
                  (case regex
                    (:. dot)
                    (:lower-class *regex-lower*)
                    (:upper-class *regex-upper*)
                    (:digit-class *regex-digit*)
                    (:alpha-class *regex-alpha*)
                    (:alnum-class *regex-alnum*)
                    (:blank-class *regex-blank*)
                    (otherwise
                     (cond
                       ((and (stringp regex)
                             concatenate-strings)
                        (cons :concatenation (map 'list #'identity regex)))
                       (t regex)))))
                 (list
                  (destructuring-bind (op &rest rest) regex
                    (case op
                      ((:union :concatenation :closure)
                       (cons op (map 'list #'rec rest)))
                      (:+
                       (assert (null (cdr rest)))
                       (let ((a (rec (car rest))))
                         (list :concatenation a (list :closure a))))
                      (:complement
                       (assert (null (cdr rest)))
                       (fa->regex (fa-canonicalize (fa-complement (regex->dfa (rec (car rest))) terminals))))
                      (:?
                       (assert (null (cdr rest)))
                       `(:union :epsilon ,(rec (car rest))))
                      (:not
                       ;(assert (finite-set-inp (car rest) terminals))
                       (cons :union (finite-set-list (finite-set-difference (finite-set-list terminals)
                                                                            rest))))
                      (otherwise regex)))))))
      (rec regex))))
