;;;; -*- Lisp -*-
;;;;
;;;; Copyright (c) 2011-2012, Georgia Tech Research Corporation
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

(in-package :motion-grammar)

(defstruct (pda (:constructor %make-pda))
  states
  input-alphabet
  stack-alphabet
  transition ;; (lambda (states input-alphabet stack-alphabet)) => (set (list states stack-alphabet))
  start
  accept)


(defun pda-edge-extended (q-0 z g-0 q-1 g-1-list)
  "Make a PDA edge."
  (list (list q-0 z g-0)
        (cons q-1 g-1-list)))

(defun pda-edge (q-0 z g-0 q-1 &rest g-1)
  "Make a PDA edge."
  (pda-edge-extended q-0 z g-0 q-1 g-1))

(defun pda-map-transtions (result-type function pda)
  "Apply FUNCTION to all transitions of PDA.
FUNCTION: (lambda ( (list q-0 z g-0) (list q-1 g-1))"
  (assert (null result-type))
  (do-finite-set (q (pda-states pda))
    (do-finite-set (sigma (pda-input-alphabet pda))
      (do-finite-set (gamma (pda-stack-alphabet pda))
        (do-finite-set (dst (funcall (pda-transition pda) q sigma gamma))
          ;(format t "edge: ~A => ~A~&" (list q sigma gamma) edge)
          (funcall function (list q sigma gamma) dst ))))))


(defun make-pda (edges start accept)
  (let ((tree-map (make-tree-map #'gsymbol-compare))
        (states (make-finite-set))
        (input-alphabet (make-finite-set))
        (stack-alphabet (make-finite-set)))
    (loop
       for edge in edges
       do
         (destructuring-bind (x y) edge
           (destructuring-bind ((q-0 sigma gamma-0) (q-1 &rest gamma-1)) edge
             ;(print edge)
             (unless (and (equal q-0 q-1)
                          (eq sigma :epsilon)
                          (eq gamma-0 :epsilon)
                          (equal gamma-1 '(:epsilon))) ;; prune silly transition
               (setq tree-map (tree-map-insert tree-map x
                                               (finite-set-add (tree-map-find tree-map x) y))
               ;(setf (gethash x hash) (finite-set-add (gethash x hash) y)
                     states (finite-set-union states (finite-set q-0 q-1))
                     input-alphabet (finite-set-add input-alphabet sigma)
                     stack-alphabet (finite-set-union stack-alphabet
                                                      (fold #'finite-set-add (make-finite-set)
                                                            (cons gamma-0 gamma-1))))))))
    (assert (finite-set-inp start states))
    (assert (finite-set-subsetp accept states))
    (%make-pda :states states
               :input-alphabet input-alphabet
               :stack-alphabet stack-alphabet
               ;:transition (lambda (q sigma gamma) (gethash (list q sigma gamma) hash))
               :transition (lambda (q sigma gamma) (tree-map-find tree-map (list q sigma gamma)))
               :start start
               :accept accept)))

(defun pda-dot (pda &key output (font-size 12))
  (let ((state-numbers (finite-set-enumerate (pda-states pda))))
    (output-dot output
                (lambda (s)
                  (format s "~&digraph {~%")
                  (format s "~&rankdir=\"LR\";~%")
                  ;; state labels
                  (format s "~:{~&  ~A[label=\"~A\",fontsize=~D];~}"
                          (finite-set-map 'list (lambda (state)
                                                  (list (funcall state-numbers state) state font-size))
                                          (pda-states pda)))
                  ;; start state
                  (when (pda-start pda)
                    (format s "~&  start[shape=none,fontsize=~D];" font-size)
                    (format s "~&  start -> ~A;"
                            (funcall state-numbers (pda-start pda))))
                  ;; accept state
                  (format s "~{~&  ~A [ shape=doublecircle ];~}"
                          (finite-set-map 'list state-numbers  (pda-accept pda)))
                  ;; transitions
                  (pda-map-transtions nil
                                      (lambda (x y)
                                        (destructuring-bind ((q-0 z g-0) (q-1 &rest g-1)) (list x y)
                                          (format s "~&  ~A -> ~A [fontsize=~D,label=\"~A, ~A&rarr;~{~A~^,~}\"];~%"
                                                  (funcall state-numbers q-0)
                                                  (funcall state-numbers q-1)
                                                  font-size
                                                  (dot-gsymbol z) (dot-gsymbol g-0)
                                                  (map 'list #'dot-gsymbol g-1))))
                                      pda)
                  (format s "~&}~%")))))

;; Sipser p116
(defun grammar->pda-sipser (grammar &key (gensym (gensym "pda")))
  (let* ((q-start (gsymbol-gen :start gensym))
         (q-accept (gsymbol-gen :accept gensym))
         (q-loop (gsymbol-gen :loop gensym))
         (edges (list (pda-edge q-start :epsilon :epsilon q-loop (grammar-start-nonterminal grammar) :$)
                      (pda-edge q-loop :epsilon :$ q-accept :epsilon))))
    ;; production edges
    (grammar-map nil
                 (lambda (head body)
                   (push (pda-edge-extended q-loop :epsilon head q-loop body)
                         edges))
                 grammar)
    ;; token edges
    (finite-set-map nil
                    (lambda (z)
                      (push (pda-edge q-loop z z q-loop :epsilon)
                            edges))
                    (grammar-terminals grammar))
    ;; construct
    (make-pda edges q-start (finite-set q-accept))))




;; Hopcroft p135
(defun pda-fa-intersection (pda fa &optional (gensym (gensym)))
  "Compute the intersection of PDA and DFA.
RESULT: a pda"
  (let ((edges nil))
    (labels ((new-state (q-pda q-fa)
               (gsymbol-gen (list q-pda q-fa)
                            gensym))
             (append-edge (q0-pda q0-fa
                                  z g0
                                  q1-pda q1-fa g1)
               (push (pda-edge-extended (new-state q0-pda q0-fa)
                                        z g0
                                        (new-state q1-pda q1-fa)
                                        g1)
                     edges)))
      (pda-map-transtions nil
                          (lambda (x y)
                            (destructuring-bind ((q0-pda z-pda g0-pda) (q1-pda &rest g1-pda)) (list x y)
                              (if (eq z-pda :epsilon)
                                  ;; add epsilon for all fa states
                                  (do-finite-set (q-fa (fa-states fa))
                                    (append-edge q0-pda q-fa :epsilon g0-pda
                                                 q1-pda q-fa g1-pda))
                                  ;; add transition for all fa edges
                                  (fa-map-edges nil
                                                (lambda (q0-fa z-fa q1-fa)
                                                  ;;(format t "~A and (~A ~A ~A)~&" x q0-fa z-fa q1-fa )
                                                  (when (equal z-pda z-fa)
                                                    (append-edge q0-pda q0-fa
                                                                 z-pda g0-pda
                                                                 q1-pda q1-fa g1-pda))
                                                  (when (equal z-fa :epsilon)
                                                    (append-edge q0-pda q0-fa
                                                                 :epsilon :epsilon
                                                                 q0-pda q1-fa :epsilon)))
                                                fa))))
                          pda)

      (let* ((states-i (fold (lambda (set edge)
                              (destructuring-bind ((q0 z g0 ) (q1 &rest g1)) edge
                                (declare (ignore z g0 g1))
                                (finite-set-add (finite-set-add set q0) q1)))
                             (make-finite-set)
                             edges))
             (accept-i (finite-set-intersection states-i
                                                (apply #'finite-set
                                                       (map-finite-set-product 'list
                                                                               #'new-state
                                                                               (pda-accept pda)
                                                                               (fa-accept fa)))))

             (start-i (new-state (pda-start pda)
                                 (fa-start fa))))
          ;;(format t "states: ~A~&~&start: ~A~&accept: ~A~&" states-i start-i accept-i)
        (make-pda edges start-i accept-i)))))
