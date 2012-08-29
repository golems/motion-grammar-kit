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
  transition ;; (tree-map (states input-alphabet stack-alphabet)) => (set (list states stack-alphabet))
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
  (map-tree-map :inorder result-type
                (lambda (x yy)
                  (finite-set-map nil (lambda (y) (funcall function x y)) yy))
                (pda-transition pda)))

(defmacro pda-destructure-transitions (((q0 z g0) (q1 g1)) pda &body body)
  (alexandria:with-gensyms (x y yy)
    `(map-tree-map :inorder nil
                   (lambda (,x ,yy)
                     (do-finite-set (,y ,yy)
                       (destructuring-bind ((,q0 ,z ,g0) (,q1 &rest ,g1)) (list ,x ,y)
                         ,@body)))
                   (pda-transition ,pda))))

(defun pda-index-successor-state-stack (pda)
  (let ((hash (make-hash-table :test #'equal)))
    (pda-map-transtions nil (lambda (x y)
                              (destructuring-bind (q0 z g0) x
                                (declare (ignore z))
                                (let ((key (list q0 g0)))
                                  (setf (gethash key hash)
                                        (finite-set-union (gethash key hash nil)
                                                          (finite-set y))))))
                        pda)
    (lambda (q g) (let ((key (list q g)))
               (declare (dynamic-extent key))
               (gethash key hash)))))

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
                          (epsilon-p sigma) (epsilon-p gamma-0) (epsilon-p gamma-1)) ;; prune silly transition
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
               ;:transition (lambda (q sigma gamma) (tree-map-find tree-map (list q sigma gamma)))
               :transition tree-map
               :start start
               :accept accept)))

(defun pda-normalize (pda)
  "All states pushes and pops at most one symbol."
  (let ((edges))
    (pda-map-transtions
     nil
     (lambda (x y)
       (destructuring-bind ((q-0 sigma gamma-0) (q-1 &rest gamma-1)) (list x y)
         (labels ((new-state (i)
                    (list x y i))
                  (push-state (i q0 z g0 gg1)
                    (cond
                      ;; stack pop and recurse
                      ((and (not (epsilon-p g0))
                            (not (epsilon-p gg1)))
                       (let ((q1 (new-state i)))
                         (push (pda-edge q0 z g0 q1 :epsilon) edges)
                         (push-state (1+ i) q1 :epsilon :epsilon gg1)))
                      ;; stack pop and done
                      ((and (not (epsilon-p g0))
                            (epsilon-p gg1))
                       (push (pda-edge q0 z g0 q-1 :epsilon) edges))
                      ;; epsilon push
                      ((epsilon-p gg1)
                       (push (pda-edge q0 z g0 q-1 :epsilon) edges))
                      ;; epsilon tail push
                      ((epsilon-p (cdr gg1))
                       (push (pda-edge q0 z g0 q-1 (car gg1)) edges))
                      ;; normal
                      (t
                       (let ((q1 (new-state i)))
                         (push (pda-edge q0 z g0 q1 (car gg1)) edges)
                         (push-state (1+ i) q1 :epsilon :epsilon (cdr gg1)))))))
           (push-state 0 q-0 sigma gamma-0 (reverse gamma-1)))))
     pda)
    (make-pda edges (pda-start pda) (pda-accept pda))))



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
                              (if (epsilon-p z-pda)
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
                                                  (when (epsilon-p z-fa)
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





;; A. Finkel, B. Willems, and P. Wolper. A direct symbolic approach to model
;; checking pushdown systems. Electronic Notes in Theoretical Computer Science, 9,
;; 1997.


(defun pda-reachability-automaton (pda)
  (let ((epsilon-hash (make-hash-table :test #'equal))
        (edges)) ;; TODO: could do better than a dumb list
    ;; init epsilon hash
    (let ((empty-set (make-finite-set :compare #'gsymbol-compare)))
      (do-finite-set (q (pda-states pda))
        (setf (gethash q epsilon-hash) (finite-set-add empty-set q))))
    (labels ((epsilon-closure (q)
               (gethash q epsilon-hash))
             (new-edge (q0 g q1)
               ;; update epsilon closure
               (when (eq :epsilon g)
                 (setf (gethash q0 epsilon-hash)
                       (finite-set-union (epsilon-closure q0)
                                         (epsilon-closure q1))))
               ;; add edge
               ;(print (list q0 g q1))
               (let ((e (list q0 g q1)))
                 ;(print (not (finite-set-inp e edges)))
                 (prog1 (not (finite-set-inp e edges))
                   (setq edges (finite-set-add edges e)))))
             )
      ;; add initial transitions
      (pda-destructure-transitions ((q0 z g0) (q1 g1)) pda
        (declare (ignore z))
        (destructuring-bind (g1) g1
          (when (epsilon-p g0) ;; pop nothing
          ;  (print (list :push q0 g1 q1))
            (new-edge q0 g1 q1))))
      ;; saturate
      (do ((modified t))
          ((not modified))
        ;(print 'loop)
        (setq modified nil)
        (pda-destructure-transitions ((q z g0) (qp g1)) pda
          (declare (ignore z))
          (unless (epsilon-p g0)  ;; pop g0
            (assert (epsilon-p g1))
            (loop for (eqpp eg eq) in edges
               when (and (equal g0 eg) ;; push g0
                         (finite-set-inp q (epsilon-closure eq))) ;; eq -> q on :epsilon (get from pushed to popped)
               do
                 ;(print (list :pop eg g0 q0 eq1))
                 (setq modified (or modified (new-edge eqpp :epsilon qp))))))
        ;(print `(modified ,modified))
        ))
    ;; make FA
    (%make-fa :states (pda-states pda)
              :edges edges
              :terminals (pda-stack-alphabet pda)
              :start (pda-start pda)
              :accept (pda-accept pda))))





;;

;; (defun pda-accepting-p (pda)
;;   "Test if PDA is empty."
;;   (let ((hash (make-hash-table :test #'equal))
;;         (empty-fa (make-epsilon-fa (pda-stack-alphabet pda)))
;;         (accept (pda-accept pda))
;;         (succ (pda-index-successor-state-stack pda))) ;; state => stack langauge FA
;;     (labels ((state-fa (q) (gethash q hash empty-fa))
;;              (visit (q)
;;                (print q)
;;                (print (state-fa q))
;;                (if (finite-set-inp q accept)
;;                    (return-from pda-accepting-p q)
;;                    (let* ((old-fa (state-fa q))
;;                           (stack-top (fa-initial-terminals old-fa)))
;;                      (do-finite-set (g (finite-set-add stack-top :epsilon))
;;                        (let ((pop-fa (if (eq :epsilon g) old-fa
;;                                          (fa-canonicalize (fa-pop-initial old-fa g)))))
;;                          (print (list 'pop pop-fa))
;;                          (do-finite-set (y (funcall succ q g))
;;                            (destructuring-bind  (q1 &rest g1) y
;;                              (let* ((succ-fa (state-fa q1))
;;                                     (new-fa (fa-canonicalize (fa-concatenate (regex->dfa `(:concatenation ,@g1))
;;                                                                              pop-fa))))
;;                                (print (list 'new-fa y pop-fa))
;;                                (unless (dfa-eq new-fa succ-fa)
;;                                  (setf (gethash q1 hash)
;;                                        (fa-canonicalize (fa-union new-fa succ-fa)))
;;                                  (visit q1)))))))))
;;                nil))
;;       (visit (pda-start pda)))))
