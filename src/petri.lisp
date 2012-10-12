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

;;; Utils

(in-package :motion-grammar-kit)


;;; Petri Net
;;; - places
;;; - transitions
;;; - arcs (list {p,t,nil} {p,t,nil})
;;; - marking (list (place . count)), no entry -> 0

(defstruct petri-net
  "A Petri Net"
  places
  transitions
  arcs
  marking)

(defun petri-net-index-transitions (pn)
  "RETURNS -- (lambda (transition)) => (values subtract-places add-places)"
  (let ((hash-subtract (make-hash-table :test #'equal))
        (hash-add (make-hash-table :test #'equal))
        (places (petri-net-places pn))
        (transitions (petri-net-transitions pn)))
    (loop for (a b) in (petri-net-arcs pn)
       when (and a b)
       do (cond
            ((finite-set-inp a places)
             (assert (finite-set-inp b transitions))
             (push a (gethash b hash-subtract nil)))
            ((finite-set-inp b places)
             (assert (finite-set-inp a transitions))
             (push b (gethash a hash-add nil)))
            (t (error "Bad arc ~A"  (list a b)))))
    (lambda (a)
      (values (gethash a hash-subtract nil)
              (gethash a hash-add nil)))))

;; wrong
;; (defun petri-net-bounded-p (pn)
;;   (let ((feeder-arcs (loop for arc in (petri-net-arcs pn)
;;                         for (a b) = arc
;;                         when (and (null a)
;;                                   (finite-set-inp b (petri-net-transitions pn)))
;;                         collect arc)))
;;     (if (null feeder-arcs)
;;         t
;;         (error "Unhandled"))))

(defun petri-net-remark (pn marking)
  "Create new petri-new like PN with markings MARKING."
  (make-petri-net :places (petri-net-places pn)
                  :transitions (petri-net-transitions pn)
                  :arcs (petri-net-arcs pn)
                  :marking marking))


(defun petri-net-explicit-marking (pn &optional (marking (petri-net-marking pn)))
  "Return marking list of PN with values for all places explicitly listed."
  (let* ((pre-marked (loop for (place . mark) in marking collect place))
         (implicit-marked (finite-set-difference (petri-net-places pn) pre-marked)))
    (append (loop for p in implicit-marked
                 collect (cons p 0))
            marking)))

(defun petri-net-fire (pn transition &key
                       (marking (petri-net-marking pn))
                       (transition-index (petri-net-index-transitions pn))
                       (unreachable-error t)
                       (unreachable-value nil))
  "Return new-marking for petri net PN after one firing of TRANSITION."
  (multiple-value-bind (sub add) (funcall transition-index transition)
    (let ((new-mark (loop for mark in marking
                       for (place . count) = mark
                       collect (cond ((finite-set-inp place sub)
                                      (when (<= count 0)
                                        (if unreachable-error
                                            (error "Can't reduce ~A" place)
                                            (return-from petri-net-fire unreachable-value)))
                                      (setq sub (finite-set-remove sub place))
                                      (cons place (1- count)))
                                     ((finite-set-inp place add)
                                      (setq add (finite-set-remove add place))
                                      (cons place (1+ count)))
                                     (t mark)))))
      (if sub
          (if unreachable-error
              (error "Can't reduce ~A" sub)
              (return-from petri-net-fire unreachable-value)))
      (petri-net-explicit-marking pn (append (loop for place in add collect (cons place 1)) new-mark)))))

(defun petri-net->fa (pn)
  "Converted a (bounded) petri-net PN to a finite automaton."
  (let ((transition-index (petri-net-index-transitions pn))
        (mark-0 (gsymbol-sort (petri-net-explicit-marking pn)))
        (hash (make-hash-table :test #'equal))
        (edges nil))
    (labels ((visit (mark)
               (unless (gethash mark hash nil)
                 (setf (gethash mark hash) t)
                 (do-finite-set (trans (petri-net-transitions pn))
                   (let ((new-mark (gsymbol-sort (petri-net-fire pn trans
                                                                 :marking mark
                                                                 :transition-index transition-index
                                                                 :unreachable-error nil
                                                                 :unreachable-value nil))))
                     (when new-mark
                       (push (list mark trans new-mark) edges)
                       (visit new-mark)))))))
      (visit mark-0)
      (make-fa edges mark-0
               (loop for k being the hash-keys of hash collect k)))))


;;;;;;;;;;;;;;
;;; OUTPUT ;;;
;;;;;;;;;;;;;;

(defun petri-net-dot (pn &key
                      output)
  "Generate Graphviz output for petri-net PN."
  (output-dot output
              (lambda (s)
                (format s "~&digraph {~%")
                ;; places
                (do-finite-set (p (petri-net-places pn))
                  (format s "~&  ~A[shape=circle,label=\"~A (~A)\"];~%"
                          p p
                          (if (assoc p (petri-net-marking pn))
                              (cdr (assoc p (petri-net-marking pn)))
                              0)))
                ;; transitions
                (do-finite-set (tr (petri-net-transitions pn))
                  (format s "~&  ~A[shape=box,style=filled,fillcolor=black,fontcolor=white,height=.25,width=1];~%" tr))
                ;; arcs
                (loop for (a b) in (petri-net-arcs pn)
                   do
                     (format s "~&  ~A -> ~A;~%" a b))
                (format s "~&}~%"))))
