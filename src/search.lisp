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

(defun fa-shortest-string (fa)
  "Generate the shortest string in the language."
  (let ((succs (fa-successors fa))
        (accept (fa-accept fa))
        (visited (make-finite-set :mutable t)))
    (if (finite-set-inp (fa-start fa) accept)
        ;; start is accept, give empty string
        nil
        ;; search it
        (labels ((visit (queue0)
                   (unless (amortized-queue-empty-p queue0)
                     (multiple-value-bind (queue element) (amortized-dequeue queue0)
                       (destructuring-bind (str . state0) element
                         (assert (not (finite-set-inp state0 accept)))
                         (dolist (succ (funcall succs state0))
                           (destructuring-bind (z state1) succ
                             (cond
                               ((finite-set-inp state1 accept)
                                (return-from fa-shortest-string (reverse (cons z str))))
                               ((not (finite-set-inp state1 visited))
                                (setq visited (finite-set-nadd visited state1)
                                      queue (amortized-enqueue queue (cons (cons z str) state1))))))))
                       queue))))
          (loop
             with queue = (amortized-queue (cons nil (fa-start fa)))
             until (amortized-queue-empty-p queue)
             do (setq queue (visit queue)))))))

(defun fa-optimal-string (fa cost-function &optional (compare #'-))
  "Generate a minimum cost string for the FA"
  (declare (type function cost-function compare))
  (let ((succs (fa-successors fa))
        (accept (fa-accept fa))
        (compare-nodes (lambda (n1 n2)
                         (funcall compare (car n1) (car n2))))
        (min-costs (make-hash-table :test #'equal)))
    (setf (gethash (fa-start fa) min-costs) 0)
    (labels ((visit (heap prev-node c q)
               (declare (type number c))
               (let ((minc (gethash q min-costs)))
                 (assert (>= c minc))
                 (if (= c minc)
                     ;; the min-cost path
                     (fold (lambda (heap zq1)
                             (destructuring-bind (z q1) zq1
                               (let ((new-c (+ c (funcall cost-function z)))
                                     (old-c (gethash q1 min-costs)))
                                 (if (and old-c (<= old-c new-c))
                                     heap
                                     (progn
                                       (setf (gethash q1 min-costs) new-c)
                                       (sycamore::pairing-heap-insert heap (cons new-c (cons prev-node zq1))
                                                                      compare-nodes))))))
                           heap (funcall succs q))
                     ;; a higher cost path, ignore it
                     heap)))
             (unwrap (node)
               (let ((s nil))
                 (loop
                    for n = node then (second n)
                    while n
                    for z = (third n)
                    do (push z s))
                 s))
             (next (heap)
               (multiple-value-bind (heap top-node) (sycamore::pairing-heap-remove-min heap compare-nodes)
                 (destructuring-bind (c prev z q) top-node
                   (declare (ignore prev z)
                            (type number c))
                   (if (finite-set-inp q accept)
                       (unwrap top-node)
                       (next (visit heap top-node c q)))))))
      (unless (finite-set-inp (fa-start fa) accept)
        (next (visit nil nil 0 (fa-start fa)))))))
