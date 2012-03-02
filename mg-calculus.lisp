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



(defun transform-split-region (grammar original-token token-1 token-2)
  "Split ORIGINAL-TOKEN into TOKEN-1 and TOKEN-2."
  (when grammar
    (destructuring-bind ( (a &rest rhs) &rest remaining ) grammar
      (let ((i (position original-token rhs :test #'equal)))
        (if i
            (let ((alpha-1 (when (> i 0) (subseq rhs 0 i)))
                  (alpha-2 (when (< i (length rhs)) (nthcdr (1+ i) rhs)))
                  (a-0 (gensym (gsymbol-name a)))
                  (a-1 (gensym (gsymbol-name a)))
                  (a-2 (gensym (gsymbol-name a)))
                  (a-3 (gensym (gsymbol-name a)))
                  (a-4 (gensym (gsymbol-name a))))
              (transform-split-region
               `(;; A -> alpha_1 A-0
                 (,a ,@alpha-1 ,a-0)
                 ;; A-0 -> A-1 | A-2
                 ,(list a-0 a-1)
                 ,(list a-0 a-2)
                 ;; A-1 -> zeta-1 A-3
                 ,(list a-1 token-1 a-3)
                 ;; A-2 -> zeta-2 A-4
                 ,(list a-2 token-2 a-4)
                 ;; A-3 -> A-2 | alpha-2
                 ,(list a-3 a-2)
                 (,a-3 ,@alpha-2)
                 ;; A-4 -> A-1 | alpha-2
                 ,(list a-4 a-1)
                 (,a-4 ,@alpha-2)
                 ,@remaining)
               original-token token-1 token-2))
            ;; else
            (cons (car grammar)
                  (transform-split-region remaining
                                          original-token token-1 token-2)))))))

(defun transform-prune-adjacent (grammar token non-adjacent)
  (let ((first-set (grammar-first-function grammar)))
    (labels ((visit (grammar)
               (when grammar
                 (destructuring-bind ( (head &rest body) &rest remaining ) grammar
                   (let ((i (position original-token rhs :test #'equal)))
                     (if i
                         (let* ((alpha-1 (when (> i 0) (subseq rhs 0 i)))
                                (alpha-2 (when (< i (length rhs))
                                       (nthcdr (1+ i) rhs)))
                                (first-alpha-2 (funcall first-set (car alpha-2))))
                           (cond
                             ;; match, single expansion, remove
                             ((and alpha-2
                                   (finite-set-inp non-adjacent first-alpha-2)
                                   (finite-set-length 1 first-alpha-2))
                              (visit remaining))
                             ;; replace prod with pruned
                             ((and alpha-2
                                   (finite-set-inp non-adjacent first-alpha-2))
                              (let ((new-sym (gensym "pruned")))
                                (visit `((,head ,@alpha-1 ,token ,new-sym)
                                         ,@(apply #'nconc
                                                  (finite-set-map
                                                   'list
                                                   (lambda (body)
                                                     (unless (and (finite-set-length
                                                           ))))))
                                         ;; FIXME: HERE....
                             ;; no match, keep prod
                             (t (cons (car grammar) (visit (cdr grammar))))))
                         (cons (car grammar) (visit (cdr grammar)))))))))
      (visit grammar))))


