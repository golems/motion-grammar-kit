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
  places
  transitions
  arcs
  marking)

;;;;;;;;;;;;;;
;;; OUTPUT ;;;
;;;;;;;;;;;;;;

(defun petri-dot (pn &key
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
                   do (format s "~&  ~A -> ~A;~%" a b))
                (format s "~&}~%"))))
