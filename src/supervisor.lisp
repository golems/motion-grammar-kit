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


(defun dfa-transition-vector (dfa)
  "Produce the transition matrix for dfa as a simple vector.

In the row major view, rows represent initial state and columns
represent the terminal.  The value of each element is the successive
state.  A value of -1 is an invalid transition."
  (let* ((mover (dfa-mover dfa))
         (terminals (finite-set-list (finite-set-tree (fa-terminals dfa))))
         (states (finite-set-list (finite-set-tree (fa-states dfa))))
         (state-hash (make-hash-table :test #'equal))
         (n-states (length states))
         (n-terminals (length terminals))
         (array (make-array (* n-states n-terminals) :element-type 'fixnum)))
    ;; index symbols
    (loop for i from 0
       for q in states
       do (setf (gethash q state-hash ) i))
    ;; row major matrix
    ;; row index is state, col index is terminals
    (loop for i from 0
       for q0 in states
       do (loop
             for j from 0
             for z in terminals
             for q1 = (funcall mover q0 z)
             do (setf (aref array (+ (* i n-terminals) j))
                      (if q1
                          (gethash q1 state-hash)
                          -1))))
    array))

;; TODO: Better to cffi-grovel this somehow
;;       Need to set proper include path
(cffi:defcstruct supervisor-data
  (n-states :uint64)
  (n-terminals :uint64)
  (bits :uint8)
  (table :uint8 :offset 32 :count 1))

(defmacro with-supervisor-cstruct ((pointer-var size-var) fa
                                &body body)
  (alexandria:with-gensyms (fa-var states terminals n-states n-terminals bits bytes
                                   vector i)
    `(let* ((,fa-var ,fa)
            (,states (fa-states ,fa))
            (,terminals (fa-terminals ,fa))
            (,n-states (finite-set-length ,states))
            (,n-terminals (finite-set-length ,terminals))
            (,bits 32)
            (,bytes (/ ,bits 8)))
       (assert (< ,n-states (expt 2 ,bits))) ;; assume 32 bit for now
       (cffi:with-foreign-pointer (,pointer-var
                                   (+ 32 (* ,bytes ,n-states ,n-terminals))
                                   ,size-var)
         (setf (cffi:foreign-slot-value ,pointer-var 'supervisor-data 'n-states)
               ,n-states
               (cffi:foreign-slot-value ,pointer-var 'supervisor-data 'n-terminals)
               ,n-terminals
               (cffi:foreign-slot-value ,pointer-var 'supervisor-data 'bits) ,bits)
         (let ((,vector (dfa-transition-vector ,fa-var)))
           (assert (= (length ,vector) (* ,n-states ,n-terminals)))
           (dotimes (,i (length ,vector))
             (setf (cffi:mem-aref (cffi:foreign-slot-pointer ,pointer-var
                                                             'supervisor-data 'table)
                                  :int32 ,i)
                   (aref ,vector ,i))))
         ,@body))))

(defun supervisor-table-ach-put (fa channel)
  (with-supervisor-cstruct (pointer size) fa
    (ach:put-pointer channel pointer size)))

(defun supervisor-table-buffer (fa)
  (with-supervisor-cstruct (pointer size) fa
    (let ((buffer (make-array size :element-type '(unsigned-byte 8))))
      (dotimes (i size)
      (setf (aref buffer i)
            (cffi:mem-aref pointer :uint8 i)))
      buffer)))

(defun supervisor-table-output (fa pathname)
  (with-open-file (s pathname :direction :output
                     :if-exists :supersede :if-does-not-exist :create
                     :element-type '(unsigned-byte 8))
    (write-sequence (supervisor-table-buffer fa) s))
  nil)
