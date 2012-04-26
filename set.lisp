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


;;;; FILE: set.lisp
;;;; BRIEF: basic operations for finite sets
;;;; AUTHOR: Neil T. Dantam
;;;;
;;;; Common Lisp includes set operations for finite sets represented
;;;; as lists, however these may become slow as sets grow very large.
;;;; Thus, we abstract the set operations to allow for easy
;;;; replacement of underlying representation with something like hash
;;;; tables, search trees, or bit vectors later.


(in-package :motion-grammar)

(defun make-finite-set (&key mutable)
  "Create a finite set.
MUTABLE: Should this be a mutable set?
         This changes the performance characteristics."
  (cond
    (mutable (make-hash-table :test #'equal))
    (t nil)))

(defun finite-set (&rest items)
  (fold #'finite-set-add (make-finite-set) items))

(defun finite-set-map (result-type function set)
  "Apply FUNCTION to all members of SET."
  (etypecase set
    (sequence (map result-type function set))
    (hash-table (maphash (lambda (k v)
                           (declare (ignore v))
                           (funcall function k))
                         set))))


(defun finite-set-map-cross (function set-1 &rest sets)
  (cond
    ((null sets) (finite-set-map nil function set-1))
    (t
     (finite-set-map nil
                     (lambda (s-1)
                       (apply #'finite-set-map-cross
                              (curry-list function s-1)
                              sets))
                     set-1))))




(defmacro do-finite-set ((var set &optional result-form) &body body)
  "Iterate over members of the set."
  (alexandria:with-gensyms (set-var)
    `(let ((,set-var ,set))
       (etypecase ,set-var
         (list (dolist (,var ,set-var ,result-form)
                 ,@body))))))

(defun finite-set-fold (function initial-value set)
  "Fold FUNCTION over SET beginning with first argument INITIAL-VALUE."
  (etypecase set
    (sequence (reduce function set :initial-value initial-value))
    (hash-table
     (let ((value initial-value))
       (maphash (lambda (k v)
                  (declare (ignore v))
                  (setq value (funcall function value k)))
                set)
       value))))

(defun finite-set-fold-range (function initial-value set)
  "Fold FUNCTION over range of SET with first argument INITIAL-VALUE."
  (etypecase set
    (hash-table
     (let ((value initial-value))
       (maphash (lambda (k v)
                  (declare (ignore k))
                  (setq value (funcall function value v)))
                set)
       value))))

(defun finite-set-filter (predicate set)
  "Return the subset of SET where PREDICATE is true.
PREDICATE: (lambda (x))
SET: a finite set
RESULT: a finite set"
  (etypecase set
    (list
     (loop for x in set
        when (funcall predicate x)
          collect x))))


(defun finite-set-length (set)
  "Return the number of elements in set."
  (etypecase set
    (sequence (length set))
    (hash-table (hash-table-count set))))

(defun finite-set-equal (a b)
  "Are sets A and B equal?"
  (cond
    ((and (listp a) (listp b))
     (and (null (set-difference a b :test #'equal))
          (null (set-difference b a :test #'equal))))
    ((hash-table-p a)
     (and (= (finite-set-length a)
             (finite-set-length b))
          (progn
            (maphash (lambda (k v)
                       (declare (ignore v))
                       (unless (finite-set-inp k b)
                         (return-from finite-set-equal nil)))
                     a)
            t)))
    ((hash-table-p b)
     (finite-set-equal b a))
    (t
     (error "Can't operate on ~A and ~B" a b))))

(defun finite-set-inp (item set)
  "Is ITEM in SET?"
  (etypecase set
      (sequence
       (find item set :test #'equal))
      (hash-table
       (multiple-value-bind (val present)
           (gethash item set)
         (declare (ignore val))
         present))))

(defun finite-set-member (set item)
  "Is ITEM a member of SET?"
  (finite-set-inp item set))

(defun finite-set-add (set item)
  "Return a new set containing ITEM and all members of SET."
  (etypecase set
    (list (if (finite-set-member set item)
              set
              (cons item set)))))

(defun finite-set-nadd (set item)
  "Destructively return a new set containing ITEM and all members of SET."
  (etypecase set
    (list (finite-set-add set item))
    (hash-table (setf (gethash item set)
                      t)
                set)))

(defun finite-set-subsetp (set-1 set-2)
  "Is set-1 a subset of set-2?"
  (cond
    ((and (listp set-1) (listp set-2))
     (subsetp set-1 set-2 :test #'equal))
    (t (error "Can't operate on ~A and ~B" set-1 set-2))))

(defun finite-set-union (set-1 set-2)
  "Return the union of set-1 and set-2."
  (cond
    ((and (listp set-1) (listp set-2))
     (union set-1 set-2 :test #'equal))
    (t (error "Can't operate on ~A and ~B" set-1 set-2))))
