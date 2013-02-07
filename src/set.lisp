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


(in-package :motion-grammar-kit)

(deftype finite-set () '(or list hash-table tree-set))

(defun make-finite-set (&key mutable compare)
  "Create a finite set.
MUTABLE: Should this be a mutable set?
         This changes the performance characteristics."
  (cond
    (mutable (make-hash-table :test #'equal))
    (compare (make-tree-set compare))
    (t nil)))

(defun finite-set (&rest items)
  (fold #'finite-set-add (make-finite-set) items))

(defun map-finite-set (result-type function set)
  "Apply FUNCTION to all members of SET."
  (etypecase set
    (sequence (map result-type function set))
    (tree-set (map-tree-set result-type function set))
    (hash-table (cond
                  ((null result-type)
                   (maphash (lambda (k v)
                              (declare (ignore v))
                              (funcall function k))
                            set))
                  ((eq 'list result-type)
                   (loop for k being the hash-keys of set
                        collect (funcall function k)))
                  (t (assert nil))))))

(defun finite-set-map (result-type function set)
  (map-finite-set result-type function set))

(defun fold-finite-set (function initial-value set)
  "Fold FUNCTION over SET beginning with first argument INITIAL-VALUE."
  (etypecase set
    (sequence (reduce function set :initial-value initial-value))
    (tree-set (fold-tree-set function initial-value set))
    (hash-table
     (loop for k being the hash-keys of set
        for value = (funcall function initial-value k) then (funcall function value k)
        finally (return value)))))

(defun sort-finite-set (set)
  (etypecase set
    (tree-set set)
    (list (gsymbol-sort set))))

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
  (alexandria:with-gensyms (set-var fun)
    `(let ((,set-var ,set))
       (labels ((,fun (,var) ,@body))
         (etypecase ,set-var
           (tree-set (map-tree-set nil #',fun ,set))
           (list (dolist (,var ,set-var ,result-form)
                   (,fun ,var)))
           (hash-table
            (loop for ,var being the hash-keys of ,set
               do (,fun ,var))))))))

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
    (tree-set (tree-set-count set))
    (hash-table (hash-table-count set))))

(defun finite-set-min-set (set1 set2)
  (if (< (finite-set-length set1)
         (finite-set-length set2))
      set1
      set2))

(defun finite-set-equal (a b)
  "Are sets A and B equal?"
  (cond
    ((and (listp a) (listp b))
     (and (null (set-difference a b :test #'equal))
          (null (set-difference b a :test #'equal))))
    ((and (tree-set-p a) (tree-set-p b))
     (tree-set-equal-p a b))
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

(defun finite-set-list (set)
  (etypecase set
    (list set)
    (tree-set (map-tree-set 'list #'identity set))
    (hash-table (loop for k being the hash-keys of set collect k))))

(defun finite-set-tree (set)
  (etypecase set
    (list (fold #'tree-set-insert (make-tree-set #'gsymbol-compare) set))
    (tree-set set)
    (hash-table
     (loop for k being the hash-keys of set
        for tree = (tree-set #'gsymbol-compare k) then (tree-set-insert tree k)
        finally (return tree)))))


(defun finite-set-inp (item set)
  "Is ITEM in SET?"
  (etypecase set
      (sequence
       (find item set :test #'equal))
      (tree-set (tree-set-member-p set item))
      (hash-table
       (multiple-value-bind (val present)
           (gethash item set)
         (declare (ignore val))
         present))))

(defun finite-set-empty-p (set)
  (etypecase set
    (null t)
    (list set)
    (tree-set (zerop (tree-set-count set)))
    (hash-table (zerop (hash-table-count set)))))

(defun finite-set-single-p (set)
  (etypecase set
    (tree-set (= 1 (the fixnum (tree-set-count set))))
    (list (and set
               (null (cdr set))))
    (hash-table (= 1 (hash-table-count set)))))

(defun finite-set-member (set item)
  "Is ITEM a member of SET?"
  (finite-set-inp item set))

(defun finite-set-subsetp (set-1 set-2)
  "Is set-1 a subset of set-2?"
  (cond
    ((and (listp set-1) (listp set-2))
     (subsetp set-1 set-2 :test #'equal))
    ((and (tree-set-p set-1)
          (tree-set-p set-2))
     (tree-set-subset-p set-1 set-2))
    (t (error "Can't subset on ~A and ~B" set-1 set-2))))

(defun finite-set-union (set-1 set-2)
  "Return the union of set-1 and set-2."
  (cond
    ((null set-1) set-2)
    ((null set-2) set-1)
    ((and (listp set-1) (listp set-2))
     (union set-1 set-2 :test #'equal))
    ((and (tree-set-p set-1)
          (tree-set-p set-2))
     (tree-set-union set-1 set-2))
    (t (error "Can't union on ~A and ~B" set-1 set-2))))

(defun finite-set-intersection (set-1 set-2)
  (cond
    ((and (listp set-1) (listp set-2))
     (intersection set-1 set-2 :test #'equal))
    ((and (tree-set-p set-1)
          (tree-set-p set-2))
     (tree-set-intersection set-1 set-2))
    (t (error "Can't intersect on ~A and ~B" set-1 set-2))))

(defun finite-set-difference (set-1 set-2)
  "Return the difference of set-1 and set-2."
  (cond
    ((and (listp set-1) (listp set-2))
     (set-difference set-1 set-2 :test #'equal))
    ((and (tree-set-p set-1)
          (tree-set-p set-2))
     (tree-set-difference set-1 set-2))
    ((null set-2) set-1)
    (t (error "Can't difference on ~A and ~B" set-1 set-2))))

(defun finite-set-add (set item)
  "Return a new set containing ITEM and all members of SET."
  (etypecase set
    (list (finite-set-union set (list item)))
    (tree-set (tree-set-insert set item))))

(defun finite-set-nadd (set item)
  "Destructively return a new set containing ITEM and all members of SET."
  (etypecase set
    (list (finite-set-add set item))
    (tree-set (tree-set-insert set item))
    (hash-table (setf (gethash item set)
                      t)
                set)))

(defun finite-set-remove (set item)
  "Return a new set  all members of SET except item."
  (etypecase set
    (list (finite-set-difference set (list item)))
    (tree-set (tree-set-remove set item))))

(defun finite-set-enumerate (set)
  "Return a function mapping from members of set to integers.
RESULT: (lambda (item)) => integer"
  (let ((hash (make-hash-table :test #'equal))
        (i -1))
    (do-finite-set (x set)
      (setf (gethash x hash) (incf i)))
    (curry-right #'gethash hash)))

(defun map-finite-set-product (result-type function set-1 &rest more-sets)
  "Apply function to all products of set arguments.
RESULT-TYPE: (or nil 'list)"
  (cond
    ((eq result-type 'list)
     (let ((result))
       (apply #'map-finite-set-product nil
              (lambda (&rest args)
                (push (apply function args) result))
              set-1
              more-sets)
       result))
    (result-type (error "Unknown result-type ~A" result-type))
    (more-sets
     (do-finite-set (x-1 set-1)
       (apply #'map-finite-set-product nil
              (curry-list function x-1)
              more-sets)))
    (t
     (do-finite-set (x-1 set-1)
       (funcall function x-1)))))

;; (defun map-finite-set-product (result-type function set-1 set-2)
;;   (let ((result))
;;     (do-finite-set (x-1 set-1)
;;       (do-finite-set (x-2 set-2)
;;         (let ((y (funcall function x-1 x-2)))
;;           (when (eq 'list result-type)
;;             (push y result)))))
;;     result))

(defun index-finite-set (set key-function value-function &key
                         (duplicate-type 'tree-set)
                         (test #'equal))
  "Index members of a finite set.
KEY-FUNCTION: Produce the index key from a set member.
VALUE-FUNCTION: Produce the value to be indexed from the set member.
DUPLICATE-TYPE: (or nil list tree-set), Don't allow duplicates,
             or store as a list or store as a tree-set
RESULT: (values (lambda (key)) => (values (list set-values...) (or t nil))
                (finite-set keys...))"
  (let ((hash (make-hash-table :test test)))
    (flet ((helper (function)
             (do-finite-set (x set)
               (let ((key (funcall key-function x)))
                 (setf (gethash key hash)
                       (funcall function (funcall value-function x)
                                (gethash key hash) ))))))
      (if duplicate-type
          (ecase duplicate-type
            ((:tree-set tree-set)
             (let ((empty-set (make-finite-set :compare #'gsymbol-compare)))
               (helper (lambda (value old)
                         (finite-set-add (or old empty-set)
                                         value)))))
            ((:list list)
             (helper (lambda (value old)
                       (cons value old)))))
          ;; no duplicates
          (helper (lambda (value old)
                    (assert (null old))
                    value)))
      (values (lambda (key) (gethash key hash))
              hash))))
