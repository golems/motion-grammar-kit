;;;; -*- Lisp -*-
;;;;
;;;; Copyright (c) 2013, Georgia Tech Research Corporation
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


;;; Logic grammar
;;; -------------
;;; E := term
;;;    | (and E E)
;;;    | (or E E)
;;;    | (not E)
;;;    | (implies E)
;;;    | (iff E E)

(defun prop-simplify (e &optional (use-minisat nil))
  (labels ((recurse (e) (prop-simplify e use-minisat))
           (same-as (e1 e2) (when use-minisat (propositions-equivalent e1 e2)))
           (never-as (e1 e2) (same-as e1 (list 'not e2)))
           (help-simplify (e)
             (pattern-case e
               ;; ;; simple equivalence
               ((:pattern (or 'and 'or) a a)
                (recurse a))
               ;; operator removal
               ((or (:pattern 'and a (eq t))
                    (:pattern 'and (eq t) a)
                    (:pattern 'or nil a)
                    (:pattern 'or a nil)
                    (:pattern 'and a (:predicate same-as a))
                    (:pattern 'or a (:predicate same-as a))
                    )
                (recurse a))
               ((or (:pattern 'and t nil)
                    (:pattern 'and nil t)
                    )
                nil)
               ((:pattern 'and a (:predicate never-as a))
                nil)

               ((or (:pattern 'or t (eq t))
                    (:pattern 'or (eq t) t)
                    )
                t)
               ((:pattern 'or a (:predicate never-as a))
                t)
               ;; de morgan (and --> or)
               ((:pattern 'not (:pattern 'and (:pattern 'not a) (:pattern 'not b)))
                (list 'or a b))
               ;; de morgan (or --> and)
               ((:pattern 'not (:pattern 'or (:pattern 'not a) (:pattern 'not b)))
                (list 'and a b))
               ;; double negation
               ((:pattern 'not (:pattern 'not a))
                (recurse a))
               ;; implication elimination
               ((:pattern 'implies a b)
                (recurse `(or (not ,a) ,b)))
               ;; biconditional elimination
               ((:pattern 'iff a b)
                (recurse `(and (implies ,a ,b) (implies ,b ,a))))
               ;; basic formula, recurse
               ((atom) e)
               ((:pattern (or 'and 'or 'implies 'iff) a b)
                 (list (car e) (recurse a) (recurse b)))
               ((:pattern 'not a)
                (list (car e) (recurse a)))
               (t (error "Invalid proposition: ~A" e)))))
    (let ((e-new (help-simplify e)))
      (if (gsymbol-equal e e-new)
        e
        (recurse e-new)))))

(defun logic-variables (e)
  (labels ((helper (v e)
             (pattern-case e
               ((:pattern (or 'and 'or 'iff 'implies) &rest args)
                (fold #'helper v args))
               ((:pattern 'not a)
                (helper v a))
               ((atom)
                (finite-set-nadd v e))
               (t (error "Invalid proposition: ~A" e)))))
    (helper (make-finite-set :mutable t) e)))


(defun prop->se (e)
  "Returns (values (lambda (integer)) variables).
The lambda will evalute E, assigning each bit of INTEGER to the corresponding variables."
  (let* ((e (prop-simplify e))
         (v (finite-set-list (logic-variables e))))
    (values
     (with-gensyms (integer)
       `(lambda (,integer)
          (declare (type (unsigned-byte ,(length v)) ,integer))
          (let ,(loop for i from 0
                   for x in v
                   collect `(,x (not (zerop (ldb (byte 1 ,i) ,integer)))))
            (declare (type boolean ,@v))
            ,e)))
     v)))

(defun prop-compile (e)
  (multiple-value-bind (se v) (prop->se e)
    (values (eval se) v)))

(defun circuit-sat-brute (e)
  "Brute-force solution to circuit-sat"
  (multiple-value-bind (fun v)
      (prop-compile e)
    (loop with max = (expt 2 (length v))
       for i from 0 below max
       for sat = (funcall fun i)
       when sat
       do (return-from circuit-sat-brute (values i v)))
    (values 0 v)))

(defun prop-join (e)
  (labels ((join (op args)
             "Returns list of joined args"
             (loop for arg in args
                append (if (and (consp arg) (eq op (car arg)))
                           (join op (cdr arg))
                           (list (prop-join arg))))))
    (if (consp e)
        (cons (car e) (join (car e) (cdr e)))
        e)))

(defun prop->cnf (e)
  "Convert E to Conjuntive-Normal-Form.
A conjunction of disjunctions of literals."
  (labels ((rewrite (op &rest args)
             (values (visit (if args
                                (cons op (mapcar #'visit args))
                                op))
                     t))
           (pass (op)
             (values op nil))
           (recurse (op args)
             (let* ((rewritten)
                    (terms (loop
                              for a in args
                              collect (multiple-value-bind (term r)
                                          (visit a)
                                        (setq rewritten (or rewritten r))
                                        term)))
                    (e (cons op terms)))
               (if rewritten
                   (visit e)
                   (values e nil))))
           (visit (e)
             (pattern-case e
               ;; operator removal
               ((or (:pattern (or 'and 'or) a a)
                    (:pattern (or 'and 'or) a)
                    ;; and t
                    (:pattern 'and a (eq t))
                    (:pattern 'and (eq t) a)
                    ;; or nil
                    (:pattern 'or nil a)
                    (:pattern 'or a nil)
                    ;; double negation
                    (:pattern 'not (:pattern 'not a)))
                (rewrite a))
               ;; and nil / or t
               ((or (:pattern 'and t nil)
                    (:pattern 'and nil t))
                (rewrite nil))
               ((or (:pattern 'or t (eq t))
                    (:pattern 'or (eq t) t))
                (rewrite t))
               ;; implication elimination
               ((:pattern 'implies a b)
                (rewrite 'or `(not ,a) b))
               ;; biconditional elimination
               ((:pattern 'iff a b)
                (rewrite 'and `(implies ,a ,b) `(implies ,b ,a)))
               ;; move not inwards
               ((:pattern 'not (:pattern 'and a b))
                (rewrite 'or `(not ,a) `(not ,b)))
               ((:pattern 'not (:pattern 'or a b))
                (rewrite 'and `(not ,a) `(not ,b)))
               ;; distribute or over and
               ((or (:pattern 'or a (:pattern 'and b c))
                    (:pattern 'or (:pattern 'and b c) a))
                (rewrite 'and `(or ,a ,b) `(or ,a ,c)))
               ;; basic formula, recurse
               ((atom) (pass e))
               ((or (:pattern (or 'and 'or)
                              t t)
                    (:pattern 'not t))
                (recurse (car e) (cdr e)))
               (t (error "Invalid proposition: ~A" e))))
           (wrap (op e)
             (if (or (not (consp e)) (not (eq (car e) op)))
                 (list op e)
                 e))
           (fixup (e)
             (cond
               ((or (atom e)
                    (eq (car e) 'not))
                (fixup `(or ,e)))
               ((eq (car e) 'or)
                `(and ,e))
               (t (fixup-ands e))))
           (fixup-ands (e)
             (if-pattern (:pattern 'and a b) e
                         (list 'and (fixup-ands a) (fixup-ands b))
                         (wrap 'or e))))
    (fixup (visit e))))

(defun prop->dimacs (e &optional (stream *standard-output*))
  "Convert E to dimacs format and return a list of the variables"
  (let* ((e (prop-join (prop->cnf e)))
         (v (finite-set-list (logic-variables e)))
         (hash (make-hash-table :test #'equal)))
    ;; enumaerate vars
    (loop for i from 1
       for x in v
       for s = (prin1-to-string i)
       do
         (setf (gethash x hash) s
               (gethash `(not ,x) hash)  (concatenate 'string "-" s)))
    ;; header
    (format stream "~&p cnf ~D ~D~&"
            (length v) (length (cdr e)))
    (format stream "~&c variables ~{~A~^, ~}~&" v)
    ;; clauses
    (assert (eq (car e) 'and))
    (dolist (x (cdr e))
      (assert (eq (car x) 'or))
      (format stream "~&~{~A ~}0~&" (loop for y in (cdr x)
                                       collect (gethash y hash))))
    ;; result
    v))

#+sbcl
(defun minisat (e &key
                (dimacs-pathname (format nil "/tmp/dimacs.~D" (sb-posix:getuid)))
                (minisat-result-pathname (format nil "/tmp/minisat.~D" (sb-posix:getuid)))
                (keep-files t))
  (unless e (return-from minisat (values nil nil)))
  (unwind-protect
       (let ((vars ;; write dimacs
              (with-open-file (s dimacs-pathname :direction :output :if-exists :supersede)
                (prop->dimacs e s)))
             (minisat-process ;; run minisat
              (sb-ext:run-program "minisat" (list dimacs-pathname minisat-result-pathname)
                                  :wait t
                                  :search t)))
         (ecase (sb-ext:process-exit-code  minisat-process)
           (0 (error "minisat failed"))
           (1 (error "minisat interrupted by sigint or couldn't read input"))
           (3 (error "minisat couldn't parse input"))
           (10 ;; sat
            ;; parse results
            (with-open-file (fin minisat-result-pathname :direction :input)
              (let ((sat-line (read-line fin))
                    (var-line (read-line fin))
                    (code 0))
                (assert (string= sat-line "SAT"))
                (loop with start = 0
                   do (multiple-value-bind (integer new-start) (parse-integer var-line :start start :junk-allowed t)
                        (setq start new-start)
                        (when (> integer 0)
                          (setf (ldb (byte 1 (1- integer)) code) 1)))
                   while (< start (length var-line)))
                (values code vars))))
           (20 ;; unsat
            (values nil vars))))
    (unless keep-files
      (when (probe-file dimacs-pathname)
        (delete-file dimacs-pathname))
      (when (probe-file minisat-result-pathname)
        (delete-file minisat-result-pathname)))))

(defun print-logic-vars (integer vars)
  (when integer
    (loop
       for v in vars
       for i from 0
       do
         (format t "~&~A: ~A" v (not (zerop (ldb (byte 1 i)
                                                 integer))))))
  (values integer vars))

(defun decode-logic-assignment (integer vars)
  (when integer
    (loop
       for v in vars
       for i from 0
       collect
         (if (zerop (ldb (byte 1 i) integer))
             (cons 'not v)
             v))))

(defun horn-clause (body &optional head)
  "Or of body implies head"
  (flet ((helper (clause b)
           `(or (not ,b)
                ,clause)))
    (if head
        (fold #'helper head body)
        (fold #'helper (list 'not (car body)) (cdr body)))))

(defun proposition-tautology (e)
  "Is e ALWAYS true?
  Note, kinda expensive since it calls minisat."
  (not (minisat (list 'not e))))

(defun propositions-equivalent (e1 e2)
  "Is ('iff e1 e2) a tautology?
  Note, kinda expensive since it calls minisat."
  (proposition-tautology (list 'iff e1 e2)))
