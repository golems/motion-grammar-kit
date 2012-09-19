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

;; Convert a FA to C Code
;; Each token is given by a C function.
;; Function returns 0 if token matches

(defparameter *c-stream* t)
(defparameter *c-indent* 0)


(defun c-format (fmt &rest args)
  (apply #'format *c-stream* fmt args))

(defun c-indent-format (fmt &rest args)
  (format *c-stream* "~&~A"
          (make-string (max 0 *c-indent*) :initial-element #\Tab))
  (apply #'c-format fmt args))

(defmacro with-c-indent (&body body)
  `(let ((*c-indent* (1+ *c-indent*)))
     ,@body))

(defmacro with-c-block (&body body)
  `(progn
     (c-indent-format "{~%")
     (with-c-indent ,@body)
     (c-indent-format "}~%")))

(defmacro with-c-nest ((&body head) &body body)
  `(progn
     ,@head
     (with-c-block ,@body)))

(defun c-format-statement (fmt &rest args)
  (apply #'c-indent-format fmt args)
  (c-format ";~%"))

(defun c-format-goto (label)
  (c-format-statement "goto ~A" label))

(defun c-format-label (label)
  (let ((*c-indent* (1- *c-indent*)))
    (c-indent-format "~A:~%" label)))

(defun c-format-return (value)
  (c-format-statement "return ~A" value))

(defun c-format-case (i)
  (c-format-label (format nil "case ~A" i)))

(defmacro with-c-output (output &body body)
  (alexandria:with-gensyms (fun-sym output-sym)
      `(let ((,output-sym ,output))
         (flet ((,fun-sym () ,@body))
           (cond
             ((streamp ,output-sym)
              (let ((*c-stream* ,output-sym))
                (,fun-sym)))
             ((null ,output-sym)
              (with-output-to-string (*c-stream*)
                (,fun-sym)))
             ((eq ,output-sym t)
              (let ((*c-stream* *standard-output*))
                (,fun-sym)))
             ((or (stringp ,output-sym)
                  (pathnamep ,output-sym))
              (with-open-file (*c-stream* ,output-sym :if-exists  :supersede :if-does-not-exist :create :direction :output)
                (,fun-sym)))
             (t (error "Unknown output: ~A" ,output-sym)))))))


(defun c-exp (c)
  (with-c-output nil
    (if (atom c)
        (c-format "~A" c)
        (spec-case c
          (((binop is (or :== :!= :+ :* :/ :& :&&)) a b)
           (c-format "(~A ~A ~A)"
                     (c-exp a) binop (c-exp b)))

          ((:or a b)
           (c-format "(~A || ~A)"
                     (c-exp a) (c-exp b)))
          (((binop is (or  :. :->)) a b)
           (c-format "(~A~A~A)"
                     (c-exp a) binop b))
          ((:call name &rest args)
           (c-format "(~A(~{~A~^, ~}))"
                     (c-exp name)
                     (map 'list #'c-exp args)))
          (((unop is (or :~ :!)) a)
           (c-format "(~A ~A)" unop
                     (c-exp a)))
          (a
           (error "Unknown c-expression: ~A" a))))))

(defun c-gen (c)
  (spec-case c
    ((&ignore (op is (or :== :!= :call :! :-> :&& :or)) &rest rest)
     (c-exp c))
    ((:= lhs rhs)
     (c-indent-format "~A = ~A;~%"
                      (c-exp lhs) (c-exp rhs)))
    ((:seq &rest rest)
     (map nil #'c-gen rest))
    ((:stmt name)
     (c-gen name)
     (c-format ";~&"))
    ((:goto label)
     (c-format-goto label))
    ((:return value)
     (c-format-return (c-exp value)))
    ((:label label)
     (c-format-label label))
    ((:case label)
     (c-format-case label))
    ((:block &rest body)
     (with-c-block
       (map nil #'c-gen body)))
    ((:if test then-clause &optional else-clause)
     (if (cdr then-clause)
         (with-c-nest ((c-indent-format "if ( ~A )" (c-exp test)))
           (map nil #'c-gen then-clause))
         (progn
           (c-indent-format "if ( ~A )" (c-exp test))
           (with-c-indent (C-gen (car then-clause)))))
     (when else-clause
       (if (cdr else-clause)
           (with-c-nest ((c-indent-format "else"))
             (map nil #'c-gen else-clause))
           (progn
             (c-indent-format "else")
             (with-c-indent (C-gen (car else-clause)))))))
    ((:comment str)
     (c-indent-format "// ~A" str))
    (a
     (error "Unknown c-code: ~A" a))))


(defun csymbol (gsymbol &optional prefix)
  (let ((name (string-downcase (gsymbol-name gsymbol))))
    (with-output-to-string (s)
      (when prefix (princ (csymbol prefix) s))
      (labels ((putchars (c) (princ c s)))
        (loop for c across name
           do (case c
                (#\- (putchars #\_))
                (#\= (putchars "_equal_"))
                (#\Space (putchars "_sp_"))
                (#\( (putchars "_lp_"))
                (#\) (putchars "_rp_"))
                (otherwise (putchars c))))))))



(defun print-c-parser-predicate (stream symbol context)
  (cond
    ((atom symbol)
     (format stream "(~A) -> ~A" context (csymbol symbol)))
    ((and (listp symbol)
          (eq :not (car symbol)))
     (destructuring-bind (not clause) symbol
       (assert (eq not :not))
       (format stream "(!(~A) -> ~A)" context (csymbol clause))))
    (t (error "Unknown predicate ~A" symbol))))


(defun print-c-parser-funcall (stream symbol context)
  (if (and (listp symbol)
           (eq :predicate (car symbol)))
      (print-c-parser-predicate stream (cadr symbol) context)
      (format stream "~A( ~A )" (csymbol symbol) context)))

(defun c-parser-test (symbol context)
  (spec-case symbol
    ((:predicate (:not x))
     (list :! (c-parser-test `(:predicate ,x) context)))
    ((:predicate x)
     (list :-> context (csymbol x)))
    (x `(:== 0 (:call ,(csymbol x) ,context)))))


(defun print-c-parser-stub (stream symbol context-type context)
  (if (and (listp symbol)
           (eq :predicate (car symbol)))
      (format stream "~&/* No code for predicate symbol */~%")
      (format stream "~&static int ~A( ~A ~A ) {~%~%~%}~%~%"
              (csymbol symbol) context-type context)))



(defun fa->c-stub (fa &key
                   output
                   (halt-function "halt")
                   ;(static-functions t)
                   (context-type "void*")
                   (print-stub #'print-c-parser-stub))
  (output-function (lambda (s)
                     (format s "~&/************************************/~&")
                     (format s "~&/* STUB FUNCTIONS FOR MOTION PARSER */~&")
                     (format s "~&/************************************/~&")
                     (format s "~%/* Each function must return 0 for an occuring terminal and nonzero otherwise */~%~%")
                     (do-finite-set (z (finite-set-add (fa-terminals fa) halt-function))
                       (format s "~&/* Terminal Symbol function for \"~A\" */~&" z)
                       (funcall print-stub s z context-type "context")))
                       ;(format s "~&~:[~;static ~]int ~A( ~A context ) {~%~%~%}~%~%" static-functions (csymbol z) context-type)))
                   output))

(defun fa->c-parser (fa &key
                     output
                     (function-name "mgparse")
                     (header "")
                     (halt-function "halt")
                     (context-type "void*"))
  (let* ((fa (fa-canonicalize fa))
         (mover (dfa-mover fa)))
    (with-c-output output
      (c-indent-format "/*****************/~&")
      (c-indent-format "/* MOTION PARSER */~&")
      (c-indent-format "/*****************/~&")
      (when header (c-indent-format "~A" header))
      (with-c-nest ((c-indent-format "int ~A( ~A context )" function-name context-type))
        (labels ((state-label (X) (csymbol X "state")))
          (c-gen `(:goto ,(state-label (fa-start fa))))
          (do-finite-set (q0 (fa-states fa))
            (c-gen `(:label ,(state-label q0)))
            (do-finite-set (z (fa-terminals fa))
              (let ((q1 (funcall mover q0 z)))
                (when q1
                  (c-gen `(:if ,(c-parser-test z "context")
                               ((:goto ,(state-label q1))))))))
            (when (finite-set-inp q0 (fa-accept fa))
              (c-gen `(:if ,(c-parser-test halt-function "context")
                           ((:return 0)))))
            (c-gen '(:return -1))))))))


(defun fa->c-file (fa pathname &key
                   (function-name "mgparse")
                   (halt-function "halt")
                   (context-type "void*")
                   (print-stub #'print-c-parser-stub)
                   (stub-pathname (make-pathname :directory (pathname-directory pathname)
                                                 :name (concatenate 'string (pathname-name pathname) "-terminal-stub")
                                                 :type "c")))
  (fa->c-stub fa
              :output stub-pathname :halt-function halt-function :context-type context-type
              :print-stub print-stub)
  (fa->c-parser fa :output pathname
                :header (format nil "#include \"~A.c\" ~&" (pathname-name stub-pathname))
                :function-name function-name
                :halt-function halt-function
                :context-type context-type
                ))



;; Generate a predictive parser for LL(1) grammars
;; - Use the C call stack for the Context-Free Stack
;; - Single parsing function -- necessary for tail-call optimization to jumps
;; - Switch-case dispatches to appropriate label for a nonterminal
;; - Optimize tail recursion to a jump

(defun grammar->c-predictive-parser (grammar &key
                                    output
                                     (function-name "mgparse")
                                     (header "")
                                     includes
                                     (context-type "void*"))
  (let* ((table (make-predictive-table grammar :duplicate-error-p t))
         (nonterminals (grammar-nonterminals grammar))
         (terminals (grammar-terminals grammar))
         (start (grammar-start-nonterminal grammar))
         (nonterm-array (make-array (finite-set-length nonterminals)
                                    :initial-contents
                                    (cons (grammar-start-nonterminal grammar)
                                          (finite-set-list (finite-set-remove nonterminals start)))))
         (hash (make-hash-table :test #'equal)))
    ;; index symbol case numbers
    (loop
       with k = -1
       for x across nonterm-array
       do (setf (gethash (list x) hash) (incf k))
         (do-finite-set (z terminals)
           (when (funcall table x z)
             (setf (gethash (cons x z) hash) (incf k)))))
    ;; output stuff
    (labels ((case-label (X &optional a)
               (if a
                   (csymbol (list X a) "prod_")
                   (csymbol X "nonterm_")))
             (case-number (x &optional a) (gethash (cons x a) hash))
             (collect-tails (tail)
               (cond
                 ;; no more symbols, return
                 ((null tail)
                  '((:return 0)))
                 ;; tail nonterminal, tail optimize
                 ((and (null (cdr tail))
                       (finite-set-inp (car tail) nonterminals))
                  `((:goto ,(case-label (car tail)))))
                  ;; non-tail nonterminal, call
                  ((finite-set-inp (car tail) nonterminals)
                   `((:if (:!= 0 (:call ,function-name "context"
                                        ,(case-number (car tail) nil)))
                          ((:return -1)))
                     ,@(collect-tails (cdr tail))))
                  ;; terminal, check it
                  ((finite-set-inp (car tail) terminals)
                   `((:if (:! ,(c-parser-test (car tail) "context"))
                          ((:return -1)))
                     ,@(collect-tails (cdr tail))))
                  (t (error "Unknown symbol type ~A" (car tail))))))
      (with-c-output output
        (when header (c-indent-format "~A" header))
        (dolist (i includes)
          (c-indent-format "#include ~A~%" i))
        (c-indent-format "/*****************/~&")
        (c-indent-format "/* MOTION PARSER */~&")
        (c-indent-format "/*****************/~&")
        (with-c-nest ((c-indent-format "int ~A( ~A context, int i )" function-name context-type))
          ;; nonterms
          (with-c-nest ((c-indent-format "switch( i )"))
            (loop
               for X across nonterm-array
               do
               ;; nonterminal label
                 (c-gen `(:seq (:case ,(case-number X nil))
                               (:label ,(case-label X))))
               ;; test each following production
                 (do-finite-set (a terminals)
                   (let ((production (funcall table X a)))
                     ;; got a live production
                     (when production
                       (c-gen `(:comment ,production))
                       (c-gen (destructuring-bind (head term0 &rest rest) production
                                (assert (equal X head))
                                `(:if ,(c-parser-test a "context")
                                      ((:case ,(case-number X a))
                                       (:label ,(case-label X a))
                                       ,@(cond
                                          ;; eat first terminal and continue
                                          ((and (equal term0 a)
                                                rest)
                                           (collect-tails rest))
                                          ;; eat first terminal and done
                                          ((and (equal term0 a)
                                                (null rest))
                                           (list '(:return 0)))
                                          ((and (finite-set-inp term0 nonterminals)
                                                rest)
                                           ;; Production starts with nonterminal, recurse
                                           `((:if (:call ,function-name "context"
                                                         ,(case-number term0 a))
                                                  ,(collect-tails rest)
                                                  ((:return -1)))))
                                          ((and (finite-set-inp term0 nonterminals)
                                                (null rest))
                                           ;; Production starts with nonterminal, jump
                                           `((:goto ,(case-label term0 a))))
                                          (t (error "Can't handle production ~A" production))))))))))
               ;; no match, syntax error
                 (c-gen '(:return -1)))
            ;; default
            (c-gen '(:seq (:label "default")
                     (:return -1)))))))))



(defun grammar->c-file (grammar pathname &key
                        (function-name "mgparse")
                        (context-type "void*")
                        ;(print-stub #'print-c-parser-stub)
                        (stub-pathname (make-pathname :directory (pathname-directory pathname)
                                                      :name (concatenate 'string (pathname-name pathname) "-terminal-stub")
                                                      :type "c")))
  ;(fa->c-stub fa
              ;:output stub-pathname :halt-function halt-function :context-type context-type
              ;:print-stub print-stub)
  (grammar->c-predictive-parser grammar :output pathname
                                :header (format nil "#include \"~A.c\" ~&" (pathname-name stub-pathname))
                                :function-name function-name
                                :context-type context-type))

(defun grammar->c-supervised-predictive-parser (grammar &key
                                                output
                                                (function-name "mgparse")
                                                (header "")
                                                (context-type "void*"))
  (let* ((table (make-predictive-table grammar :duplicate-error-p t))
         (nonterminals (grammar-nonterminals grammar))
         (terminals (finite-set-tree (grammar-terminals grammar)))
         (start (grammar-start-nonterminal grammar))
         (nonterm-array (make-array (finite-set-length nonterminals)
                                    :initial-contents
                                    (cons (grammar-start-nonterminal grammar)
                                          (finite-set-list (finite-set-remove nonterminals start)))))
         (hash (make-hash-table :test #'equal))
         (term-hash (make-hash-table :test #'equal)))
    ;; index symbol case numbers
    (loop
       with k = -1
       for x across nonterm-array
       do (setf (gethash (list x) hash) (incf k))
         (do-finite-set (z terminals)
           (when (funcall table x z)
             (setf (gethash (cons x z) hash) (incf k)))))
    (let ((i -1))
      (dolist (z (finite-set-list terminals))
        (setf (gethash z term-hash) (incf i))
        (print (list z i))))
    ;; output stuff
    (labels ((case-label (X &optional a)
               (if a
                   (csymbol (list X a) "prod_")
                   (csymbol X "nonterm_")))
             (case-number (x &optional a) (gethash (cons x a) hash))
             (collect-tails (tail)
               (cond
                 ;; no more symbols, return
                 ((null tail)
                  '((:return 0)))
                 ;; tail nonterminal, tail optimize
                 ((and (null (cdr tail))
                       (finite-set-inp (car tail) nonterminals))
                  `((:goto ,(case-label (car tail)))))
                  ;; non-tail nonterminal, call
                  ((finite-set-inp (car tail) nonterminals)
                   `((:if (:!= 0 (:call ,function-name "context" "table"
                                        ,(case-number (car tail) nil)))
                          ((:return -1)))
                     ,@(collect-tails (cdr tail))))
                  ;; terminal, check it
                  ((finite-set-inp (car tail) terminals)
                   `((:if (:or (:! (:call "mg_supervisor_allow" "table"
                                          ,(gethash (car tail) term-hash)))
                               (:! ,(c-parser-test (car tail) "context")))
                          ((:return -1)))
                     (:= (:-> "table" "state")
                         (:call "mg_supervisor_next_state" "table"
                                ,(gethash (car tail) term-hash)))
                     ,@(collect-tails (cdr tail))))
                  (t (error "Unknown symbol type ~A" (car tail))))))
      (with-c-output output
        (when header (c-indent-format "~A" header))
        (c-indent-format "/*****************/~&")
        (c-indent-format "/* MOTION PARSER */~&")
        (c-indent-format "/*****************/~&")
        (with-c-nest ((c-indent-format "int ~A( ~A context, mg_supervisor_table_t *table, int i )" function-name context-type))
          ;; nonterms
          (with-c-nest ((c-indent-format "switch( i )"))
            (loop
               for X across nonterm-array
               do
               ;; nonterminal label
                 (c-gen `(:seq (:case ,(case-number X nil))
                               (:label ,(case-label X))))
               ;; test each following production
                 (do-finite-set (a terminals)
                   (let ((production (funcall table X a)))
                     ;; got a live production
                     (when production
                       (c-gen `(:comment ,production))
                       (c-gen (destructuring-bind (head term0 &rest rest) production
                                (assert (equal X head))
                                `(:if (:&& (:call "mg_supervisor_allow" "table"
                                                  ,(gethash a term-hash))
                                           ,(c-parser-test a "context"))
                                      ((:= (:-> "table" "state")
                                           (:call "mg_supervisor_next_state" "table"
                                                  ,(gethash a term-hash)))
                                       (:case ,(case-number X a))
                                       (:label ,(case-label X a))
                                       ,@(cond
                                          ;; eat first terminal and continue
                                          ((and (equal term0 a)
                                                rest)
                                           (collect-tails rest))
                                          ;; eat first terminal and done
                                          ((and (equal term0 a)
                                                (null rest))
                                           (list '(:return 0)))
                                          ((and (finite-set-inp term0 nonterminals)
                                                rest)
                                           ;; Production starts with nonterminal, recurse
                                           `((:if (:call ,function-name "context", "table"
                                                         ,(case-number term0 a))
                                                  ,(collect-tails rest)
                                                  ((:return -1)))))
                                          ((and (finite-set-inp term0 nonterminals)
                                                (null rest))
                                           ;; Production starts with nonterminal, jump
                                           `((:goto ,(case-label term0 a))))
                                          (t (error "Can't handle production ~A" production))))))))))
               ;; no match, syntax error
                 (c-gen '(:return -1)))
            ;; default
            (c-gen '(:seq (:label "default")
                     (:return -1)))))))))
