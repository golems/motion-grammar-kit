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

(in-package :motion-grammar)

(defvar *fuzz-log*)
(defvar *fuzz-input*)
(defvar *fuzz-counts*)

(defun fuzz-test (name test-function)
"Call TEST-FUNCTION with no arguments.  If result is true, mark
successful test.  If result is false, print an error message.
NAME: name of thest test
TEST-FUNCTION: (lambda ()) => (or nil RESULT)
RESULT: the result of TEST-FUNCTION"
  (declare (type function test-function)
           (type symbol name))
  (let ((result (handler-case (funcall test-function)
                  (condition (e) ;error
                    (pprint `(:condition ,name :description ,(write-to-string e) :input ,*fuzz-input*)
                            *fuzz-log*)
                    (return-from fuzz-test)))))
    (if result
        (when *fuzz-counts*
          (incf (gethash name *fuzz-counts* 0)))
        (pprint `(:fail ,name :input ,*fuzz-input*) *fuzz-log*))
    result))

;; TODO: catch assertions and conditions
(defun perform-fuzz (generator tester &key
                     (formatter #'identity)
                     (log *standard-output*)
                     (count 1))
  (let ((*fuzz-counts* (make-hash-table))
        (*fuzz-log* log))
    (dotimes (i count)
      (let* ((input (funcall generator))
             (*fuzz-input* (funcall formatter input)))
        (funcall tester input)))
    (print `(:result
             ,(loop for k being the hash-keys of *fuzz-counts*
                 collect (list k (gethash k *fuzz-counts*)))))))


(defun fa-fuzz-generator ()
  (random-fa (+ 1 (random 6))
             3))

(defun curry0 (function arg)
  (lambda () (funcall function arg)))

(defun fa-fuzz-tester (fa)
  ;; first, get the minimal forms
  (let ((hop (fuzz-test 'hopcroft (curry-list #'fa-canonicalize-hopcroft fa)))
        (brz (fuzz-test 'brzozowski (curry-list #'fa-canonicalize-brzozowski fa))))
    ;; check hopcroft
    (when hop
      (fuzz-test 'hopcroft-dfa
                 (curry-list #'dfap hop))

      (fuzz-test 'hopcroft-terminals
                 (lambda () (finite-set-equal (fa-terminals fa) (fa-terminals hop)))))
    ;; check brzozowski
    (when brz
      (fuzz-test 'brzozowski-dfa
                 (curry-list #'dfap brz))
      (fuzz-test 'brzozowski-terminals
                 (lambda () (finite-set-equal (fa-terminals fa) (fa-terminals brz)))))
    ;; check that they match
    (when (and hop brz)
      (fuzz-test 'hopcroft-brzozowski-equal
                 (curry-list #'dfa-eq hop brz)))

    ;; check some other properties
    (when hop
      (let ((universal (make-universal-fa (fa-terminals fa))))
        ;; f = u \cap f
        (fuzz-test 'union-universal
                   (lambda () (dfa-eq hop
                                 (fa-canonicalize (fa-intersection universal hop)))))
        ;; u = f \cup \not f
        (fuzz-test 'union-complement
                   (lambda () (fa-universal-p (fa-union hop (fa-complement fa)))))
        ;; \emptyset = f \cap \not f
        (fuzz-test 'intersection-complement
                   (lambda () (fa-empty-p (fa-intersection hop (fa-complement fa)))))))))


(defun fa-fuzz-formatter (fa)
  `(:fa :states ,(finite-set-list (fa-states fa))
        :terminals ,(finite-set-list (fa-terminals fa))
        :edges ,(fa-edges fa)
        :start ,(fa-start fa)
        :accept ,(finite-set-list (fa-accept fa))))

(defun fa-fuzz-deformatter (list)
  (destructuring-bind (fa &key states terminals edges start accept) list
    (assert (eq :fa fa))
    (make-fa-1 states terminals edges start accept)))

(defun fa-fuzz (&key
                (count 1)
                (state-count 8)
                (terminal-count 4)
                (randomize-counts t))
  (perform-fuzz (if randomize-counts
                    (lambda () (random-fa state-count terminal-count))
                    (lambda () (random-fa (1+ (random (1- state-count)))
                                     (1+ (random (1- terminal-count))))))
                #'fa-fuzz-tester
                :formatter #'fa-fuzz-formatter
                :count count))
