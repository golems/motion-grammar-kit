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


;;;;;;;;;;;;;;;;;;;;;
;; FINITE AUTOMATA ;;
;;;;;;;;;;;;;;;;;;;;;

(defun fa-fuzz-tester (fa)
  ;; first, get the minimal forms
  (let ((hop (fuzz:test-true 'hopcroft (curry #'fa-canonicalize-hopcroft fa)))
        (brz (fuzz:test-true 'brzozowski (curry #'fa-canonicalize-brzozowski fa))))
    ;; check hopcroft
    (when hop
      (fuzz:test-predicate 'hopcroft-dfa
                           #'dfa-p (thunk hop))

      (fuzz:test-predicate 'hopcroft-terminals
                           #'finite-set-equal
                           (curry #'fa-terminals fa)
                           (curry #'fa-terminals hop)))
    ;; check brzozowski
    (when brz
      (fuzz:test-predicate 'brzozowski-dfa
                           #'dfa-p (thunk brz))
      (fuzz:test-predicate 'brzozowski-terminals
                           #'finite-set-equal
                           (curry #'fa-terminals fa)
                           (curry #'fa-terminals brz)))
    ;; check that they match
    (when (and hop brz)
      (fuzz:test-predicate 'hopcroft-brzozowski-equal
                           #'dfa-eq
                           (thunk hop)
                           (thunk brz)))

    ;; check some other properties
    (when hop
      (let ((universal (make-universal-fa (fa-terminals fa))))
        ;; f = u \cap f
        (fuzz:test-predicate 'union-universal
                             #'dfa-eq
                             (thunk hop)
                             (thunk
                               (fa-canonicalize (fa-intersection universal hop))))
        ;; u = f \cup \not f
        (fuzz:test-predicate 'union-complement
                             #'fa-universal-p
                             (thunk (fa-union hop (fa-complement fa))))
        ;; \emptyset = f \cap \not f
        (fuzz:test-predicate 'intersection-complement
                             #'fa-empty-p
                             (thunk (fa-intersection hop (fa-complement fa))))))))


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
  (fuzz:run-tests (if randomize-counts
                      (thunk (random-fa state-count terminal-count))
                      (thunk (random-fa (random-whole state-count)
                                        (random-whole terminal-count))))
                  #'fa-fuzz-tester
                  :formatter #'fa-fuzz-formatter
                  :count count))


;;;;;;;;;;
;; TRIE ;;
;;;;;;;;;;

(defun random-string (base length)
  (loop for i below (random-whole length)
     collect (random base)))

(defun trie-fuzz (&key
                  (count 1)
                  (string-count 100)
                  (string-base 100)
                  (string-length 100))

  (fuzz:run-tests (thunk (loop for i below (random-whole string-count)
                              collect (random-string string-base string-length)))
                  (lambda (strings)
                    (let ((trie (fold #'trie-insert nil strings))
                          (set (remove-duplicates strings :test #'equal)))
                      (let ((trie-set))
                        (map-trie (lambda (s) (push s trie-set)) trie)
                        (fuzz:test-predicate 'trie-set #'finite-set-equal
                                             (thunk set)
                                             (thunk trie-set)))))
                  :count count))

;;;;;;;;;
;; ATN ;;
;;;;;;;;;

(defun random-grammar (nonterminal-count terminal-count production-count production-length)
  (let* ((n-nonterm (random-whole nonterminal-count))
         (n-term (random-whole terminal-count))
         (n-sym (+ n-nonterm n-term)))
    (let ((nonterminals (loop for i below n-nonterm
                           collect (list 'nonterm i)))
          (terminals (loop for i below n-term
                      collect (list 'term i))))
      (let ((symbols (finite-set-union nonterminals terminals)))
        (loop for k below (random-whole production-count)
           collect (cons (elt nonterminals (random n-nonterm))
                         (loop for i below (random-whole production-length)
                            collect (elt symbols (random n-sym)))))))))


(defun atn-fuzz (&key (count 1))
  (fuzz:run-tests (thunk (random-grammar 10 20 20 5))
                  (lambda (grammar)
                    (fuzz:test-predicate 'atn<->grammar
                                         #'finite-set-equal
                                         (thunk grammar)
                                         (thunk (atn->grammar (grammar->atn grammar)))))
                  :count count))
