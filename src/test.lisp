;;;; -*- Lisp -*-
;;;;
;;;; Copyright (c) 2011, Georgia Tech Research Corporation
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

(lisp-unit:REMOVE-TESTS :ALL)

(defmacro assert-finite-set-equal (a b)
  `(lisp-unit:assert-true (finite-set-equal ,a ,b)))

(lisp-unit:define-test dfa-equal-basic
  (lisp-unit:assert-true (dfa-equal (make-fa '((0 a 1) (1 b 0)) 0 (finite-set 1))
                                    (make-fa '((x a y) (y b x)) 'x (finite-set 'y))))
  (lisp-unit:assert-false (dfa-equal (make-fa '((0 a 1) (1 b 0)) 0 (finite-set 1))
                                     (make-fa '((x a y) (y b x)) 'y (finite-set 'x)))))


(lisp-unit:define-test util
  (flet ((relation (a b)
           (= (eval a) (eval b))))
    (lisp-unit:assert-true (finite-set-equivalent #'relation
                                                  '(1 2 3)
                                                  '((+ 2 1) (+ 1 1) (+ 1 0))))

    (lisp-unit:assert-false (finite-set-equivalent #'relation
                                                   '(1 2 3)
                                                   '((+ 2 1) (+ 1 1) (+ 1 1))))
    (lisp-unit:assert-false (finite-set-equivalent #'relation
                                                   '(1 2 3)
                                                   '((+ 2 1) (+ 1 1) (+ 1 0) 5)))
    (lisp-unit:assert-true (finite-set-relation-subset #'relation
                                                       '(1 2 3)
                                                       '((+ 2 1) (+ 1 1) (+ 1 0) 5)))))

(defmacro test-fa (fa)
  `(progn
     (lisp-unit:assert-true (dfa-p (fa-minimize-hopcroft ,fa)))
     (lisp-unit:assert-true (dfa-eq (fa-canonicalize-brzozowski ,fa)
                                    (fa-canonicalize-hopcroft ,fa)))
     ;; check some identities
     (let ((universal (make-universal-fa (fa-terminals ,fa))))
       ;; f = u \cap f
       (lisp-unit:assert-true (dfa-eq (fa-canonicalize ,fa)
                                      (fa-canonicalize (fa-intersection universal ,fa))))
       ;; u = f \cup \not f
       (lisp-unit:assert-true (fa-universal-p (fa-union ,fa (fa-complement ,fa))))
       ;; \emptyset = f \cap \not f
       (lisp-unit:assert-true (fa-empty-p (fa-intersection ,fa (fa-complement ,fa)))))))

(defmacro test-regex (regex)
  `(progn
     ;; check minimization matches
     (test-fa (regex->nfa ,regex))
     ;; check equivalent regexes
     (lisp-unit:assert-true (dfa-eq (regex->dfa ,regex)
                                    (regex->dfa (fa->regex (regex->dfa ,regex)))))))

(defmacro test-regex-min-fa (regex min-fa)
  `(progn
     (test-regex ,regex)
     (test-fa ,min-fa)
     (lisp-unit:assert-true (dfa-eq (dfa-renumber ,min-fa)
                                    (regex->dfa ,regex)))))

(defmacro test-regex-fa (regex fa)
  `(progn
     (test-fa ,fa)
     (test-regex-min-fa ,regex (fa-minimize-brzozowski ,fa))))


(defmacro test-fa-min-fa (fa min-fa)
  `(progn
     (lisp-unit:assert-true (dfa-eq (dfa-renumber ,min-fa)
                                    (fa-canonicalize-hopcroft ,fa)))
     (lisp-unit:assert-true (dfa-eq (dfa-renumber ,min-fa)
                                    (fa-canonicalize-brzozowski ,fa)))))


(lisp-unit:define-test fa-regression
  ;; no edges to start state, start in accept
  (lisp-unit:assert-true (dfa-p (fa-canonicalize-hopcroft (make-fa '((1 0 0) (1 0 2)) 0 '(0)))))
  (lisp-unit:assert-true (dfa-eq (fa-canonicalize-brzozowski (make-fa-1 '(0) '(0)
                                                                       nil 0 '(0)))
                                 (fa-canonicalize-hopcroft (make-fa '((1 0 0) (1 0 2)) 0 '(0)))))

  (lisp-unit:assert-true (let ((fa (make-fa '((3 0 0)) 0  '(0))))
                           (dfa-eq (fa-canonicalize-hopcroft fa)
                                   (fa-canonicalize-brzozowski fa))))

  ;; token sets for empty fa
  (lisp-unit:assert-true (fa-empty-p (fa-canonicalize-brzozowski (make-fa '((1 1 1)) 0 '(1)))))
  (lisp-unit:assert-true (fa-empty-p (fa-canonicalize-hopcroft (make-fa '((1 1 1)) 0 '(1)))))

  ;;
  (test-fa (make-fa '((1 1 1)) 0 '(1)))
  (test-fa  (make-fa '((0 0 1) (1 1 2) (2 0 1) (2 2 3) (3 0 4) (4 1 2) (4 0 5))
                                            0 '(1 4)))

  ;;;;
 (test-fa (fa-fuzz-deformatter  '(:fa :states (0 1 2 3 4) :terminals (0 1 2) :edges
                                  ((3 0 2) (3 1 3) (2 0 4) (3 2 2) (0 0 1) (0 1 1) (3 1 3) (1 1 2) (2 1 0)
                                   (0 1 2) (0 0 4) (1 0 3) (4 2 1))
                                  :start 0 :accept (0 4))))

  (test-fa (fa-fuzz-deformatter
            '(:fa :states (0 1 2 3 4 5 6) :terminals (0 1 2 3) :edges
              ((1 3 4) (0 1 3) (1 2 1) (5 1 4) (0 2 2) (1 1 0) (4 2 6) (3 1 0) (6 0 0)
               (0 0 3) (0 0 1) (5 2 2) (6 1 1) (5 2 5) (3 0 6) (1 1 5) (0 1 2) (6 3 6)
               (4 1 5) (0 2 4) (2 1 4) (2 3 0) (6 2 3) (4 2 1) (5 3 4) (0 3 0)
               (2 0 4))
              :start 0 :accept (0 1))))

  (test-fa (fa-fuzz-deformatter
            '(:fa :states (0 1 2 3 4 5 6) :terminals (0 1 2 3) :edges
              ((1 3 4) (0 1 3) (1 2 1) (5 1 4) (0 2 2) (1 1 0) (4 2 6) (3 1 0) (6 0 0)
               (0 0 3) (0 0 1) (5 2 2) (6 1 1) (5 2 5) (3 0 6) (1 1 5) (0 1 2) (6 3 6)
               (4 1 5) (0 2 4) (2 1 4) (2 3 0) (6 2 3) (4 2 1) (5 3 4) (0 3 0)
               (2 0 4))
              :start 0 :accept (0 1))))

  (test-fa (fa-fuzz-deformatter '(:fa :states (0 1 2 3 4) :terminals (0 1 2) :edges
                                  ((2 1 1) (2 0 3) (2 1 2) (1 1 4) (4 1 0) (3 1 1) (4 2 4) (3 2 2) (0 1 1)
                                   (4 0 0) (1 2 3) (0 1 3) (0 0 2))
                                  :start 0 :accept (1 4))))


  )

(lisp-unit:define-test dfa-minimize
  (test-fa-min-fa (make-fa '((0 a 1) (1 b 2) (2 a 1)) 0 (finite-set 1))
                  (make-fa '((0 a 1) (1 b 0)) 0 (finite-set 1)))

  (test-fa (make-fa '((0 a 1) (1 b 2) (2 a 1)) 0 (finite-set 1)))

  (test-fa (make-fa '((0 a 1) (1 b 0) (1 c 2) (2 a 1)
                      (0 e 3) (1 e 3) (2 e 3))
                    0 (finite-set 3))))

;; examples from the dragon book
(lisp-unit:define-test fa-dragon
  (let* ((fig-3-56  '(:concatenation (:closure (:union a b)) a b b))
         (fig-3-63 (make-fa '((0 b 0) (0 a 1) (1 a 1) (1 b 2) (2 a 1) (2 b 3)
                              (3 a 1) (3 b 0))
                            0 (finite-set 3))))
    (test-regex-min-fa fig-3-56 fig-3-63)))

;; examples from Hopcroft '79
(lisp-unit:define-test fa-hopcroft-79
  (let ((fig-2-15-a (make-fa '((q7 :epsilon q5) (q7 :epsilon q8)
                               (q5 1 q6)
                               (q6 :epsilon q5) (q6 :epsilon q8))
                             'q7 (finite-set 'q8)))
        (fig-2-15-b (make-fa '((q3 0 q4)
                               (q4 :epsilon q7)
                               (q7 :epsilon q5) (q7 :epsilon q8)
                               (q5 1 q6)
                               (q6 :epsilon q5) (q6 :epsilon q8))
                             'q3 (finite-set 'q8)))
        (fig-2-15-c (make-fa '((q9 :epsilon q1) (q9 :epsilon q3)
                               (q1 1 q2)
                               (q2 :epsilon q10)
                               (q3 0 q4)
                               (q4 :epsilon q7)
                               (q7 :epsilon q8) (q7 :epsilon q5)
                               (q5 1 q6)
                               (q6 :epsilon q5) (q6 :epsilon q8)
                               (q8 :epsilon q10))
                             'q9 (finite-set 'q10)))
        (regex-2-15-a '(:closure 1))
        (regex-2-15-b '(:concatenation 0 (:closure 1)))
        (regex-2-15-c '(:union (:concatenation 0 (:closure 1)) 1)))
    (test-regex-fa regex-2-15-a
                   fig-2-15-a)
    (test-regex-fa regex-2-15-b
                   fig-2-15-b)
    (test-regex-fa regex-2-15-c
                   fig-2-15-c))



  (let ((ex-3-7 '(:concatenation (:closure 0) 1 (:closure 0)))
        (fig-3-2 (make-fa '((a 0 b) (a 1 c)
                            (b 0 a) (b 1 d)
                            (c 0 e) (c 1 f)
                            (d 0 e) (d 1 f)
                            (e 0 e) (e 1 f)
                            (f 0 f) (f 1 f))
                          'a '(c d e)))
        (fig-3-4 (make-fa '((e 0 e) (e 1 1)
                            (1 0 1) (1 1 11)
                            (11 0 11) (11 1 11))
                          'e (finite-set 1)))
        (fig-3-4-min (make-fa '((e 0 e) (e 1 1)
                                (1 0 1))
                              'e (finite-set 1))))
    ;; mostly-minimal
    (test-fa-min-fa fig-3-4 fig-3-4-min)

    ;; bigger dfa
    (test-fa-min-fa fig-3-2 fig-3-4-min)

    ;; regex/fa
    (test-regex-fa ex-3-7 fig-3-2))

  (let ((fig-3-5 (make-fa '((a 0 b) (a 1 f)
                            (b 0 g) (b 1 c)
                            (c 0 a) (c 1 c)
                            (d 0 c) (d 1 g)
                            (e 0 h) (e 1 f)
                            (f 0 c) (f 1 g)
                            (g 0 g) (g 1 e)
                            (h 0 g) (h 1 c))
                          'a (finite-set 'c)))
        (fig-3-7 (make-fa '((a-e 0 b-h) (a-e 1 d-f)
                            (b-h 0 g)   (b-h 1 c)
                            (g 0 g)     (g 1 a-e)
                            (c 0 a-e)   (c 1 c)
                            (d-f 0 c)   (d-f 1 g))
                          'a-e (finite-set 'c))))
    (lisp-unit:assert-true
     (dfa-equal fig-3-7
                (fa-minimize-hopcroft fig-3-5)))

    (lisp-unit:assert-true
     (dfa-equal fig-3-7
                (fa-minimize-brzozowski fig-3-5))))
  t)



;; examples from Sipser
(lisp-unit:define-test fa-sipser
  (let ((fig-1-42 (make-fa '((1 :epsilon 3) (1 b 2)
                             (2 a 2) (2 a 3) (2 b 3)
                             (3 a 1))
                           1 (finite-set 1)))
        (fig-1-44 (make-fa '((2 a 23) (2 b 3)
                             (3 a 13)
                             (13 a 13) (13 b 2)
                             (23 b 3) (23 a 123)
                             (123 a 123) (123 b 23))
                           13 (list 13 123))))
    (test-fa-min-fa fig-1-42 fig-1-44)))


(lisp-unit:define-test regex-dfa-matcher
  (labels ((equiv-hop-brz (regex)
             (let* ((nfa (regex->nfa regex)))
               (dfa-equal (fa-canonicalize-brzozowski nfa) (fa-canonicalize-hopcroft nfa))))
           (match-dfa (regex string)
             (funcall (chain regex
                             #'regex->nfa
                             #'nfa->dfa
                             #'dfa->string-matcher)
                      string))
           (match-hop (regex string)
             (funcall (chain regex
                             #'regex->nfa
                             #'fa-minimize-hopcroft
                             #'dfa->string-matcher)
                      string))
           (match-brz (regex string)
             (funcall (chain regex
                             #'regex->nfa
                             #'fa-minimize-brzozowski
                             #'dfa->string-matcher)
                      string)))
    (macrolet ((test (regex positive negative)
                 `(progn
                    (test-regex ',regex)
                    ,@(mapcan
                       (lambda (p)
                         `((lisp-unit:assert-true (match-dfa ',regex ,p))
                           (lisp-unit:assert-true (match-hop  ',regex,p))
                           (lisp-unit:assert-true (match-brz  ',regex,p))))
                       positive)
                    ,@(mapcan
                       (lambda (n)
                         `((lisp-unit:assert-false (match-dfa ',regex ,n))
                           (lisp-unit:assert-false (match-hop ',regex ,n))
                           (lisp-unit:assert-false (match-brz ',regex ,n))))
                       negative))))
      (test (:closure #\a)
            ("" "a" "aa" "aaa" "aaa")
            ("b" "ba" "aab" "baaa" "aba"))
      (test (:concatenation (:closure #\a) #\b)
            ("b" "ab" "aab" "aaab" "aaaaaab")
            ("" "a" "aba" "aaa" "baaa"))
      ;; Sipser p. 65, some "interesting" regexes
      (test (:concatenation (:closure #\a) #\b (:closure #\a))
            ("b" "ab" "aab" "baa" "aba" "aaba" "aaaaaab")
            ("" "a" "abba" "baaab" "bbaaa"))
      (test (:concatenation (:closure (:union #\a #\b #\c))
                            #\a #\a #\b
                            (:closure (:union #\a #\b #\c)))
            ("aab" "aaab" "aaba" "aabaa" "aabaaba")
            ("" "a" "abba" "baaa" "bbaaa"))
      (test (:closure (:concatenation (:union #\a #\b)
                                      (:union #\a #\b)))
            ("" "aa" "bb" "ab" "ba" "aaaa" "aaab" "aaba" "bbaa")
            ("a" "a" "b" "aaa" "aba" "baa" "bbb"))
      (test (:closure (:concatenation (:union #\a #\b)
                                      (:union #\a #\b)
                                      (:union #\a #\b)))
            ("" "aaa" "bbb" "aab" "aba" "aaaaaa" "bbbbbb" "aaabbb")
            ("a" "a" "b" "aa" "ab" "ba" "babb" "bbaaa"))
      (test (:union (:concatenation #\a (:closure (:union #\a #\b)) #\a)
                    (:concatenation #\b (:closure (:union #\a #\b)) #\b)
                    #\a #\b)
            ("a" "b" "aa" "bb" "aba" "bab" "abaa" "bababab")
            ("ab" "ba" "abb" "bba" "ababab"))
      t)))



(lisp-unit:define-test fa-convert
  (let ((state-fa (make-fa '((eat hungry sleep)
                             (eat thirsty drink)
                             (sleep hungry eat)
                             (sleep thirsty drink)
                             (drink hungry eat)
                             (drink tired sleep))
                           'sleep '(sleep)))
        (edge-fa (make-fa '((eat-1 hungry sleep-0)
                            (eat-1 thirsty drink-0)
                            (sleep-1 hungry eat-0)
                            (sleep-1 thirsty drink-0)
                            (drink-1 hungry eat-0)
                            (drink-1 tired sleep-0)
                            (eat-0 eat eat-1)
                            (sleep-0 sleep sleep-1)
                            (drink-0 drink drink-1))
                          'sleep-0 '(sleep-1))))
    (lisp-unit:assert-true (fa-equiv (fa-minimize-brzozowski (fa-state->edge state-fa))
                                     (fa-minimize-hopcroft edge-fa)))))

(lisp-unit:define-test fa-rewrite-states
  (let* ((orig (make-fa '((a 1 b)
                          (b 2 c)
                          (c 3 d)
                          )
                        'a '(b d)))
         (fun (lambda (x) (format nil "~A~A" x x)))
         (expected (make-fa '((aa 1 bb)
                              (bb 2 cc)
                              (cc 3 dd)
                              )
                            'aa '(bb dd)
                            ))
         (result (fa-rewrite-states fun orig))
        )

    (lisp-unit:assert-true (dfa-equal result
                                      expected))))

(lisp-unit:define-test fa-rewrite-edges
  (let* ((orig (make-fa '((a 1 b)
                          (b 2 c)
                          (c 3 d)
                          )
                        'a '(b d)))
         (fun (lambda (x) (* x x)))
         (expected (make-fa '((a 1 b)
                              (b 4 c)
                              (c 9 d)
                              )
                            'a '(b d)
                            ))
         (result (fa-rewrite-edges fun orig))
        )

    (lisp-unit:assert-true (dfa-equal result
                                      expected))))

(lisp-unit:define-test fa-op
  ;; intersection
  (let ((r-1 '(:closure (:concatenation a a b)))
        (r-2 '(:concatenation  (:closure (:union a b)) b))
        (r-i '(:concatenation a a b  (:closure (:concatenation a a b)))))
    (let ((fa-1 (regex->dfa r-1))
          (fa-2 (regex->dfa r-2))
          (fa-i (regex->dfa r-i)))
    (lisp-unit:assert-true
     (fa-equiv (regex->nfa r-i)
               (fa-intersection fa-1 fa-2)))
    (lisp-unit:assert-true (fa-empty-p (fa-intersection fa-1 (fa-complement fa-1))))
    (lisp-unit:assert-true (fa-empty-p (fa-intersection fa-2 (fa-complement fa-2))))
    (lisp-unit:assert-true (fa-empty-p (fa-intersection fa-i (fa-complement fa-i))))

    ;; pop-initial
    (lisp-unit:assert-true (fa-equiv (fa-pop-initial (regex->dfa '(:concatenation a b a b)) 'a)
                                     (regex->dfa '(:concatenation b a b))))

    (lisp-unit:assert-true (fa-equiv (fa-pop-initial (regex->dfa '(:concatenation (:closure a) b a b)) 'a)
                                     (regex->dfa '(:concatenation (:closure a) b a b))))
  )))


(lisp-unit:define-test fa-set
  (lisp-unit:assert-true (fa-subset-p (regex->dfa '(:closure (:union a )))
                                      (regex->dfa '(:closure (:union a b)))))
  (lisp-unit:assert-false (fa-subset-p (regex->dfa '(:closure (:union a c)))
                                       (regex->dfa '(:closure (:union a b)))))
  (lisp-unit:assert-true (fa-subset-p (regex->dfa '(:concatenation (:closure a) (:closure b)))
                                       (regex->dfa '(:closure (:union a b)))))
  (lisp-unit:assert-false (fa-subset-p (regex->dfa '(:concatenation (:closure a) (:closure c)))
                                       (regex->dfa '(:closure (:union a b)))))
  )

(lisp-unit:define-test grammar-basic
  (let ((g '((a b c) (b e f))))
    ;; map
    (lisp-unit:assert-true
     (equal (grammar-map 'list (lambda (l r) (list :a l r))
                         g)
            '((:a a (b c))
              (:a b (e f)))))
    ;; nonterminals
    (let ((n-r '(a b))
          (n (grammar-nonterminals g)))
      (lisp-unit:assert-true
       (finite-set-equal n n-r)))

    ;; substitute list
    (lisp-unit:assert-true
     '((a k b c) (c k b f))
     (grammar-substitute-terminal-list '((a b c) (c b f))
                                       'b '(k b)))

    ;; remove non-sentential
    (lisp-unit:assert-true
     (equal (grammar-remove-nonsentential '((s a b) (s 1) (a 1)) '(1))
            '((s 1)
              (a 1))))

    ;; Remove unreachables
    (lisp-unit:assert-true
     (equal (grammar-remove-unreachable '((a b) (b 1) (b 3) (c 2)))
            '((a b) (b 1) (b 3))))

    (lisp-unit:assert-true
     (equal (grammar-remove-unreachable '((a 1 c) (b 1) (b 3) (c 2)))
            '((a 1 c) (c 2))))

    ;; Remove useless
    (lisp-unit:assert-true
     (equal (grammar-remove-useless '((s a b) (s 1) (a 1)) '(1))
            '((s 1))))

    ;; first/follow function
    ;; Dragon p222
    (let* ((grammar '((E T E-p)
                      (E-p + T E-p)
                      (E-p :epsilon)
                      (T F T-p)
                      (T-p * F T-p)
                      (T-p :epsilon)
                      (F |(| E |)|)
                      (F id)))
           (first (grammar-first-function grammar))
           (follow (grammar-follow-function grammar))
           (table (make-predictive-table grammar)))
      ;; first
      (assert-finite-set-equal (funcall first '+)
                               (finite-set '+))
      (assert-finite-set-equal (funcall first '*)
                               (finite-set '*))
      (assert-finite-set-equal (funcall first '|)|)
                               (finite-set '|)|))
      (assert-finite-set-equal (funcall first '|(|)
                               (finite-set '|(|))
      (assert-finite-set-equal (funcall first 'id)
                               (finite-set 'id))

      (assert-finite-set-equal (funcall first 'F)
                               (finite-set '|(| 'id))
      (assert-finite-set-equal (funcall first 'T)
                               (finite-set '|(| 'id))
      (assert-finite-set-equal (funcall first 'E)
                               (finite-set '|(| 'id))

      (assert-finite-set-equal (funcall first 'E-p)
                               (finite-set '+ :epsilon))
      (assert-finite-set-equal (funcall first 'T-p)
                               (finite-set '* :epsilon))
      ;; follow
      (assert-finite-set-equal (funcall follow 'E)
                               (finite-set '|)| :$))
      (assert-finite-set-equal (funcall follow 'E-p)
                               (finite-set '|)| :$))

      (assert-finite-set-equal (funcall follow 'T)
                               (finite-set '+ '|)| :$))
      (assert-finite-set-equal (funcall follow 'T-p)
                               (finite-set '+ '|)| :$))

      (assert-finite-set-equal (funcall follow 'F)
                               (finite-set '+ '* '|)| :$))
      ;; predictive table
      (lisp-unit:assert-equal '(E T E-p) (funcall table 'E 'id))
      (lisp-unit:assert-equal nil (funcall table 'E-p 'id))
      (lisp-unit:assert-equal '(T F T-p) (funcall table 'T 'id))
      (lisp-unit:assert-equal nil (funcall table 'T-p 'id))
      (lisp-unit:assert-equal '(F id) (funcall table 'F 'id))

      (lisp-unit:assert-equal nil (funcall table 'E '+))
      (lisp-unit:assert-equal '(E-p + T E-p) (funcall table 'E-p '+))
      (lisp-unit:assert-equal nil (funcall table 'T '+))
      (lisp-unit:assert-equal '(T-p :epsilon) (funcall table 'T-p '+))
      (lisp-unit:assert-equal nil (funcall table 'F '+))

      (lisp-unit:assert-equal nil (funcall table 'E '*))
      (lisp-unit:assert-equal nil (funcall table 'E-p '*))
      (lisp-unit:assert-equal nil (funcall table 'T '*))
      (lisp-unit:assert-equal '(T-p * F T-p) (funcall table 'T-p '*))
      (lisp-unit:assert-equal nil (funcall table 'F '*))

      (lisp-unit:assert-equal nil (funcall table 'E '*))
      (lisp-unit:assert-equal nil (funcall table 'E-p '*))
      (lisp-unit:assert-equal nil (funcall table 'T '*))
      (lisp-unit:assert-equal '(T-p * F T-p) (funcall table 'T-p '*))
      (lisp-unit:assert-equal nil (funcall table 'F '*))

      (lisp-unit:assert-equal '(E T E-p) (funcall table 'E '|(|))
      (lisp-unit:assert-equal nil (funcall table 'E-p '|(|))
      (lisp-unit:assert-equal '(T F T-p) (funcall table 'T '|(|))
      (lisp-unit:assert-equal nil (funcall table 'E-p '|(|))
      (lisp-unit:assert-equal '(F |(| E |)|) (funcall table 'F '|(|))

      (lisp-unit:assert-equal nil (funcall table 'E '|)|))
      (lisp-unit:assert-equal '(E-p :epsilon) (funcall table 'E-p '|)|))
      (lisp-unit:assert-equal nil (funcall table 'T '|)|))
      (lisp-unit:assert-equal '(T-p :epsilon) (funcall table 'T-p '|)|))
      (lisp-unit:assert-equal nil (funcall table 'F '|)|))

      (lisp-unit:assert-equal nil (funcall table 'E :$))
      (lisp-unit:assert-equal '(E-p :epsilon) (funcall table 'E-p :$))
      (lisp-unit:assert-equal nil (funcall table 'T :$))
      (lisp-unit:assert-equal '(T-p :epsilon) (funcall table 'T-p :$))
      (lisp-unit:assert-equal nil (funcall table 'F :$)))

    ;; predictive table
    ;; Dragon p225, example 4.33
    (let* ((grammar  '((S |i| E |t| S S-p)
                       (S |a|)
                       (S-p |e| S)
                       (S-p :epsilon)
                       (E |b|)))
           (table (make-predictive-table grammar :duplicate-error-p nil)))
      (assert-finite-set-equal (finite-set '(S |a|))
                               (funcall table 's '|a|))
      (assert-finite-set-equal (finite-set)
                               (funcall table 's-p '|a|))
      (assert-finite-set-equal (finite-set)
                               (funcall table 'e '|a|))

      (assert-finite-set-equal (finite-set)
                               (funcall table 's '|b|))
      (assert-finite-set-equal (finite-set)
                               (funcall table 's-p '|b|))
      (assert-finite-set-equal (finite-set '(E |b|))
                               (funcall table 'e '|b|))

      (assert-finite-set-equal (finite-set)
                               (funcall table 's '|e|))
      (assert-finite-set-equal (finite-set '(S-p :epsilon) '(S-p |e| S))
                               (funcall table 's-p '|e|))
      (assert-finite-set-equal (finite-set)
                               (funcall table 'e '|e|))

      (assert-finite-set-equal (finite-set '(S |i| E |t| S S-p))
                               (funcall table 's '|i|))
      (assert-finite-set-equal (finite-set)
                               (funcall table 's-p '|i|))
      (assert-finite-set-equal (finite-set)
                               (funcall table 'e '|i|))

      (assert-finite-set-equal (finite-set)
                               (funcall table 's '|t|))
      (assert-finite-set-equal (finite-set)
                               (funcall table 's-p '|t|))
      (assert-finite-set-equal (finite-set)
                               (funcall table 'e '|t|))

      (assert-finite-set-equal (finite-set)
                               (funcall table 's :$))
      (assert-finite-set-equal (finite-set '(S-p :epsilon))
                               (funcall table 's-p :$))
      (assert-finite-set-equal (finite-set)
                               (funcall table 'e :$)))


    ;; chain rule
    (lisp-unit:assert-true (grammar-chain-rule-p '(1 2 3) '(a b c) '(a b)))
    (lisp-unit:assert-true (grammar-chain-rule-p '(1 2 3) '(a b c) '(c a)))
    (lisp-unit:assert-false (grammar-chain-rule-p '(1 2 3) '(a b c) '(a 1)))
    (lisp-unit:assert-false (grammar-chain-rule-p '(1 2 3) '(a b c) '(a)))
    (lisp-unit:assert-false (grammar-chain-rule-p '(1 2 3) '(a b c) '(a b c)))
    (lisp-unit:assert-false (grammar-chain-rule-p '(1 2 3) '(a b c) '(a 1 c)))))

(lisp-unit:define-test grammar-regular
  (let ((fa (make-fa '((0 x 1) (1 y 0) (1 z 2)) 0 (finite-set 2)))
        (gram-1 '((a x b) (b y a)  (b z)))
        (gram-2 '((a x b) (b y a) (b y x b) (b z))))
    (lisp-unit:assert-true
     (fa-equiv fa (grammar->fa  gram-1)))
    (lisp-unit:assert-true
     (fa-equiv fa (nfa->dfa (grammar->fa gram-2))))))


(lisp-unit:define-test grammar-norm
  (let ((sipser-2-10 '((s a s a) (s x b) (a b) (a s) (b y) (b)))
        (no-epsilon '((s a s a) (s x b) (s x) (s s a) (s a s) (s s) (a b) (a s) (b y)))
        (no-epsilon-unit '((s a s a) (s x b) (s x) (s s a) (s a s)
                           (a y) (a a s a) (a x b) (a x) (a s a) (a a s)
                           (b y))))
    (lisp-unit:assert-true
     (finite-set-equal no-epsilon
                       (grammar-remove-epsilon sipser-2-10)))
    (lisp-unit:assert-true
     (finite-set-equal no-epsilon-unit
                       (grammar-remove-unit (grammar-remove-epsilon sipser-2-10)))))
  ;; Blum-Koch
  (lisp-unit:assert-true
   (finite-set-equal (blum-koch-subgrammar-productions 'b '((b a 2) (a 1)))
                     (finite-set (list (gsymbol-gen 'b 'start) 1 (gsymbol-gen 'a 'b))
                                 (list (gsymbol-gen 'a 'b) 2))))

  (lisp-unit:assert-true
   (finite-set-equal (blum-koch-subgrammar-productions  'b
                                                        '((b d1 a1)
                                                          (d1 d2 a2)
                                                          (d2 d3 a3)
                                                          (d3 d4 a4)
                                                          (d4 a g)))
                     (finite-set (list (gsymbol-gen 'b 'start) 'a 'g (gsymbol-gen 'd4 'b))
                                 (list (gsymbol-gen 'd4 'b) 'a4 (gsymbol-gen 'd3 'b))
                                 (list (gsymbol-gen 'd3 'b) 'a3 (gsymbol-gen 'd2 'b))
                                 (list (gsymbol-gen 'd2 'b) 'a2 (gsymbol-gen 'd1 'b))
                                 (list (gsymbol-gen 'd1 'b) 'a1))))


  )

(lisp-unit:define-test partition-finite-set
  (assert-finite-set-equal '((2 8 5) (3 9 6) (1 10 7 4))
                           (partition-finite-set '(1 2 3 4 5 6 7 8 9 10)
                                                 (lambda (a b) (= (mod a 3) (mod b 3))))))

(lisp-unit:define-test grammar-left-factoring
  (labels
    ((test (input expected-output)
           (let ((actual-output (grammar-left-factor input nil)))
             ;;(format t "Got: ~S~%Exp: ~S~%~%" actual-output expected-output)
             (lisp-unit:assert-true (set-equal actual-output expected-output :test #'equal))
             (lisp-unit:assert-equal (grammar-start-nonterminal actual-output)
                                     (grammar-start-nonterminal expected-output)))))
    (let ((grammar-1  '((A a)))
          (grammar-1-res '((A a)))
          (grammar-2 '((A |a| B)
                       (A |a| C)
                       (B |b|)
                       (C |c|)))
          (grammar-2-res '((A |a| ((A |a|)))
                           (((A |a|)) B)
                           (((A |a|)) C)
                           (B |b|)
                           (C |c|)))
          (grammar-3 '((A |a| B)
                       (A |a| |a| C)
                       (A |a| |a| B)
                       (A |a| C)
                       (B |b|)
                       (C |c|)))
          (grammar-3-res '((A |a| ((A |a|)))
                           (((A |a|)) B)
                           (((A |a|)) C)
                           (((A |a|)) |a| ((((A |a|)) |a|)))
                           (((((A |a|)) |a|)) B)
                           (((((A |a|)) |a|)) C)
                           (B |b|)
                           (C |c|)))
          (grammar-4 '((S |i| E |t| S)
                       (S |i| E |t| S |e| |S|)
                       (S |a|)
                       (E |b|)))
          (grammar-4-res '((S |i| E |t| S ((S |i|)))
                           (S |a|)
                           (((S |i|)) |e| S)
                           (((S |i|)) :epsilon)
                           (E |b|))))

      (test grammar-1 grammar-1-res)
      (test grammar-2 grammar-2-res)
      (test grammar-3 grammar-3-res)
      (test grammar-4 grammar-4-res))))

(lisp-unit:define-test grammar-map-grouped
  (assert-finite-set-equal (grammar-map-grouped 'list (lambda (head bodys) (list head bodys)) '((A |b| |c|) (A |c| |d|) (B |e|)))
                           '((B ((|e|))) (A ((|c| |d|) (|b| |c|))))))

(lisp-unit:define-test codegen
    (let ((fa (make-fa '((a 0 b)
                         (b 1 a)
                         (b 2 c))
                       'a '(c)))
          (vector (vector 1 -1 -1
                          -1 0 2
                          -1 -1 -1)))
      (lisp-unit:assert-equalp (dfa-transition-vector fa)
                               vector)))


(lisp-unit:define-test search
  (let ((fa (regex->dfa '(:union 5 (:concatenation 1 2)))))
    (lisp-unit:assert-equal '(5) (fa-shortest-string fa))
    (lisp-unit:assert-equal '(1 2) (fa-optimal-string fa #'identity)))
  (let ((fa (regex->dfa '(:concatenation 1 2 3 (:closure 4) (:union 5 (:concatenation 6 7))))))
    (lisp-unit:assert-equal '(1 2 3 5) (fa-shortest-string fa))
    (lisp-unit:assert-equal '(1 2 3 5) (fa-optimal-string fa #'identity))))


(lisp-unit:define-test petri
  ;; Test Firings
  (lisp-unit:assert-equal
   '((p1 . 0) (p2 . 0) (p3 . 1))
   (gsymbol-sort (petri-net-fire (make-petri-net :places '(p1 p2  p3)
                                                 :transitions '(t0 t1)
                                                 :marking '((p1 . 1) (p2 . 1))
                                                 :arcs '((p1 t1) (p2 t1) (t1 p3)))
                                 't1)))
   (lisp-unit:assert-equal
    '((p1 . 2) (p2 . 2) (p3 . 0))
    (gsymbol-sort (petri-net-fire (make-petri-net :places '(p1 p2  p3)
                                                  :transitions '(t0 t1)
                                                  :marking '((p1 . 1) (p2 . 1))
                                                  :arcs '((nil t0) (t0 p1) (t0 p2) (p1 t1) (p2 t1) (t1 p3)))
                                  't0)))
   (lisp-unit:assert-equal
    '((p1 . 0) (p2 . 0) (p3 . 0))
    (gsymbol-sort (petri-net-fire (make-petri-net :places '(p1 p2  p3)
                                                  :transitions '(t0 t1 t2)
                                                  :marking '((p3 . 1))
                                                  :arcs '((nil t0) (t0 p1) (t0 p2) (p1 t1) (p2 t1) (t1 p3) (p3 t2) (t2 nil)))
                                  't2))))


(lisp-unit:define-test pattern
  ;; basic patterns
  (lisp-unit:assert-eq 'yes
                       (if-pattern (:pattern 1 :b)
                                   (list 1 :b)
                                   'yes
                                   'no))
  (lisp-unit:assert-eq 'no
                       (if-pattern (:pattern 1 :b)
                                   (list 1 2)
                                   'yes
                                   'no))
  (lisp-unit:assert-eq 2
                       (if-pattern (:pattern 1 b)
                                   (list 1 2)
                                   b
                                   'no))
  (lisp-unit:assert-eq 'no
                       (if-pattern (:pattern 1 b)
                                   (list 2 2)
                                   b
                                   'no))

  (lisp-unit:assert-false (if-pattern (:pattern 1) 'a 'a))

  (lisp-unit:assert-equal '(2 3)
                          (if-pattern (:pattern 1 (:pattern a b))
                                      (list 1 (list 2 3))
                                      (list a b)
                                      'no))
  (lisp-unit:assert-equal 'no
                          (if-pattern (:pattern 1 (:pattern a b))
                                      (list 1 (list 2))
                                      (list a b)
                                      'no))
  (lisp-unit:assert-equal 'no
                          (if-pattern (:pattern 1 (:pattern a b))
                                      (list 1 (list 2 3 4))
                                      (list a b)
                                      'no))
  (lisp-unit:assert-equal '(2 3 4)
                          (if-pattern (:pattern 1 b)
                                      (list 1 (list 2 3 4))
                                      b
                                      'no))
  (lisp-unit:assert-eq 1
                       (if-pattern (:pattern a a)
                                   (list 1 1)
                                   a
                                   'no))
  (lisp-unit:assert-eq 'no
                       (if-pattern (:pattern a a)
                                   (list 1 2)
                                   a
                                   'no))
   (lisp-unit:assert-eq 1
                        (if-pattern (:pattern a (:pattern 2 a))
                                    (list 1 (list 2 1))
                                    a
                                    'no))
   (lisp-unit:assert-eq 'no
                        (if-pattern (:pattern a (:pattern 2 a))
                                    (list 1 (list 2 3))
                                    a
                                    'no))

   (lisp-unit:assert-eq 'b
                        (pattern-case (list 1 2)
                          ((:pattern 2 1) 'a)
                          ((:pattern 1 2) 'b)))

   (lisp-unit:assert-eq 'a
                        (pattern-case (list 1 2)
                          ((:pattern 1 2) 'a)
                          ((:pattern 2 1) 'b)))
   (lisp-unit:assert-eq 'b
                        (pattern-case (list 1 2)
                          ((:pattern 1 3) 'a)
                          (t 'b)))

   ;; pattern case
   (lisp-unit:assert-true (pattern-case (list 1)
                            ((consp) t)))
   (lisp-unit:assert-false (pattern-case 1
                            ((consp) t)))
   (lisp-unit:assert-true (pattern-case 1
                            ((atom) t)))
   (lisp-unit:assert-false (pattern-case (list 1)
                            ((atom) t)))

   ;; or patterns
   (lisp-unit:assert-eql 1
                         (if-pattern  (or (:pattern b) (:pattern b b)) '(1) b 'no))
   (lisp-unit:assert-eql 2
                         (if-pattern  (or (:pattern b) (:pattern b b)) '(2 2) b 'no))
   (lisp-unit:assert-eql 'no
                         (if-pattern  (or (:pattern b) (:pattern b b)) '(1 2) b 'no))

   ;; lambdas
   (lisp-unit:assert-eql 5
                         (funcall (pattern-lambda ((:pattern a b a)) (+ a b b)) '(1 2 1)))
   (lisp-unit:assert-eql 5
                         (funcall (pattern-lambda (a b a) (+ a b b)) 1 2 1))

   (let ((myvar 100))
     (lisp-unit:assert-equal '(nil 99 nil 98)
                             (mapcar (pattern-lambda ((or (:pattern a b) (:pattern b a b)))
                                       (incf myvar)
                                       (- a b))
                                     '(hi (100 1) 6 (2 100 2))))
     (lisp-unit:assert-eql 102 myvar)
     )

   ;; and patterns
   ;(lisp-unit:assert-eql 5
   ;                      (if-pattern  (and a (:predicate #'oddp)) 5 a 'no))

   ;(lisp-unit:assert-eql 'no
   ;                      (if-pattern  (and a (:predicate #'evenp)) 5 a 'no))

   ;(lisp-unit:assert-eql 5
   ;                      (if-pattern  (and a a) 5 a 'no))

   ;(lisp-unit:assert-eql 5
   ;                      (if-pattern  (and a b) 5 a 'no))

   ;(lisp-unit:assert-eql 25
   ;                      (if-pattern  (and a b) 5 (* a b) 'no))

   ;(lisp-unit:assert-eql 5
   ;                      (if-pattern  (and a) 5 a 'no))

   ;(lisp-unit:assert-eql 11
   ;                      (if-pattern  (:pattern (and a (:predicate #'oddp)) b) '(5 6) (+ a b) 'no))

   ;(lisp-unit:assert-eql 'no
   ;                      (if-pattern  (and (:pattern a a) (:pattern b b)) '(5 6) (+ a b) 'no))

   ;(lisp-unit:assert-eql 11
   ;                      (if-pattern  (and (:pattern a b) (:pattern a b)) '(5 6) (+ a b) 'no))

   ;(lisp-unit:assert-eql 'no
   ;                      (if-pattern  (and (:pattern a b) (:pattern b a)) '(5 6) (+ a b) 'no))

   ;(lisp-unit:assert-eql 10
   ;                        (if-pattern  (and (:pattern a b) (:pattern b a)) '(5 5) (+ a b) 'no))
   )

(defun finite-fake-atnconf-set (configurations &optional (also-stack t))
  (let* ((fake-fun (lambda (atn-state-name) (make-atn-state atn-state-name 'fake 'fake 'fake)))
         (conf-convert (lambda (config)
                         (fill-atnconf (funcall fake-fun (atnconf-state config))
                                       (atnconf-alternative config)
                                       (if also-stack
                                         (mapcar fake-fun (atnconf-stack config))
                                         (atnconf-stack config))))))
    (fold #'finite-set-add (empty-atnconf-set) (mapcar conf-convert configurations))))

(defun assert-atnconf-equal (set expected)
  (assert-finite-set-equal set
                           (finite-set-map 'list (lambda (atnconf) (cons (caar atnconf) (cdr atnconf))) expected)))

(defun grammar ()
  '((A |a| |b|) (A C |b|) (C |d| C |f|) (C |a|)))

(defun atn ()
  (grammar->ATN (grammar)))

(defun test-closure (start-config tuples &optional (debug nil))
  "Expected is a list of tuples WITHOUT the alternative fixnum!"
  (let ((res (atn-closure (atn) (make-empty-dd) start-config))
        (expected (finite-fake-atnconf-set (mapcar (pattern-lambda ((:pattern x y)) (list x (atnconf-alternative start-config) y)) tuples))))
    (when debug (print res))
    (assert-finite-set-equal expected res)))

(lisp-unit:define-test ll-star-closure-C
  (test-closure (list (ATN-start-name 'C) 7 nil)
                '(("p_C" nil) ("p_7" nil) ("p_11" nil))))

(lisp-unit:define-test ll-star-closure-A
  (test-closure (list (ATN-start-name 'A) 5 nil)
                '(("p_A" nil) ("p_1" nil) ("p_4" nil) ("p_C" ("p_5")) ("p_7" ("p_5")) ("p_11" ("p_5")))))

(lisp-unit:define-test ll-star-closure-8
  (test-closure (list (ATN-numeric-name 8 'C 2) 5 nil)
                '(("p_8" nil) ("p_C" ("p_9")) ("p_7" ("p_9")) ("p_11" ("p_9")))))

(lisp-unit:define-test ll-star-closure-empty
  (test-closure (list (ATN-numeric-name 12 'C 3) 5 nil)
                '(("p_12" nil) ("p_C'" nil) ("p_9" nil) ("p_5" nil))))

(lisp-unit:define-test ll-star-closure
  ;; TODO: refactor like the other guys above
  ;; Slightly harder with different stack rule but still possible
  (let* ((grammar '((A |a| |b|) (A C |b|) (C |d| C |f|) (C |a|)))
         (atn (grammar->ATN grammar))

         ;; Test for undoing with nonempty stack
         (stackNe (list (ATN-numeric-name 9 'C 2) (ATN-numeric-name 5 'A 1)))
         (resNe (atn-closure atn (make-empty-dd) (list (ATN-numeric-name 12 'C 3) 5 stackNe))))
    ;(print resNe)
    (let ((s stackNe)) ;; s as in stack
      ;; Note, we don't need (or we shouldn't) fakeify the stack as it already is complete
      ;(print (finite-fake-atnconf-set `("p_12" 5 ,s) `("p_C'" 5 ,s) `("p_9" 5 ,(cdr s))))
      (assert-finite-set-equal (finite-fake-atnconf-set `(("p_12" 5 ,s) ("p_C'" 5 ,s) ("p_9" 5 ,(cdr s))) nil) resNe))))

(defun ds-only-configurations (configurations)
  (let ((ds (make-empty-dd)))
    (setf (dd-configurations ds) (finite-fake-atnconf-set configurations))
    ds))

(defun ds-only-configuration-states (states)
  (ds-only-configurations
    (loop for state in states
          for x below 123456789
          collect (list state x nil))))

(lisp-unit:define-test dd-unique-atn-states
  (labels ((helper (configs)
             (dd-unique-atn-states (ds-only-configurations configs)))
           (checker (expected actual)
             (lisp-unit:assert-true (and (= (finite-set-length expected) (finite-set-length actual))
                                         (finite-set-equal expected (finite-set-map 'list #'atn-state-name actual))))))
    (checker '("p_A" "a" "b")
             (helper '(("p_A" 3 nil) ("a" 1 nil) ("b" 3 nil))))

    (checker '("p_A" "a")
             (helper '(("p_A" 3 nil) ("a" 1 nil) ("p_A" 3 ("p_B")))))

    (checker '("p_A" "a")
             (helper '(("a" 3 nil) ("a" 1 nil) ("p_A" 3 ("p_B")))))
    )
  )

(lisp-unit:define-test atn-get-terminals
  (labels ((helper (grammar states)
             (atn-get-terminals (grammar->ATN grammar) #'terminal-string-p (ds-only-configuration-states states)))
           (checker (expected actual)
             (lisp-unit:assert-true (and (= (finite-set-length expected) (finite-set-length actual))
                                         (finite-set-equal expected actual)))))

    (let* ((grammar '((A |a| |b|) (A C |b|) (C |d| C |f|) (C |a|) (A |gg| |ll| |vv|))))

      (checker '()
        (helper grammar '("p_A")))

      (checker '(|gg|)
        (helper grammar '("p_13" "p_13")))

      (checker '(|a|)
        (helper grammar '("p_1" "p_11")))

      (checker '(|ll| |b| |f| |d|)
        (helper grammar '("p_14" "p_16" "p_3" "p_2" "p_8" "p_9" "p_7"))))))

(lisp-unit:define-test find-predicted-alternatives
  (labels ((helper (configs)
             (sort  (find-predicted-alternatives (ds-only-configurations configs)) #'<)))
    (lisp-unit:assert-equal '(1 3)
                            (helper '(("p_A" 3 nil) ("a" 1 nil) ("b" 3 nil))))

    (lisp-unit:assert-equal '(1)
                            (helper '(("p_A" 1 nil) ("a" 1 nil) ("b" 1 nil))))

    (lisp-unit:assert-equal '()
                            (helper '()))))

(lisp-unit:define-test LL-star-move
  (labels ((helper (configs terminal)
             (apply #'finite-tree-set #'atnconf-compare
                    (atn-move (grammar->ATN '((A |a| |b|) (A C |b|) (C |d| C |f|) (C |a|)))
                              (ds-only-configurations configs)
                              terminal)))
           (helper-expected (configs)
             (finite-fake-atnconf-set configs)))
    (assert-finite-set-equal (helper-expected '(("p_2" 3 nil)))
                             (helper '(("p_1" 3 nil)) '|a|))

    (assert-finite-set-equal (helper-expected '())
                             (helper '(("p_1" 3 nil)) '|b|))


    (assert-finite-set-equal (helper-expected '())
                             (helper '(("p_4" 3 nil)) '|C|)) ; XXX: Note illegal input just to show how it works

    (assert-finite-set-equal (helper-expected '(("p_2" 3 nil) ("p_12" 2 nil) ("p_2" 3 ("p_9"))))
                             (helper '(("p_1" 3 nil) ("p_11" 2 nil) ("p_1" 3 ("p_9")) ("p_3" 2 nil)) '|a|))))

(defun assert-fa-equivalent (expected actual &optional (mgmode nil))
  ;; TODO: It's SERIOUS issue that I've commented out that the propositions-equivalent.
  ;; I did it because the many many calls to the sat solver killed my machine
  (let* ((mgmode-comparator (lambda (e1 e2) (and (gsymbol-compare (first e1) (first e2))
                                                 ;(propositions-equivalent (second e1) (second e2))
                                                 (gsymbol-compare (third e1) (third e2)))))
         (comparator (if mgmode
                       (curry #'finite-set-equivalent mgmode-comparator)
                       #'finite-set-equal)))
    (lisp-unit:assert-true (dfa-equal expected actual comparator))))

(defun assert-create-dfa-errors (grammar nonterminal &optional (mgmode nil))
  (lisp-unit:assert-error 'simple-error (create-dfa grammar nonterminal mgmode)))

(lisp-unit:define-test ll-star-create-dfa
  ;; TODO: Also test that productions have the right id in the final states
  (let* ((my-grammar '((A |a| |b|) (A C |g|) (C |d| C |f|) (C |a|)))
         ;; This grammar is relatively easy because it's unambigious (though
         ;; not seen trivially) and there are no backloops
         (resA (create-dfa my-grammar 'A))
         (resC (create-dfa my-grammar 'C)))
    ;(fa-pdf (fa-rewrite-states #'atn-state-name (grammar->ATN my-grammar)))
    ;(fa-pdf resA :OUTPUT "/tmp/dfa.pdf")
    (assert-fa-equivalent (make-fa '((s0 |a| s1) (s0 |d| f1) (s1 |g| f1) (s1 |b| f0)) 's0 '(f0 f1)) resA)
    (assert-fa-equivalent (make-fa '((s0 |a| f3) (s0 |d| f2)) 's0 '(f2 f3)) resC))
  (let* ((my-grammar '((A L |b|) (A L |d|) (L) (L |a| L)))
         ;; This grammar is containing a loop, that is it's not LL(k) nor LR(k) for any k
         ;(resA (create-dfa my-grammar 'A))
         (resL (create-dfa my-grammar 'L)))
    ;(assert-fa-equivalent (make-fa '((s0 |a| s1) (s0 |b| f0) (s0 |d| f1)
    ;                                 (s1 |a| s1) (s1 |b| f0) (s1 |d| f1)) 's0 '(f0 f1)) resA)
    ;;; Actually, the fa above is not as good as it could be, but it's still ok.
    ;;
    ;; Whoups, that actually was LL-regular but the heuristic *should* throw an
    ;; error here! (see chapter in the LL* paper: "aborting DFA construction")
    (assert-create-dfa-errors my-grammar 'A)

    (assert-fa-equivalent (make-fa '((s0 |a| f3) (s0 |d| f2) (s0 |b| f2)) 's0 '(f2 f3)) resL))

  (let* ((my-grammar '((A |a| |b| |c| |d| |e| B |f|) (A |a| |b| |c| |d| B |f|) (B |e| |e| A) (B |e| |e| B) (B )))
         (resA (create-dfa my-grammar 'A))
         (resB (create-dfa my-grammar 'B)))
    (assert-fa-equivalent (make-fa '((s0 |a| s1) (s1 |b| s2) (s2 |c| s3) (s3 |d| s4)
                                     (s4 |e| s6) (s4 |f| f1)
                                     (s6 |e| s7) (s6 |f| f0)
                                     (s7 |e| s10) (s7 |a| f1) (s7 |f| f1)
                                     (s10 |e| s7) (s10 |a| f0) (s10 |f| f0)) 's0 '(f0 f1)) resA)
    (assert-fa-equivalent (make-fa '((s0 |f| f4) (s0 |e| s1)
                                     (s1 |e| s2)
                                     (s2 |a| f2) (s2 |e| f3) (s2 |f| f3)) 's0 '(f2 f3 f4)) resB)

    )

  (assert-create-dfa-errors '((A |a| |b| |c|) (A |a| |b|)) 'A) ;; This grammar should error becuase we can't determine the production
  (assert-create-dfa-errors '((A |a| C |b|) (A |a| C |c|) (C |d| C |e|) (C )) 'A) ;; This grammar isn't LL-regular (but LR(0) I think)
  ;; TODO add test for recursion overflow, but I wanna wait until it's parametrized.
  )

(defun fa-make-fa-print (fa &optional (pp t))
  (let ((*print-pretty* pp)) (format t "~%(make-fa '~A '~A '~A)~%"
                             (fa-map-edge-lists 'list #'identity fa)
                             (fa-start fa)
                             (finite-set-map 'list #'identity (fa-accept fa)))))

(lisp-unit:define-test ll-star-create-dfa-mgmode
  ;; TODO: Test that productions have the right id in the final states
  ;; TODO: There is one issue with these tests, two edges like (A) vs (NOT (NOT
  ;; A)) are really equivalent. But currently we just check strict equality, so
  ;; if our prop->simplify method improves, ours tests will start to fail
  (let* ((my-grammar '((A (pred is-warm) (mu cool-down) A) (A (pred (not is-warm)) (mu heat-up) A)))
         ;; Note that we use (not is-warm) rather than (is-warm), as then this can be proven to be semantically LL(1)
         (resA (create-dfa my-grammar 'A t)))
    (assert-fa-equivalent (make-fa '((s0 IS-WARM f0) (s0 (and (NOT IS-WARM) (NOT IS-WARM)) f1)) 's0 '(f0 f1)) resA) t)

  (assert-create-dfa-errors '((A (pred is-warm) (mu cool-down) A)
                              (A (pred is-cold) (mu heat-up) A)) 'A t) ;; Not semantically LL(1)
  (assert-create-dfa-errors '((A (pred is-warm) (mu cool-down) A)
                              (A (pred (not is-warm)) (mu heat-up) A)
                              (A (pred (or is-warm (not is-warm))) (mu finale))) 'A t) ;; Not semantically LL(1)

  (let* ((my-grammar '((A (mu cool-down) (pred is-warm)) (A (mu cool-down) (pred (not is-warm)))))
         ;; Here we check if mutators also appear in the dfa
         (resA (create-dfa my-grammar 'A t)))
    ;(fa-make-fa-print resA)
    (assert-fa-equivalent (make-fa '((s0 (MU cool-down) s1) (s1 IS-WARM f0) (s1 (and (NOT IS-WARM) (NOT IS-WARM)) f1)) 's0 '(f0 f1)) resA) t)

  ;; This test is gonna be kinda ugly so I have to add some helpers
  ;; We should see that the splitting is working as it should
  (labels ((ot1 (s) (if (gsymbol-equal s 'a) 'b 'a)) ;; other 1
           (ot2 (s) (if (gsymbol-equal s 'c) 'b 'c)) ;; other 2
           (ndef (s) `(and (not ,s ) (or (or d e) f))) ;; (ndef 'd) ==> !d and (e or f)
           (nabc (s) `(and (and (not ,s) ,(ot1 s)) ,(ot2 s))) ;; (nabc 'a) ==> !a and b and c
           )
    (let* ((mu '(MU someeffect))
           (my-grammar `((A (pred (and a ,(ndef 'd))) ,mu (pred ,(nabc 'a)))
                         (A (pred (and b ,(ndef 'e))) ,mu (pred ,(nabc 'b)))
                         (A (pred (and c ,(ndef 'f))) ,mu (pred ,(nabc 'c)))
                         ))
           (resA (create-dfa my-grammar 'A t))
           (ansA (make-fa '((s9 (AND (NOT (AND (AND (NOT C) A) B)) (AND (AND (NOT B) A) C))
                                final=>1)
                            (s9 (AND (AND (AND (NOT C) A) B) (NOT (AND (AND (NOT B) A) C)))
                                final=>2)
                            (s8 (AND (NOT (AND (AND (NOT C) A) B)) (AND (AND (NOT A) B) C))
                                final=>0)
                            (s8 (AND (AND (AND (NOT C) A) B) (NOT (AND (AND (NOT A) B) C)))
                                final=>2)
                            (s7 (AND (NOT (AND (AND (NOT A) B) C)) (AND (AND (NOT B) A) C))
                                final=>1)
                            (s7 (AND (AND (AND (NOT A) B) C) (NOT (AND (AND (NOT B) A) C)))
                                final=>0)
                            (s6 (MU SOMEEFFECT) s9) (s5 (MU SOMEEFFECT) s8)
                            (s3 (MU SOMEEFFECT) s7)
                            (s0
                              (AND
                                (AND (NOT (AND A (AND (NOT D) (OR (OR D E) F))))
                                     (AND B (AND (NOT E) (OR (OR D E) F))))
                                (AND C (AND (NOT F) (OR (OR D E) F))))
                              s6)
                            (s0
                              (AND
                                (AND (AND A (AND (NOT D) (OR (OR D E) F)))
                                     (NOT (AND B (AND (NOT E) (OR (OR D E) F)))))
                                (AND C (AND (NOT F) (OR (OR D E) F))))
                              s5)
                            (s0
                              (AND
                                (AND (NOT (AND A (AND (NOT D) (OR (OR D E) F))))
                                     (NOT (AND B (AND (NOT E) (OR (OR D E) F)))))
                                (AND C (AND (NOT F) (OR (OR D E) F))))
                              final=>2)
                            (s0
                              (AND
                                (AND (AND A (AND (NOT D) (OR (OR D E) F)))
                                     (AND B (AND (NOT E) (OR (OR D E) F))))
                                (NOT (AND C (AND (NOT F) (OR (OR D E) F)))))
                              s3)
                            (s0
                              (AND
                                (AND (NOT (AND A (AND (NOT D) (OR (OR D E) F))))
                                     (AND B (AND (NOT E) (OR (OR D E) F))))
                                (NOT (AND C (AND (NOT F) (OR (OR D E) F)))))
                              final=>1)
                            (s0
                              (AND
                                (AND (AND A (AND (NOT D) (OR (OR D E) F)))
                                     (NOT (AND B (AND (NOT E) (OR (OR D E) F)))))
                                (NOT (AND C (AND (NOT F) (OR (OR D E) F)))))
                              final=>0)) 's0 '(final=>0 final=>1 final=>2))
                 ))

      (assert-fa-equivalent ansA resA t) ;; Sufficient but not necessary!
      ;(fa-pdf resA :OUTPUT "/tmp/dfa.pdf")

      (assert-create-dfa-errors `((A (pred (and a ,(ndef 'd))) ,mu (pred ,(nabc 'a)))
                                  (A (pred (and b ,(ndef 'e))) (MU othereffect) (pred ,(nabc 'b))) ;; Not semantically LL(1)
                                  (A (pred (and c ,(ndef 'f))) ,mu (pred ,(nabc 'c)))
                                  ) 'A t)))

  ;; TODO: These tests below are correct but we must fix that they don't fail
  ;; if you change the propositions to something that is equivalent
  ;(let* ((my-grammar '((A (pred hungry)       (mu hunt) EAT (mu sleep))
  ;                     (A (pred enjoy-hunting) (mu hunt) EXAMRESULT (mu not-sleep))
  ;                     (EAT (pred helper) (pred hunt-went-ok) (mu grill) (pred (not abc)) (mu eat))
  ;                     (EAT (pred (not helper)) (pred (and (not you-are-happy-anyway) (not hunt-went-ok))) (mu get-pissed) (mu chillax) (mu chill-more))
  ;                     (EXAMRESULT (pred helper) (pred success) (mu grill) (pred abc) (mu abc))
  ;                     (EXAMRESULT (pred (not helper)) (pred you-are-happy-anyway) (mu grill) (pred (not abc)) (mu def))))
  ;       (resA (create-dfa my-grammar 'A t))
  ;       (ansA (make-fa '((s12 ABC final=>1) (s12 (AND (NOT ABC) (NOT ABC)) final=>0)
  ;         (s9 (MU GRILL) s12)
  ;         (s6
  ;          (AND (OR YOU-ARE-HAPPY-ANYWAY HUNT-WENT-OK)
  ;               YOU-ARE-HAPPY-ANYWAY)
  ;          final=>1)
  ;         (s6
  ;          (AND (AND (NOT YOU-ARE-HAPPY-ANYWAY) (NOT HUNT-WENT-OK))
  ;               (NOT YOU-ARE-HAPPY-ANYWAY))
  ;          final=>0)
  ;         (s5 (AND HUNT-WENT-OK SUCCESS) s9)
  ;         (s5 (AND (NOT HUNT-WENT-OK) SUCCESS) final=>1)
  ;         (s5 (AND HUNT-WENT-OK (NOT SUCCESS)) final=>0)
  ;         (s4 (AND (NOT HELPER) (NOT HELPER)) s6) (s4 HELPER s5)
  ;         (s3 (MU HUNT) s4) (s0 (AND HUNGRY ENJOY-HUNTING) s3)
  ;         (s0 (AND (NOT HUNGRY) ENJOY-HUNTING) final=>1)
  ;         (s0 (AND HUNGRY (NOT ENJOY-HUNTING)) final=>0)) 's0 '(final=>0
  ;                                                               final=>1)))
  ;       )
  ;  ;; Multiple nonterminals. Checking many steps here to see that the semantically LL(1) condition seem to work
  ;  ;(fa-pdf (fa-rewrite-states #'atn-state-name (atn-fa (grammar->ATN my-grammar))))
  ;  ;(fa-pdf resA :OUTPUT "/tmp/dfa.pdf")
  ;  ;(fa-pdf (fa-rewrite-states #'atn-state-name (atn-fa (grammar->ATN my-grammar)))  :OUTPUT "/tmp/dotANSA.pdf")
  ;  ;(fa-pdf ansA :OUTPUT "/tmp/dfaANSA.pdf")
  ;  ;(fa-make-fa-print ansA)
  ;  (assert-fa-equivalent ansA resA t)
  ;  )

  ;(let* ((my-grammar '((B (pred a) (pred a) B)
  ;                     (B )
  ;                     (A (pred a) B (pred (not a)))
  ;                     (A B (pred (not a)))))
  ;       ;; Not LL(k) (nor LR(k) I believe)
  ;       (resA (create-dfa my-grammar 'A t))
  ;       (ansA (make-fa '((s6 (AND (NOT A) (NOT A)) final=>2) (s6 A s3) (s3 A s6)
  ;         (s3 (AND (NOT A) (NOT A)) final=>3)
  ;         (s2 (AND (NOT A) (NOT A)) final=>2) (s2 A s3) (s0 A s2)
  ;         (s0 (AND (NOT A) (NOT A)) final=>3)) 's0 '(final=>2 final=>3)))
  ;       )
  ;  ;(fa-make-fa-print resA)
  ;  ;(fa-pdf (fa-rewrite-states #'atn-state-name (grammar->ATN my-grammar)))
  ;  ;(fa-pdf resA :OUTPUT "/tmp/dfa.pdf")
  ;  (assert-fa-equivalent ansA resA t)
  ;  )
  )
