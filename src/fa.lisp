;;;; -*- Lisp -*-
;;;;
;;;; Copyright (c) 2011-2012, Georgia Tech Research Corporation
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

(in-package :motion-grammar)

;;; FA
;;;  -(states, tokens, edges, start, accept),
;;;    - edges: (list state-0 token state-1)

;;; Representation opptions
;;; - transition matrix
;;; - transition hash table
;;; - edge list
;;; - successor array


(defstruct (finite-automaton)
  "A Finite Automaton."
  states
  terminals
  edges
  start
  accept)

(defstruct (fa (:constructor %make-fa)
               (:include finite-automaton)))

(defun fa-map-edge-lists (result-type function fa)
  (map result-type function (fa-edges fa)))

(defun fa-map-edges (result-type function fa)
  "Apply FUNCTION to each edge in FA.
FUNCTION: (lambda (q-0 z q-1))"
  (map result-type (curry #'apply function)
       (fa-edges fa)))

(defun fold-fa-edges (function initial-value fa)
  (fold (lambda (v e) (apply function v e))
        initial-value (fa-edges fa)))

(defmacro do-fa-edges ((q0 z q1) fa &body body)
  `(loop for (,q0 ,z ,q1) in (fa-edges ,fa)
      do ,@body))


(defun make-fa-1 (states terminals edges start accept)
  (%make-fa :states (finite-set-tree states)
            :terminals (finite-set-tree terminals)
            :edges edges
            :start start
            :accept (finite-set-tree accept)))


(defun make-fa (edges start accept)
  "Create a finite-automaton.
EDGES: List of edges, each (list state-0 terminal state-1).
START: The automaton start state.
ACCEPT: Set of automaton accept states."
  (declare (type finite-set accept))
  (let ((state-set (make-finite-set :mutable t))     ; faster to build up the hash-table first
        (terminal-set (make-finite-set :mutable t))) ; then convert to the tree-set
    ;; edge states
    (loop for (q0 z q1) in edges
       do
         (setq state-set (finite-set-nadd (finite-set-nadd state-set q0)
                                          q1)
               terminal-set (finite-set-nadd terminal-set z)))
    ;; start state
    (make-fa-1 (finite-set-nadd state-set start) ; start state
               terminal-set edges start accept)))

    ;; (%make-fa :states (finite-set-tree state-set)
    ;;           :terminals (finite-set-tree terminal-set)
    ;;           :edges edges
    ;;           :start start
    ;;           :accept (finite-set-tree accept))))


;;;;;;;;;;;;;;
;; INDEXING ;;
;;;;;;;;;;;;;;

(defun dfa-mover (fa)
  (let ((hash (fold-fa-edges (lambda (hash q0 z q1)
                               (assert (not (eq z :epsilon)))
                               (assert q1)
                               (let ((k (list q0 z)))
                                 (assert (null (gethash k hash)))
                                 (setf (gethash k hash) q1))
                               hash)
                             (make-hash-table :test #'equal)
                             fa)))
    (lambda (q0 z) (gethash (list q0 z) hash))))

(defun nfa-mover (fa)
  (let ((hash (fold-fa-edges (lambda (hash q0 z q1)
                               (assert q1)
                               (let ((k (list q0 z)))
                                 (push q1 (gethash k hash)))
                               hash)
                             (make-hash-table :test #'equal)
                             fa)))
    (lambda (q0 z) (gethash (list q0 z) hash))))

(defun nfa-reverse-mover (nfa)
  (let ((hash (fold-fa-edges (lambda (hash q0 z q1)
                               (assert q1)
                               (let ((k (list q1 z)))
                                 (setf (gethash k hash)
                                       (finite-set-add (or (gethash k hash)
                                                           (make-finite-set :compare #'gsymbol-compare))
                                                       q0)))
                               hash)
                             (make-hash-table :test #'equal)
                             nfa)))
    (lambda (q1 z) (gethash (list q1 z) hash))))

(defun fa-outgoing-terminal-function (fa)
  "Index fa state to terminals leaving that state.
RESULT: (lambda (state)) => (finite-set terminals)"
  (let ((hash (make-hash-table :test #'equal)))
    (fa-map-edges nil (lambda (q z p)
                        (declare (ignore p))
                        (setf (gethash q hash)
                              (finite-set-add (or (gethash q hash)
                                                  (make-finite-set :compare #'gsymbol-compare))
                                              z)))
                  fa)
    (lambda (q) (gethash q hash))))

(defun fa-incoming-terminal-function (fa)
  "Index fa state to terminals leaving that state.
RESULT: (lambda (state)) => (finite-set terminals)"
  (let ((hash (make-hash-table :test #'equal)))
    (fa-map-edges nil (lambda (q z p)
                        (declare (ignore q))
                        (setf (gethash p hash)
                              (finite-set-add (gethash p hash) z)))
                  fa)
    (lambda (q) (gethash q hash))))


(defun fa-successors (fa)
  "Map from original-state to (list (list nonterminal resultant-state))."
  (let ((hash (make-hash-table :test #'equal)))
    (loop for e in (fa-edges fa)
       for q0 = (car e)
       for z-q1 = (cdr e)
       do (push z-q1 (gethash q0 hash)))
    (lambda (q0) (gethash q0 hash))))


;;;;;;;;;;;;;;;;;
;; CONVERSIONS ;;
;;;;;;;;;;;;;;;;;

(defun dfa-p (fa)
  "True if FA is deterministic"
  (labels ((rec (set edges)
             (if edges
                 (destructuring-bind ((q0 z q1) &rest edges) edges
                   (declare (ignore q1))
                   (let ((k (list q0 z)))
                     (unless (or (eq z :epsilon )
                                 (finite-set-inp k set))
                       (rec (finite-set-nadd set k) edges))))
                 t)))
    (rec (make-finite-set :mutable t) (fa-edges fa))))



(defun dfa-renumber (dfa)
  (let ((hash (make-hash-table :test #'equal))
        (succ (fa-successors dfa)))
    ;; label states
    (let ((count -1))
      (declare (type fixnum count))
      (labels ((visit (q)
                 (unless (gethash q hash)
                   (setf (gethash q hash) (incf count))
                   (do-finite-set (rest (sort-finite-set (funcall succ q)))
                     (destructuring-bind (z q1) rest
                       (declare (ignore z))
                       (visit q1))))))
        (visit (fa-start dfa)))
      ;; build
      (%make-fa :states  (let ((s (make-finite-set :compare #'gsymbol-compare)))
                           (dotimes (i (1+ count))
                             (setq s (finite-set-add s i)))
                           s)
                ;;(loop for i from 0 to count collect i)
                :terminals (sort-finite-set (fa-terminals dfa))
                :edges (gsymbol-nsort (loop for (q0 z q1) in (fa-edges dfa)
                                         collect (list (gethash q0 hash) z (gethash q1 hash))))
                :start 0
                :accept (fold-finite-set (lambda (set x) (finite-set-add set (gethash x hash)))
                                         (make-finite-set :compare #'gsymbol-compare)
                                         (fa-accept dfa))))))

(defun fa-reverse (fa &optional (unique (gensym)))
  (let ((new-start (gsymbol-gen "start" unique)))
    (make-fa-1 (finite-set-add (fa-states fa) new-start)
               (finite-set-add (fa-terminals fa) :epsilon)
               (append (finite-set-map 'list (lambda (a) (list new-start :epsilon a)) (fa-accept fa))
                       (map 'list #'reverse (fa-edges fa)))
               new-start
               (finite-set (fa-start fa)))))


(defun fa-canonicalize (fa)
  "Return a canonical representation of FA.

Minimize the state of FA and rename state variables."
  ;; Hoprcroft's actually does go faster
  (dfa-renumber (fa-minimize-hopcroft fa)))
  ;;(dfa-renumber (fa-minimize-brzozowski fa)))


(defun fa-canonicalize-brzozowski (fa)
  (dfa-renumber (fa-minimize-brzozowski fa)))

(defun fa-canonicalize-hopcroft (fa)
  (dfa-renumber (fa-minimize-hopcroft fa)))

;;; See Aho, 2nd p. 153-154. These closure computations are a
;;; functional variation thereof.

(defun nfa-e-closure (states mover
                      &optional closure); (closure (make-finite-set :mutable t)))
  "epsilon-closure of list STATES.
STATES: list of states
MOVER: fuction from (state-0 token) => (list state-1-0 state-1-1...)"
  (do-finite-set (q states)
    (unless (finite-set-inp q closure)
      (setq closure
            (nfa-e-closure (funcall mover q :epsilon)
                           mover
                           (finite-set-nadd closure q)))))
  closure)

(defun nfa-move-e-closure (states z mover &optional
                           (closure (make-finite-set :mutable t)))
  "epsilon-closure of list STATES transitioned by token Z.
STATES: list of states
Z: token
MOVER: fuction from (state-0 token) => (list state-1-0 state-1-1...)"
  (do-finite-set (q0 states)
    (nfa-e-closure (funcall mover q0 z) mover closure))
  closure)

(defun nfa-start-closure (nfa &optional (mover (nfa-mover nfa)))
  (let ((start-0 (nfa-e-closure (list (fa-start nfa)) mover)))
    (sort (if (or (null (cdr start-0))
                  (some (lambda (e)
                          (destructuring-bind (q0 z q1) e
                            (or (and (equal q0 (fa-start nfa))
                                     (not (eq :epsilon z)))
                                (and (equal q1 (fa-start nfa))
                                     (not (finite-set-inp q0 start-0))))))
                        (fa-edges nfa)))
              (finite-set-list start-0)
              (finite-set-remove start-0 (fa-start nfa)))
          #'gsymbol-predicate)))



;; See Aho, 2nd p. 152
(defun nfa->dfa (nfa)
  "Convert an NFA to a DFA"
  (let* ((mover (nfa-mover nfa))
         (hash (make-hash-table :test #'equal))
         (closure-set (make-finite-set :mutable t)) ;; reuse this object
         ;; eliminate useless NFA start states
         (start (nfa-start-closure nfa mover))
         (terminals (finite-set-remove (fa-terminals nfa) :epsilon))
         edges)
    ;; subset construction
    (labels ((subset (q)
               (unless (gethash q hash)
                 (setf (gethash q hash) t)
                 (do-finite-set (z terminals)
                   (let ((u (gsymbol-sort (finite-set-list (nfa-move-e-closure q z
                                                                               mover
                                                                               (clrhash closure-set))))))
                     (when u
                       ;;(assert (null (finite-set-difference u (nfa-e-closure u mover))))
                       (push (list q z u) edges)
                       (subset u)))))))
      (subset start))
    ;; result
    (let ((fa (%make-fa :states (finite-set-tree hash)
                        :terminals (finite-set-remove (finite-set-tree (fa-terminals nfa)) :epsilon)
                        :edges edges
                        :start start
                        :accept (finite-set-tree
                                 (loop
                                    with a-list = (finite-set-list (fa-accept nfa))
                                    for q being the hash-keys of hash
                                    when (finite-set-intersection a-list q)
                                    collect (finite-set-list q))))))
      ;(assert (dfa-p fa))
      fa)))


(defun ensure-dfa (fa)
  "If FA is not a DFA, convert it to a DFA."
  (if (dfa-p fa)
      fa
      (nfa->dfa fa)))

(defmacro with-dfa ((var fa) &body body)
  `(let ((,var (ensure-dfa ,fa)))
     ,@body))

;; Brzozowski's Algorithm
(defun fa-minimize-brzozowski (dfa)
  "Minimize a DFA or NFA via Brzozowski's Algorithm."
  (chain dfa
         #'fa-reverse #'nfa->dfa
         #'fa-reverse #'nfa->dfa))


(defun fa-prune (fa)
  "Remove unreachable and dead states from the FA."
  ;; index reachable states
  (let ((succ (fold-fa-edges (lambda (hash q0 v q1)
                               (declare (ignore v))
                               (setf (gethash q0 hash)
                                     (finite-set-add (or (gethash q0 hash)
                                                         (make-finite-set :compare #'gsymbol-compare))
                                                     q1))
                               hash)
                             (make-hash-table :test #'equal)
                             fa))
        (pred (fold-fa-edges (lambda (hash q0 v q1)
                               (declare (ignore v))
                               (setf (gethash q1 hash)
                                     (finite-set-add (or (gethash q1 hash)
                                                         (make-finite-set :compare #'gsymbol-compare))
                                                     q0))
                               hash)
                             (make-hash-table :test #'equal)
                             fa))
        (live-fwd (make-hash-table :test #'equal))
        (live-rev (make-hash-table :test #'equal)))
    ;; mark forward live states
    (labels ((visit (q)
               (unless (gethash q live-fwd)
                 (setf (gethash q live-fwd) t)
                 (finite-set-map nil #'visit (gethash q succ)))))
      (visit (fa-start fa)))
    ;; mark reverse live states
    (labels ((visit (q)
               (unless (gethash q live-rev)
                 (setf (gethash q live-rev) t)
                 (finite-set-map nil #'visit (gethash q pred)))))
      (finite-set-map nil #'visit (fa-accept fa)))
    (let ((live (fold-finite-set (lambda (live q)
                                   (if (and (gethash q live-fwd)
                                            (gethash q live-rev))
                                       (finite-set-add live q)
                                       live))
                                 (make-finite-set :compare #'gsymbol-compare)
                                 (fa-states fa))))
      (if (finite-set-inp (fa-start fa) live)
          (%make-fa :states (fold-finite-set (lambda (set x)
                                               (if (finite-set-inp x live-rev)
                                                   (finite-set-add set x)
                                                   set))
                                             (make-finite-set :compare #'gsymbol-compare) live-fwd)
                    :terminals (fa-terminals fa)
                    :edges (loop for (q0 z q1) in (fa-edges fa)
                              when (and (finite-set-inp q0 live)
                                        (finite-set-inp q1 live))
                              collect (list q0 z q1))
                    :start (fa-start fa)
                    :accept (finite-set-intersection live (fa-accept fa)))
          (%make-fa :terminals (fa-terminals fa)
                    :states '(0)
                    :edges nil
                    :start 0
                    :accept nil)))))

(defun dfa-add-reject (dfa &optional (reject (gensym "reject")))
  "Add explicit reject state to the dfa."
  (let ((mover (dfa-mover dfa))
        (edges (fa-edges dfa)))
    (do-finite-set (z (fa-terminals dfa))
      (push (list reject z reject) edges)
      (do-finite-set (q (fa-states dfa))
        (unless (funcall mover q z)
          (push (list q z reject)
                edges))))
    (make-fa edges (fa-start dfa) (fa-accept dfa))))



;;; Regex
;;;  Parse Tree S-Expression
;;;    - :concatenation  --- ab
;;;    - :closure        --- a*
;;;    - :union          --- a|b

(defun fa-hopcroft-create (dfa p q-reject)
  ;;(print p)
  ;;(print (fa-terminals dfa))
  (assert (= (finite-set-length (fa-states dfa))
             (loop for part in p summing (finite-set-length part)))
          () "Partition states don't sum to initial states")
  (assert (finite-set-equal  (fold #'finite-set-union nil p)
                             (fa-states dfa)))
  (let* ((p (map 'list #'finite-set-list p))
         (state-hash (let ((hash (make-hash-table :test #'equal)))
                       (dolist (e p)
                         (dolist (q0 e)
                           (setf (gethash q0 hash) e)))
                       hash))
         (edge-hash (fold (lambda (hash edge)
                            (destructuring-bind (q0 z q1) edge
                              (unless (eq q1 q-reject)
                                (setf (gethash (list (gethash q0 state-hash)
                                                     z
                                                     (gethash q1 state-hash))
                                               hash)
                                      t)))
                            hash)
                          (make-hash-table :test #'equal)
                          (fa-edges dfa))))
    (fa-prune (%make-fa :states (finite-set-tree p)
                        :terminals (fa-terminals dfa)
                        :edges (finite-set-list edge-hash)
                        :start (gethash (fa-start dfa) state-hash)
                        :accept (fold-finite-set (lambda (accept q)
                                                   (finite-set-add accept (gethash q state-hash)))
                                                 (make-finite-set :compare #'gsymbol-compare)
                                                 (fa-accept dfa))))))

(defun fa-hopcroft-partition (dfa)
  (assert (dfa-p dfa))
  (let ((p (list  nil
                  (fa-accept dfa)
                  (finite-set-difference (fa-states dfa)
                                         (fa-accept dfa))))
        (p-single))
    ;; build minimal states
    ;; Note: CLISP 2.48 seemingly can't handle LOOP here
    (do ((q (make-finite-set :compare #'gsymbol-compare))
         (imover (nfa-reverse-mover dfa))
         (a (finite-set-min-set (second p) (third p))
            (multiple-value-bind (q1 a1) (tree-set-remove-min q)
              (setq q q1) a1)))
        ((null a))
      (do-finite-set (c (fa-terminals dfa))
        ;; x: predecessors of a for token c
        (let ((x (fold-finite-set (lambda (x q)
                                    (finite-set-union x (funcall imover q c)))
                                  nil a)))
          (when x
            (do* ((yyy p)
                  (yy (cdr yyy) (cdr yyy)))
                 ((null yy))
              (let ((y (car yy)))
                (if (finite-set-single-p y)
                    (progn ;; optmization: stop checking singleton partitions
                      (push y p-single)
                      (rplacd yyy (cdr yy)))
                    (let ((i (finite-set-intersection y x)))
                      (unless (finite-set-empty-p i)
                        (let ((j (finite-set-difference y x)))
                          (unless (finite-set-empty-p j) ;; two new partitions
                            ;;(format t "~&~%i: ~A~&j: ~A~&" i j)
                            ;; insert partitions into q
                            (if (finite-set-inp y q)
                                ;; add both
                                (setq q
                                      (tree-set-insert (tree-set-insert (tree-set-remove q y) i) j))
                                ;; add smaller
                                (setq q (tree-set-insert q (finite-set-min-set i j))))
                            ;; insert partitions into p, right here, destructively
                            (rplaca yy i)
                            (rplacd yy (cons j (cdr yy))))))
                      ;; Increment loop
                      (setq yyy (cdr yyy))))))))))
    (dolist (p0 p-single)
      (assert (finite-set-single-p p0)))
    (setq p (nconc p-single (cdr p)))
    p))

;; Hopcroft's Algorithm
(defun fa-minimize-hopcroft (fa)
  "Minimize a DFA via Hopcroft's's Algorithm."
  ;; TODO: more efficient representation of rejecting state
  ;;       make implicit somehow
  ;(declare (optimize (speed 3) (safety 0)))
  (let* ((reject (gensym "reject"))
         (dfa (dfa-add-reject (fa-prune (if (dfa-p fa)
                                            fa
                                            (nfa->dfa fa)))
                              reject)))
    (let ((p (fa-hopcroft-partition dfa)))
      (fa-hopcroft-create dfa p reject))))


(defun dfa->string-matcher (dfa)
  "Return a (lambda (string)) predicate to test if dfa matches string."
  (let ((mover (dfa-mover dfa))
        (start (fa-start dfa))
        (accept (fa-accept dfa)))
    (lambda (string)
      (finite-set-inp (fold (lambda (state x)
                              (when state
                                (funcall mover state x)))
                            start string)
       accept))))


(defun fa-state->edge (fa &optional (unique (gensym)))
  (let ((incoming (make-hash-table :test #'equal))
        (outgoing (make-hash-table :test #'equal)))
    ;; q => (q0 q q1)
    (let ((state-edges (finite-set-map 'list (lambda (q)
                                               (let ((in (gsymbol-gen (cons q 0) unique))
                                                     (out (gsymbol-gen (cons q 1) unique)))
                                                 (setf (gethash q incoming) in
                                                       (gethash q outgoing) out)
                                                 (list in q out)))
                                       (fa-states fa))))
      ;; make
      (make-fa (nconc state-edges (loop for (q0 z q1) in (fa-edges fa)
                                     collect (list (gethash q0 outgoing)
                                                   z
                                                   (gethash q1 incoming))))
               (gethash (fa-start fa) incoming)
               (finite-set-map 'list (curry-right #'gethash outgoing) (fa-accept fa))))))

(defun graph->fa (adj &key (start (car adj))  directed)
  (let ((incoming (make-hash-table :test #'equal))
        (outgoing (make-hash-table :test #'equal))
        (places (make-hash-table :test #'equal))
        (accept (make-hash-table :test #'equal)))
    ;; index incoming, outgoing, and places
    (dolist (e adj)
      (destructuring-bind (q0 q1) e
        (push e (gethash q0 outgoing))
        (push e (gethash q1 incoming))
        (setf (gethash e accept) t)
        (unless directed
          (let ((e (list q1 q0)))
            (setf (gethash e accept) t)
            (push e (gethash q1 outgoing))
            (push e (gethash q0 incoming))))
        (setf (gethash q0 places) t
              (gethash q1 places) t)))
    ;; build edges
    (let ((edges))
      (loop for q being the hash-keys of places
         do
           (dolist (in (gethash q incoming))
             (dolist (out (gethash q outgoing))
               (push (list in q out) edges))))
      (assert (every (lambda (e)
                       (if (= 2 (length e))
                           (destructuring-bind ((q0 q1) z) e
                             (declare (ignore q0))
                             (equal q1 z))
                           (destructuring-bind ((q0 q1) z (q2 q3)) e
                             (declare (ignore q0 q3))
                             (and (equal q1 z) (equal q2 z)))))
                     edges))
      (make-fa edges
               start
               accept))))

;;;;;;;;;;;;;;;;;;;;
;; SET OPERATIONS ;;
;;;;;;;;;;;;;;;;;;;;


(defun dfa-eq (a b)
  "Check equivalence up to state names of DFAs"
  (and (finite-set-equal (fa-states a) (fa-states b))
       (finite-set-equal (fa-terminals a) (fa-terminals b))
       (finite-set-equal (fa-accept a) (fa-accept b))
       (equal (fa-edges a) (fa-edges b))
       (equal (fa-start a) (fa-start b))))

(defun dfa-equal (a b)
  "Check equivalence up to state names of DFAs"
  (assert (dfa-p a))
  (assert (dfa-p b))
  (let ((a (dfa-renumber a))
        (b (dfa-renumber b)))
    (dfa-eq a b)))

(defun fa-equiv (a b)
  "Check if two FAs recognize the same language."
  (dfa-eq (fa-canonicalize a)
          (fa-canonicalize b)))

(defun fa-empty-p (fa)
  "Does FA include no strings?"
  (with-dfa (dfa fa)
    (let ((succ (fa-successors dfa))
          (visited (make-finite-set :mutable t))
          (accept (fa-accept dfa)))
      (labels ((visit (q)
                 (unless (or (null q) (finite-set-inp q visited))
                   (if (finite-set-inp q accept)
                       t ;; found accept (not empty)
                       (progn (setq visited (finite-set-nadd visited q))
                              (some (lambda (z-q1) (visit (second z-q1))) (funcall succ q)))))))
        (not (visit (fa-start dfa)))))))

(defun make-empty-fa (terminals)
  "Create an FA including no strings."
  (%make-fa :states (finite-set-tree '(0))
            :terminals (finite-set-tree terminals)
            :edges nil
            :start 0
            :accept nil))


(defun make-epsilon-fa (terminals)
  "Create an FA including only the empty string."
  (%make-fa :states (finite-set-tree '(0))
            :terminals (finite-set-tree terminals)
            :edges nil
            :start 0
            :accept (finite-set-tree '(0))))

(defun make-universal-fa (terminals)
  "Create an FA recognizing all strings over TERMINALS."
  (make-fa-1 (finite-set 0)
            terminals
            (finite-set-map 'list (lambda (z) (list 0 z 0)) terminals)
            0
            (finite-set 0)))

(defun fa-universal-p (fa &optional (terminals (fa-terminals fa)))
  "Does FA recognize all strings over TERMINALS?"
  (dfa-eq (fa-canonicalize fa)
          (dfa-renumber (make-universal-fa (finite-set-remove terminals :epsilon)))))

(defun fa-intersection (fa1 fa2)
  "Intersection of FA1 and FA2"
  (with-dfa (dfa1 fa1)
    (with-dfa (dfa2 fa2)
      ;; Simulate each FA simultaneously.  Accept when both FAs accept.
      (let ((move-1 (dfa-mover dfa1))
            (move-2 (dfa-mover dfa2))
            (out-1 (fa-outgoing-terminal-function dfa1))
            (out-2 (fa-outgoing-terminal-function dfa2))
            (visited (make-finite-set :mutable t))  ;; visited states of new fa
            (edges)) ;; edges of new fa
        (labels ((visit (q1 q2)
                   (let ((qq (list q1 q2))
                         (zz-1 (funcall out-1 q1))
                         (zz-2 (funcall out-2 q2)))
                     (setf visited (finite-set-nadd visited qq)) ;; mark state visited
                     ;; map over outgoing terminals from the compound state
                     (finite-set-map 'nil
                                     (lambda (z)
                                       (when (finite-set-inp z zz-2) ;; follow only when both have outgoing terminal
                                         (let* ((p1 (funcall move-1 q1 z))
                                                (p2 (funcall move-2 q2 z))
                                                (pp (list p1 p2)))
                                           (push (list qq z pp) edges)
                                           (unless (finite-set-inp pp visited)
                                             (visit p1 p2)))))
                                     zz-1))))
          (visit (fa-start dfa1) (fa-start dfa2)))
        ;; now build the fa structure
        (make-fa-1 visited
                   (finite-set-union (fa-terminals dfa1) (fa-terminals dfa2)) ;; does this make sense?
                   edges
                   (list (fa-start dfa1) (fa-start dfa2)) ;; start of both fa
                   ;; accept when both FAs accept
                   (loop for k being the hash-keys of visited
                      when (and (finite-set-inp (first k) (fa-accept dfa1))
                                (finite-set-inp (second k) (fa-accept dfa2)))
                      collect k))))))


(defun fa-complement (fa &optional (terminals (finite-set-remove (fa-terminals fa) :epsilon)))
"Return the complement of FA.

This is the finite-automaton that accepts all strings NOT in FA."
  (with-dfa (dfa fa)
    (let ((dead-accept (gensym "ACCEPT"))
          (edges (fa-edges dfa))
          (outgoing (fa-outgoing-terminal-function dfa)))
      ;; add "dead" edges"
      (do-finite-set (q (fa-states dfa))
        (do-finite-set (z (finite-set-difference terminals
                                                 (funcall outgoing q)))
          (push (list q z dead-accept) edges)))
      ;; add dead self edges
      (do-finite-set (z terminals)
        (push (list dead-accept z dead-accept) edges))
      ;; make fa
     (make-fa edges
               (fa-start dfa)
               (finite-set-add (finite-set-difference (fa-states dfa) (fa-accept dfa))
                               dead-accept)))))

(defun fa-union (fa1 fa2 &optional (unique (gensym)))
  "Union of finite automata FA1 and FA2."
  (if (< (finite-set-length (fa-states fa2))
         (finite-set-length (fa-states fa1)))
      (fa-union fa2 fa1)
      (progn
        (labels ((fixup (q) (cons q unique)))
          (make-fa (append
                    `((,unique :epsilon ,(fixup (fa-start fa1)))
                      (,unique :epsilon ,(fa-start fa2)))
                    (loop for (q0 z q1) in (fa-edges fa1)
                       collect (list (fixup q0) z (fixup q1)))
                    (fa-edges fa2))
                   unique
                   (finite-set-union (fa-accept fa2)
                                     (fold-finite-set (lambda (set x)
                                                        (finite-set-add set (fixup x)))
                                                      (make-finite-set :compare #'gsymbol-compare)
                                                      (fa-accept fa1))))))))


(defun fa-concatenate (fa1 fa2 &optional (unique (gensym)))
  "Concatenation language of FA1 and FA2."
  (let ((edges (fa-edges fa2))
        (start2 (fa-start fa2))
        (states (fa-states fa2)))
    (loop for (q0 z q1) in (fa-edges fa1)
       for nq0 = (gsymbol-gen q0 unique)
       for nq1 = (gsymbol-gen q1 unique)
       do
         (push (list nq0 z nq1) edges)
         (setq states (finite-set-add (finite-set-add states nq0)
                                      nq1)))
    (do-finite-set (q0 (fa-accept fa1))
      (push (list (gsymbol-gen q0 unique)
                  :epsilon start2)
            edges))
    (%make-fa :states states
              :terminals (finite-set-union (fa-terminals fa1)
                                           (fa-terminals fa2))
              :edges edges
              :start (gsymbol-gen (fa-start fa1) unique)
              :accept (fa-accept fa2))))



(defun random-fa (state-count terminal-count &key
                  (edge-count  (random (* state-count terminal-count)))
                  (accept-count (random state-count)))
  (make-fa-1 (loop for i below state-count collect i)
             (loop for i below terminal-count collect i)
             (loop for i below edge-count
                collect (list (random state-count) (random terminal-count) (random state-count)))
             0
             (loop for i below accept-count
                collect (random state-count))))


(defun fa-pop-initial (fa terminal)
  "Return new fa for postfix after seeing TERMINAL or NIL if unable."
  (with-dfa (fa fa)
    (let* ((start (fa-start fa))
           (new-start (block new-start
                        (fa-map-edges nil (lambda (q0 z q1)
                                            (when (and (equal q0 start)
                                                       (equal z terminal))
                                              (return-from new-start q1)))
                                      fa))))
      (when new-start
        (%make-fa :states (fa-states fa)
                  :terminals (fa-terminals fa)
                  :edges (fa-edges fa)
                  :start new-start
                  :accept (fa-accept fa))))))

(defun fa-initial-terminals (fa)
  "Return set of terminals the may begin strings in FA."
  (let* ((start (fa-start fa)))
    (fold-fa-edges (lambda (set q0 z q1)
                     (declare (ignore q1))
                     (if (equal q0 start)
                         (finite-set-nadd set z)
                         set))
                   (make-finite-set :compare #'gsymbol-compare)
                   (ensure-dfa fa))))



;;;;;;;;;;;;;;
;;; OUTPUT ;;;
;;;;;;;;;;;;;;

(defun fa-dot (fa &key output (font-size 12) (accept-shape "doublecircle"))
  "Generate Graphviz output for FA.
FA: finite automaton.
OUTPUT: output file, type determined by suffix (png,pdf,eps)."
  (let ((state-numbers (finite-set-enumerate (fa-states fa))))
    (output-dot output
                (lambda (s)
                  (format s "~&digraph {~%")
                  (format s "~&rankdir=\"LR\";~%")
                  ;; state labels
                  (format s "~:{~&  ~A[label=\"~A\",fontsize=~D];~}"
                          (finite-set-map 'list (lambda (state)
                                                  (list (funcall state-numbers state) state font-size))
                                          (fa-states fa)))
                  ;; start state
                  (format s "~&  start[shape=none,fontsize=~D];" font-size)
                  (format s "~&  start -> ~A;" (funcall state-numbers (fa-start fa)))
                  ;; accept state
                  (format s "~:{~&  ~A [ shape=~A ];~}"
                          (finite-set-map 'list (lambda (q)
                                                  (list (funcall state-numbers q) accept-shape))
                                          (fa-accept fa)))

                  ;; edges
                  (fa-map-edges nil
                                (lambda (q0 z q1)
                                  (format s "~&  ~A -> ~A [fontsize=~D,label=\"~A\"];~%"
                                          (funcall state-numbers q0)
                                          (funcall state-numbers q1)
                                          font-size (dot-gsymbol z)))
                                fa)
                  (format s "~&}~%")))))

(defun fa-pdf (fa &key (output "/tmp/dot.pdf") (font-size 12))
  "Graphviz output of dfa."
  (fa-dot fa :output output :font-size font-size))
