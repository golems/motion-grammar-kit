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

(defstruct (fa (:constructor %make-fa))
  states
  terminals
  edges
  start
  accept)

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


(defun make-fa (edges start accept)
  (declare (type finite-set accept))
  (let ((state-set (make-finite-set :mutable t))
        (terminal-set (make-finite-set :mutable t)))
    (loop for (q0 z q1) in edges
       do (setf (gethash q0 state-set) t
                (gethash q1 state-set) t
                (gethash z terminal-set) t))
    (%make-fa :states (finite-set-list state-set)
              :terminals (finite-set-list terminal-set)
              :edges edges
              :start start
              :accept (finite-set-list accept))))

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
                                       (finite-set-add (gethash k hash) q0)))
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
                              (finite-set-add (gethash q hash) z)))
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

(defun dfap (fa)
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



(defun dfa-canonicalize (dfa)
  (let ((hash (make-hash-table :test #'equal))
        (succ (fa-successors dfa)))
    ;; label states
    (let ((count -1))
      (declare (type fixnum count))
      (labels ((visit (q)
                 (unless (gethash q hash)
                   (setf (gethash q hash) (incf count))
                   (do-finite-set (rest (gsymbol-sort (funcall succ q)))
                     (destructuring-bind (z q1) rest
                       (declare (ignore z))
                       (visit q1))))))
        (visit (fa-start dfa)))
      ;; build
      (%make-fa :states (loop for i from 0 to count collect i)
                :terminals (sort (copy-list (fa-terminals dfa)) #'gsymbol-predicate)
                :edges (sort (loop for (q0 z q1) in (fa-edges dfa)
                                collect (list (gethash q0 hash) z (gethash q1 hash)))
                             #'gsymbol-predicate)
                :start 0
                :accept (sort (finite-set-map 'list (curry-right #'gethash hash) (fa-accept dfa))
                              #'gsymbol-predicate)))))

(defun fa-reverse (fa &optional (unique (gensym)))
  (let ((new-start (gsymbol-gen "start" unique)))
    (make-fa (append (finite-set-map 'list (lambda (a) (list new-start :epsilon a)) (fa-accept fa))
                     (map 'list #'reverse (fa-edges fa)))
             new-start
             (list (fa-start fa)))))


(defun fa-canonicalize (fa)
  ;; Hoprcroft's actually does go faster
  (dfa-canonicalize (fa-minimize-hopcroft fa)))
  ;;(dfa-canonicalize (fa-minimize-brzozowski fa)))


(defun fa-canonicalize-brzozowski (fa)
  (dfa-canonicalize (fa-minimize-brzozowski fa)))

(defun fa-canonicalize-hopcroft (fa)
  (dfa-canonicalize (fa-minimize-hopcroft fa)))

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

(defun nfa-move-e-closure (states z mover)
  "epsilon-closure of list STATES transitioned by token Z.
STATES: list of states
Z: token
MOVER: fuction from (state-0 token) => (list state-1-0 state-1-1...)"
  (let ((closure (make-finite-set :mutable t)))
    (do-finite-set (q0 states)
      (nfa-e-closure (funcall mover q0 z) mover closure))
    closure))

;; See Aho, 2nd p. 152
(defun nfa->dfa (nfa)
  "Convert an NFA to a DFA"
  (let* ((mover (nfa-mover nfa))
         (hash (make-hash-table :test #'equal))
         (start-0 (finite-set-list (nfa-e-closure (list (fa-start nfa)) mover)))
         ;; eliminate useless NFA start states
         (start (sort (if (or (null (cdr start-0))
                              (some (lambda (e) (equal (fa-start nfa) (third e))) (fa-edges nfa)))
                          start-0
                          (finite-set-remove start-0 (fa-start nfa)))
                      #'gsymbol-predicate))
         (terminals (finite-set-remove (fa-terminals nfa) :epsilon))
         edges)
    ;; subset construction
    (labels ((subset (q)
               (unless (gethash q hash)
                 (setf (gethash q hash) t)
                 (do-finite-set (z terminals)
                   (let ((u (sort (finite-set-list (nfa-move-e-closure q z mover)) #'gsymbol-predicate)))
                     (when u
                       ;;(assert (null (finite-set-difference u (nfa-e-closure u mover))))
                       (push (list q z u) edges)
                       (subset u)))))))
      (subset start))
    ;; result
    (let ((fa (make-fa edges
                       start
                       (loop for q being the hash-keys of hash
                          when (finite-set-intersection (fa-accept nfa) q)
                          collect q))))
      ;(assert (dfap fa))
      fa)))

(defmacro with-dfa ((var fa) &body body)
  (alexandria:with-gensyms (fa-sym)

    `(let ((,var (let ((,fa-sym ,fa))
                   (if (dfap ,fa-sym)
                       ,fa-sym
                       (nfa->dfa ,fa-sym)))))
       ,@body)))

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
                                     (finite-set-add (gethash q0 hash) q1))
                               hash)
                             (make-hash-table :test #'equal)
                             fa))
        (pred (fold-fa-edges (lambda (hash q0 v q1)
                               (declare (ignore v))
                               (setf (gethash q1 hash)
                                     (finite-set-add (gethash q1 hash) q0))
                               hash)
                             (make-hash-table :test #'equal)
                             fa))
        (live-fwd (make-hash-table :test #'equal))
        (live-rev (make-hash-table :test #'equal)))
    ;; mark forward live states
    (labels ((visit (q)
               (unless (gethash q live-fwd)
                 (setf (gethash q live-fwd) t)
                 (map nil #'visit (gethash q succ)))))
      (visit (fa-start fa)))
    ;; mark reverse live states
    (labels ((visit (q)
               (unless (gethash q live-rev)
                 (setf (gethash q live-rev) t)
                 (map nil #'visit (gethash q pred)))))
      (map nil #'visit (fa-accept fa)))
    (let ((live (fold-finite-set (lambda (live q)
                                   (if (and (gethash q live-fwd)
                                            (gethash q live-rev))
                                       (finite-set-add live q)
                                       live))
                                 nil (fa-states fa))))
      (when (finite-set-inp (fa-start fa) live)
        (make-fa (loop for (q0 z q1) in (fa-edges fa)
                    when (and (finite-set-inp q0 live)
                              (finite-set-inp q1 live))
                    collect (list q0 z q1))
                 (fa-start fa)
                 (finite-set-intersection live (fa-accept fa)))))))

(defun dfa-add-reject (dfa &optional (reject (gensym "reject")))
  "Add explicit reject state to the dfa."
  (let ((mover (dfa-mover dfa))
        (edges (fa-edges dfa)))
    (do-finite-set (q (fa-states dfa))
      (do-finite-set (z (fa-terminals dfa))
        (unless (funcall mover q z)
          (push (list q z reject)
                edges))))
    (make-fa edges (fa-start dfa) (fa-accept dfa))))



;;; Regex
;;;  Parse Tree S-Expression
;;;    - :concatenation  --- ab
;;;    - :closure        --- a*
;;;    - :union          --- a|b


;; Hopcroft's Algorithm
(defun fa-minimize-hopcroft (fa)
  "Minimize a DFA via Hopcroft's's Algorithm."
  ;; TODO: more efficient representation of rejecting state
  ;;       make implicit somehow
  (let* ((reject (gensym "reject"))
         (dfa (dfa-add-reject (if (dfap fa)
                                  fa
                                  (nfa->dfa fa))
                              reject))
         (p (list (fa-accept dfa)
                  (finite-set-difference (fa-states dfa)
                                         (fa-accept dfa)))))
    ;; build minimal states
    ;; Note: CLISP 2.48 seemingly can't handle LOOP here
    (do ((q (finite-set (fa-accept dfa)))
         (imover (nfa-reverse-mover dfa))
         (a (fa-accept dfa) (pop q)))
        ((null a))
         ;(format t "~&p: ~A~&" p)
      (do-finite-set (c (fa-terminals dfa))
        ;; x: predecessors of a for token c
        (let ((x (fold (lambda (x q)
                         (finite-set-union x (funcall imover q c)))
                       nil a)))
          (when x
            (loop for yy on p
               for y = (car yy)
               ;; subset of y transitioning to a on c
               for i = (and (cdr y)
                            (finite-set-intersection y x))
               ;; subset of y transitioning to (not a) on c
               for j = (and i (finite-set-difference y x))
               when (and i j)
               do (when (< (length j) (length i))
                    (rotatef i j))  ; i is smaller
                 (assert (<= (length i) (length j)))
               ;; insert into q
                 (loop for zz on q
                    when (equal y (car zz))
                    do (rplaca zz j))
                 (push i q)
               ;; insert into p
                 (rplaca yy i)
                 (rplacd yy (cons j (cdr yy))))))))
    ;;(format t "~&~A" p)
    (assert (= (length (fa-states dfa))
               (loop for part in p summing (length part))))
    (assert (finite-set-equal  (fold #'finite-set-union nil p)
                               (fa-states dfa)))
    (let* ((state-hash (fold (lambda (hash p)
                               (do-finite-set (q p)
                                 (setf (gethash q hash) p))
                               hash)
                             (make-hash-table :test #'equal) p))
           (edge-hash (fold (lambda (hash edge)
                              (destructuring-bind (q0 z q1) edge
                                (unless (eq q1 reject)
                                  (setf (gethash (list (gethash q0 state-hash)
                                                       z
                                                       (gethash q1 state-hash))
                                                 hash)
                                        t)))
                              hash)
                            (make-hash-table :test #'equal)
                            (fa-edges dfa))))
      (fa-prune (make-fa (loop for k being the hash-keys of edge-hash collect k)
                         (find-if (lambda (x) (finite-set-inp (fa-start dfa) x)) p)
                         (loop for x in p
                            when (finite-set-intersection x (fa-accept dfa))
                            collect x))))))

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

(defun fa-from-adjacency (adj &key (start (car adj))  directed)
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

(defun fa->right-regular-grammar (fa)
  (fa-edges fa))

;;;;;;;;;;;;;;;;;;;;
;; SET OPERATIONS ;;
;;;;;;;;;;;;;;;;;;;;

(defun dfa-equal (a b)
  "Check equivalence up to state names of DFAs"
  (assert (dfap a))
  (assert (dfap b))
  (equalp (dfa-canonicalize a) (dfa-canonicalize b)))

(defun fa-equiv (a b)
  "Check if two FAs recognize the same language."
  (dfa-equal (fa-minimize-brzozowski a)
             (fa-minimize-brzozowski b)))

(defun fa-empty-p (fa)
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

(defun fa-intersection (fa1 fa2)
  "Intersection of two FA"
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
                     (setf visited (finite-set-nadd visited (list q1 q2))) ;; mark state visited
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
        (make-fa edges
                 (list (fa-start dfa1) (fa-start dfa2)) ;; start of both fa
                 ;; accept when both FAs accept
                 (loop for k being the hash-keys of visited
                    when (and (finite-set-inp (first k) (fa-accept dfa1))
                              (finite-set-inp (second k) (fa-accept dfa2)))
                    collect k))))))


(defun fa-complement (nfa &optional (terminals (finite-set-remove (fa-terminals nfa) :epsilon)))
  (with-dfa (dfa nfa)
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




;;;;;;;;;;;;;;
;;; OUTPUT ;;;
;;;;;;;;;;;;;;

(defun fa-dot (fa &key output (font-size 12) (accept-shape "doublecircle"))
  "Graphviz output of dfa.
fa: finite automaton
output: output file, type determined by suffix (png,pdf,eps)"
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
                  (format s "~{~&  ~A [ shape=doublecircle ];~}"
                          (finite-set-map 'list state-numbers  (fa-accept fa)))
                  ;; edges
                  (fa-map-edges nil
                                (lambda (q0 z q1)
                                  (format s "~&  ~A -> ~A [fontsize=~D,label=\"~A\"];~%"
                                          (funcall state-numbers q0)
                                          (funcall state-numbers q1)
                                          font-size z))
                                fa)
                  (format s "~&}~%")))))

(defun fa-pdf (fa &key (output "/tmp/dot.pdf") (font-size 12))
  "Graphviz output of dfa."
  (fa-dot fa :output output :font-size font-size))
