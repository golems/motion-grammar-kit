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

(defun intersectionp (a b &optional (test #'eql))
  (map nil (lambda (a)
             (map nil (lambda (b)
                        (when (funcall test a b)
                          (return-from intersectionp t)))
                  b))
       a))

(defun symbol-compare (a b)
  (cond
    ((and (numberp a) (numberp b))
     (< a b))
    ((numberp a) t)
    ((numberp b) nil)
    (t (string< (string a) (string b)))))

(defun symbol-list-compare (a b)
  (cond
    ((and (atom a) (atom b)) (symbol-compare a b))
    ((atom a) t)
    ((atom b) nil)
    ((null a) t)
    ((null b) t)
    (t (if (equal (car a) (car b))
           (symbol-list-compare (cdr a) (cdr b))
           (symbol-list-compare  (car a) (car b))))))

(defun curry (function arg0)
  (lambda (arg1) (funcall function arg0 arg1)))

(defun curry-right (function arg1)
  (lambda (arg0) (funcall function arg0 arg1)))

(defun curry-list (function &rest initial-args)
  (lambda (&rest final-args) (apply function (append initial-args final-args))))

(defun chain (value &rest functions)
  (if functions
      (apply #'chain
             (funcall (car functions) value)
             (cdr functions))
      value))

(defun fold (function initial-value &rest lists)
  (let ((value initial-value))
    (apply #'map nil
           (lambda (&rest args)
             (setq value (apply function value args)))
           lists)
    value))

(defun multiple-value-reduce (function sequence &key initial-value-list)
  (let ((result initial-value-list))
    (map nil
         (lambda (&rest rest)
           (setq result (multiple-value-list
                         (apply function (append result rest)))))
         sequence)
    (apply #'values result)))

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
  tokens
  edges
  start
  accept
  reject)

(defun fa-state-name (fa i)
  (aref (fa-states fa) i))
(defun fa-token-name (fa i)
  (aref (fa-tokens fa) i))

(defun fa-map-edge-lists (result-type function fa)
  (map result-type function (fa-edges fa)))

(defun fa-map-edges (result-type function fa)
  "Apply FUNCTION to each edge in FA."
  (map result-type (curry #'apply function)
       (fa-edges fa)))


(defun fa-subset-indices (subset sequence)
  "return a list of indices of sequence elements that intersect with subset"
  (multiple-value-reduce (lambda (indices i s)
                           (values (if (intersectionp subset s)
                                       (cons i indices)
                                       indices)
                                   (1+ i)))
                         sequence :initial-value-list '(nil 0)))


(defun make-fa-simple (edges start accept)
  (let (states tokens)
    (map nil (lambda (x)
               (destructuring-bind (q0 z q1) x
                 (pushnew q0 states :test #'equal)
                 (pushnew q1 states :test #'equal)
                 (pushnew z tokens :test #'equal)))
         edges)
    (%make-fa :states states
              :tokens tokens
              :edges edges
              :start (alexandria:ensure-list start)
              :accept (alexandria:ensure-list accept))))

;; fixme: multiple start states
(defun make-fa-renumber (edges start accept)
  (let ((state-hash (make-hash-table :test #'equal))
        (token-hash (make-hash-table :test #'equal))
        (state-counter -1)
        (token-counter 0)
        nedges)
    (labels ((new-state (name)
               (unless (gethash name state-hash)
                 (setf (gethash name state-hash)
                       (incf state-counter)))
               (gethash name state-hash)))
      (setf (gethash :epsilon token-hash) 0)
      ;; start/accept states
      (map nil #'new-state (alexandria:ensure-list start))
      (map nil #'new-state (alexandria:ensure-list accept))
      ;; build hashes
      (loop for  (q0 z q1) in edges
         do (unless (gethash z token-hash)
              (setf (gethash z token-hash) (incf token-counter)))
           (push (list (new-state q0) (gethash z token-hash) (new-state q1))
                 nedges))
      ;; map hashes to build new edges and mapping arrays
      (let ((state-array (make-array (1+ state-counter)
                                     :initial-element nil))   ; i -> q
            (token-array (make-array (1+ token-counter)
                                     :initial-element nil)))  ;  i -> z
        (maphash (lambda (n d)
                   ;;(format t "~&[~A]: ~A~&" d n)
                   (setf (aref state-array d) n))
                 state-hash)
        (maphash (lambda (n d) (setf (aref token-array d) n))
                 token-hash)
        (%make-fa :states state-array
                  :tokens token-array
                  :edges nedges
                  :start (mapcar #'new-state (alexandria:ensure-list start))
                  :accept (mapcar #'new-state
                                  (alexandria:ensure-list accept)))))))


(defun make-fa (edges start accept &key (renumber-symbols t) (dfa-sort nil))
  (let ((fa (if renumber-symbols
                (make-fa-renumber edges start accept)
                (make-fa-simple edges start accept))))
    (if dfa-sort
        (dfa-sort fa)
        fa)))


(defun dfap (fa)
  "True if FA is deterministic"
  (let ((mover (fa-mover fa)))
    (when (> (length (fa-start fa)) 1)
      (return-from dfap (values nil -1 0)))
    (dotimes (i (length (fa-states fa)))
      ;; epsilon
      (when (funcall mover i 0)
        (return-from dfap (values nil i 0)))
      ;; others
      (loop for j from 1 below (length (fa-tokens fa))
         do (when (> (length (funcall mover i j)) 1)
              (return-from dfap (values nil i j)))))
    fa))


(defun dfa-sort (fa &key preserve-states)
  "Sort all elements of the FA into canonical (ascending) order.
PRESERVE-STATES: If true, sort state names.
                 Otherwise renumber the states in a canonical (DFS) order."
  (let ((token-indices (make-array (length (fa-tokens fa)) :element-type 'fixnum))
        (state-indices (make-array (length (fa-states fa)) :element-type 'fixnum))
        (token-old (make-array (length (fa-tokens fa)) :element-type 'fixnum))
        (state-old (make-array (length (fa-states fa)) :element-type 'fixnum)))
    ;; sort tokens
    (dotimes (i (length token-indices)) (setf (aref token-indices i) i))
    (sort token-indices
          (lambda (a b)
            (cond ((zerop a) t)
                  ((zerop b) nil)
                  (t (symbol-list-compare  (aref (fa-tokens fa) a)
                                           (aref (fa-tokens fa) b))))))
    (reduce (lambda (i k)
              (1+ (setf (aref token-old k) i)))
            token-indices :initial-value 0)
    ;; sort state
    (if preserve-states
        ;; use original states
        (progn
          (dotimes (i (length state-indices)) (setf (aref state-indices i) i))
          (sort state-indices
                (lambda (a b) (symbol-list-compare  (aref (fa-states fa) a)
                                               (aref (fa-states fa) b))))
          (reduce (lambda (i k)
                    (1+ (setf (aref state-old k) i)))
                  state-indices :initial-value 0))
        ;; renumber states
        (progn
          (dotimes (i (length state-indices))
            (setf (aref state-indices i) i
                  (aref state-old i) -1))
          (let ((counter -1)
                (mover (fa-mover fa)))
            (labels ((visit (i-q)
                       (when (and i-q
                                  (= (aref state-old i-q)
                                     -1))
                         (setf (aref state-old i-q) (incf counter))
                         (loop for i below (length (fa-tokens fa))
                            for io = (aref token-indices i) ;; in sorted token order
                            do (visit (car (funcall mover i-q io)))))))
              (visit (car (fa-start fa)))))))
    (dotimes (i (length (fa-states fa)))
      (assert (>= (aref state-old i) 0)))
    ;; Build sorted FA
    (fa-renumber fa :state-map state-old :token-map token-old :sort t)))


(defun dfa-equal (a b)
  "Check equivalence up to state names of DFAs"
  (assert (dfap a))
  (assert (dfap b))
  (equalp (dfa-sort a) (dfa-sort b)))

(defun fa-equiv (a b)
  "Check if two FAs recognize the same language."
  (dfa-equal (fa-minimize-brzozowski a)
             (fa-minimize-brzozowski b)))

(defun fa-mover (fa)
  (let ((move (make-array (list (length (fa-states fa)) (length (fa-tokens fa)))
                          :initial-element nil)))
    (fa-map-edges nil (lambda (q0 z q1)
                        (push q1 (aref move q0 z)))
                  fa)
    (curry-list #'aref move)))


(defun fa-inv-mover (fa)
  (let ((move (make-array (list (length (fa-states fa)) (length (fa-tokens fa)))
                          :initial-element nil)))
    (fa-map-edges nil (lambda (q0 z q1)
                        (push q0 (aref move q1 z)))
                  fa)
    (curry-list #'aref move)))

(defun fa-successor-array (fa)
  "Make array indexed by original state of edge lists from that state."
  (let ((array (make-array (length (fa-states fa)) :initial-element nil)))
    (fa-map-edges nil (lambda (&rest e)
                        (push e (aref array (car e))))
                  fa)
    array))

(defun fa-predecessor-array (fa)
  "Make array indexed by final state of edge lists to that state."
  (let ((array (make-array (length (fa-states fa)) :initial-element nil)))
    (fa-map-edges nil (lambda (&rest e)
                        (push e (aref array (caddr e))))
                  fa)
    array))

(defun fa-dot (fa &key output (font-size 12))
  "Graphviz output of dfa.
fa: finite automaton
output: output file, type determined by suffix (png,pdf,eps)"
  (labels ((token-label (i)
             (let ((name (fa-token-name fa i)))
               (if (eq :epsilon name)
                   "&epsilon;" ; print epsilons in greek
                   name)))
           (state-label (i)
             (let ((name (fa-state-name fa i)))
               name))
           (helper (s)
             (format s "~&digraph {~%")
             (format s "~&rankdir=\"LR\";~%")
             ;; state labels
             (format s "~:{~&  ~A[label=\"~A\",fontsize=~D];~}"
                     (loop for i below (length (fa-states fa))
                          collect (list i (state-label i) font-size)))
             ;; start state
             (when (fa-start fa)
               (format s "~&  start[shape=none,fontsize=~D];" font-size)
               (format s "~{~&  start -> ~A;~}"
                       (map 'list #'identity
                            (alexandria:ensure-list (fa-start fa)))))
             ;; accept state
             (format s "~{~&  ~A [ shape=doublecircle ];~}"
                     (map 'list #'identity
                          (alexandria:ensure-list (fa-accept fa))))
             (fa-map-edges nil
                           (lambda (q0 z q1)
                             (format s "~&  ~A -> ~A [fontsize=~D,label=\"~A\"];~%"
                                     q0 q1 font-size (token-label z)))
                           fa)
             (format s "~&}~%"))
           (dot (ext)
             (let ((p (sb-ext:run-program "dot" (list (concatenate 'string "-T" ext))
                                          :wait nil :search t :input :stream :output output
                                          :if-output-exists :supersede)))
               (helper (sb-ext:process-input p))
               (close (sb-ext:process-input p))
               (sb-ext:process-wait p)
               (sb-ext:process-close p))))
    (cond
      ((and (stringp output) (ppcre:scan "\.png$" output))
       (dot "png"))
      ((and (stringp output) (ppcre:scan "\.ps$" output))
       (dot "ps"))
      ((and (stringp output) (ppcre:scan "\.eps$" output))
       (dot "eps"))
      ((and (stringp output) (ppcre:scan "\.pdf$" output))
       (dot "pdf"))
      ((streamp output)
       (helper output))
      (t
       (with-output-to-string (s)
         (helper s))))))

(defun fa-reverse (fa)
  (%make-fa :states (fa-states fa)
            :tokens (fa-tokens fa)
            :edges (mapcar #'reverse (fa-edges fa))
            :start (fa-accept fa)
            :accept (fa-start fa)))


(defun fa-prune (fa)
  "Remove unreachable and dead states from the FA."
  (let ((pred (fa-predecessor-array fa))
        (succ (fa-successor-array fa))
        (live-rev (make-array (length (fa-states fa)) :element-type 'bit))
        (live-fwd (make-array (length (fa-states fa)) :element-type 'bit))
        (state-map (make-array (length (fa-states fa)) :initial-element nil)))
    ;; mark forward live states
    (labels ((visit (i)
               (when (zerop (aref live-fwd i))
                 (setf (aref live-fwd i) 1)
                 (map nil (lambda (e) (visit (third e))) (aref succ i)))))
      (map nil #'visit (fa-start fa)))
    ;; mark reverse live states
    (labels ((visit (i)
               (when (zerop (aref live-rev i))
                 (setf (aref live-rev i) 1)
                 (map nil (lambda (e) (visit (car e))) (aref pred i)))))
      (map nil #'visit (fa-accept fa)))
    ;; find reorder mapping
    (loop
       for i from 0 ; original index
       with j = -1   ; new index
       for x across live-fwd
       for y across live-rev
       unless (or (zerop x) (zerop y))
       do (setf (aref state-map i)
                (incf j)))
    (fa-renumber fa :state-map state-map)))

(defun dfa-add-reject (dfa)
  "Add explicit reject state to the dfa."
  (let ((edges (fa-map-edges 'list
                             (lambda (q0 z q1)
                               (list q0 (aref (fa-tokens dfa) z) q1))
                             dfa))
        (mover (fa-mover dfa)))
    (dotimes (i (length (fa-states dfa)))
      (loop for j from 1 below (length (fa-tokens dfa))
         do (unless (funcall mover i j)
              (push (list i (aref (fa-tokens dfa) j) -1)
                    edges))))

    ;(loop for j from 1 below (length (fa-tokens dfa))
       ;do (push (list -1 (aref (fa-tokens dfa) j) -1)
                ;edges))
    (make-fa edges (fa-start dfa) (fa-accept dfa))))



(defun fa-renumber (fa &key state-map token-map sort)
  "Reorder, merge, or remove states and tokens in the FA.
STATE-MAP: (aref state-map old-index) => (or new-index nil)
TOKEN-MAP: (aref token-map old-index) => new-index
SORT: if t, put the resulting DFA in a canonical order"
  (labels ((new-state (i)
             (if state-map
                 (aref state-map i)
                 i))
           (new-token (i)
             (if token-map
                 (aref token-map i)
                 i))
           (state-translate (list)
             (reduce (lambda (new-list old)
                       (if (new-state old)
                           (union (list (new-state old)) new-list)
                           new-list))
                     list :initial-value nil)))
    (let ((fa (%make-fa :states (cond ((null state-map) (fa-states fa))
                                      (t (map 'vector #'identity
                                              (loop
                                                 with i = -1
                                                 for x across (remove-duplicates state-map)
                                                 when x collect (incf i)))))
                        :tokens (if (null token-map)
                                    (fa-tokens fa)
                                    (let ((tokens (make-array (loop
                                                                 for x across
                                                                   (remove-duplicates token-map)
                                                                 count x)
                                                              :initial-element nil)))
                                      (reduce (lambda (i x)
                                                (if x
                                                  (progn (setf (aref tokens (aref token-map i))
                                                               (aref (fa-tokens fa) i))
                                                         (1+ i))
                                                  i))
                                              token-map :initial-value 0)
                                      tokens))
                        :start (state-translate (fa-start fa))
                        :accept (state-translate (fa-accept fa))
                        :edges (let ((hash (make-hash-table :test #'equal)))
                                 (fa-map-edges nil
                                               (lambda (q0 z q1)
                                                 (when (and (new-state q0)
                                                            (new-token z)
                                                            (new-state q1))
                                                   (setf (gethash (list (new-state q0)
                                                                        (new-token z)
                                                                        (new-state q1))
                                                                  hash)
                                                         t)))
                                               fa)
                                 (loop for key being the hash-keys of hash
                                    collect key)))))

      (when sort
        (setf (fa-edges fa) (sort (fa-edges fa) #'symbol-list-compare)
              (fa-start fa) (sort (fa-start fa) #'<)
              (fa-accept fa) (sort (fa-accept fa) #'<)))
      fa)))


;;; Regex
;;;  Parse Tree S-Expression
;;;    - :concatenation  --- ab
;;;    - :closure        --- a*
;;;    - :union          --- a|b


;; The McNaughton-Yamada-Thompson algorithm
;; Aho 2nd Ed., P 159

(defun regex->nfa (regex)
  "Convert a regex parse-tree to an NFA
RESULT: (list edges start final)"
  (let ((state-counter 0)
        (edges nil))
    (labels ((is-op (symbol tree)
               (and (listp tree)
                    (eq symbol (first tree))))
             (new-edge (state-0 token state-1)
               (push (list state-0 token state-1) edges))
             (visit (start tree)
               ;; recursively visit the tree
               (cond
                 ((is-op :concatenation tree)
                  (reduce #'visit (cdr tree)
                          :initial-value start))
                 ((is-op :union tree)
                  (let ((end (incf state-counter)))
                    (map nil (lambda (tree)
                               (new-edge (visit start tree) :epsilon end))
                         (cdr tree))
                    end))
                 ((is-op :closure tree)
                  (assert (= 2 (length tree)))
                  (let* ((start2 (incf state-counter))
                         (end (visit start2 (cadr tree))))
                    (new-edge start :epsilon start2)
                    (new-edge start :epsilon end)
                    (new-edge end :epsilon start2)
                    end))
                 (t (push (list start tree (incf state-counter)) edges)
                    state-counter)
                 ;(error "Unknown tree ~A" tree)
                 )))
      ;; visit the start
      (let ((final (visit 0 regex)))
        (make-fa edges 0 final)))))

;;; See Aho, 2nd p. 153-154. These closure computations are a
;;; functional variation thereof.

(defun nfa-e-closure (states mover
                      &optional (closure nil))
  "epsilon-closure of list STATES.
STATES: list of states
MOVER: fuction from (state-0 token) => (list state-1-0 state-1-1...)"
  (reduce (lambda (closure state)
            (if (find state closure)
                ;; already checked this state
                closure
                ;; new state
                (nfa-e-closure (funcall mover state 0)
                               mover
                               (cons state closure))))
          states :initial-value closure))

(defun nfa-move-e-closure (states z mover)
  "epsilon-closure of list STATES transitioned by token Z.
STATES: list of states
Z: token
MOVER: fuction from (state-0 token) => (list state-1-0 state-1-1...)"
  (reduce (lambda (closure state)
            (nfa-e-closure (funcall mover state z) mover closure))
          states :initial-value nil))

;; See Aho, 2nd p. 152
(defun nfa->dfa (nfa)
  "Convert an NFA to a DFA"
  (let ((mover (fa-mover nfa)))
    ;; d-states
    (let ((d-states (make-array 0 :adjustable t :fill-pointer t))
          (d-hash (make-hash-table :test #'equal))
          (d-edge))
      ;; start state
      (vector-push-extend (nfa-e-closure (fa-start nfa) mover) d-states)
      (setf (gethash (aref d-states 0) d-hash) 0)
      ;; subset construction
      (loop for mark = 0 then (1+ mark)
         while (< mark (length d-states))
         for ds = (aref d-states mark)
         do (progn
              (loop
                 for i-z from 1 below (length (fa-tokens nfa))
                 for u = (sort (nfa-move-e-closure ds i-z mover) #'<)
                 when u
                 do (progn
                      (assert (null (set-difference u (nfa-e-closure u mover))))
                      (when (not (gethash u d-hash))
                        (setf (gethash u d-hash) (length d-states))
                        (vector-push-extend u d-states))
                      (push (list mark (fa-token-name nfa i-z) (gethash u d-hash))
                            d-edge)))))
      ;; result
      (loop for ds across d-states
         do (assert (= 1 (loop for dt across d-states
                            counting (equal ds dt)))))
      (let ((fa (make-fa d-edge
                         0
                         (fa-subset-indices (fa-accept nfa) d-states))))
        (assert (dfap fa))
        fa))))

(defun fa-merge (fa i)
  "Merge all states equivalent to state at index i"
  (let ((is-accept (find i (fa-accept fa)))
        (succs (fa-successor-array fa))
        (state-map (make-array (length (fa-states fa)))))
    (map-into succs (curry-right #'sort #'symbol-list-compare) succs)
    (loop
       for j below (length succs)
       with k = -1
       do
         (setf (aref state-map j)
               (if (or (= i j)
                       (and (loop
                               for (i0 iz i1) in (aref succs i)
                               for (j0 jz j1) in (aref succs j)
                               always (and (= iz jz) (= i1 j1)))
                            (if is-accept
                                (find j (fa-accept fa))
                                (not (find j (fa-accept fa))))))
                   (progn  (incf k) i)
                   (- j k))))
    (fa-renumber fa :state-map state-map)))


(defun dfa-merge-start (dfa)
  "Merge all states equivalent to the start state"
  (assert (dfap dfa))
  (fa-merge dfa (car (fa-start dfa))))

;; Brzozowski's Algorithm
(defun fa-minimize-brzozowski (dfa)
  "Minimize a DFA or NFA via Brzozowski's Algorithm."
  (chain dfa
         #'fa-reverse #'nfa->dfa
         #'fa-reverse #'nfa->dfa
         #'dfa-merge-start))

;; Hopcroft's Algorithm
(defun dfa-minimize-hopcroft (dfa)
  "Minimize a DFA via Hopcroft's's Algorithm."
  ;; TODO: more efficient representation of rejecting state
  ;;       make implicit somehow
  (let* ((dfa (dfa-add-reject (fa-prune dfa)))
         (p (list (fa-accept dfa)
                  (set-difference (loop for i below
                                       (length (fa-states dfa))
                                     collect i)
                                  (fa-accept dfa)))))
    ;; build minimal states
    (loop
       with imover = (fa-inv-mover dfa)
       with q = (list (fa-accept dfa))
       while q
       for a = (pop q)
       do ;(print p)
         (loop for c below (length (fa-tokens dfa))
            ;; x: predecessors of a for token c
            for x = (reduce (lambda (x i-q)
                              (union x
                                     (if (< i-q 0)
                                         '(-1)
                                         (funcall imover i-q c))))
                            a :initial-value nil)
            when x
            do
                                        ;(format t "~&z: ~A x: ~A~%" c x)
              (loop for yy on p
                 for y = (car yy)
                 ;; subset of y transitioning to a on c
                 for i = (and (cdr y)
                              (intersection y x))

                 ;; subset of y transitioning to (not a) on c
                 for j = (and i (set-difference y x))
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
                   (rplacd yy (cons j (cdr yy))))))
    ;;(format t "~&~A" p)
    (assert (= (length (fa-states dfa))
               (loop for part in p summing (length part))))
    (assert (equal (sort (copy-list (reduce #'union p)) #'<)
                   (loop for i below (length (fa-states dfa)) collect i)))
    (let ((state-map (make-array (length (fa-states dfa)) :initial-element -1)))
      (reduce (lambda (i-new part)
                (map nil (lambda (i-old)
                           (assert (= -1 (aref state-map i-old)))
                           (setf (aref state-map i-old) i-new))
                     part)
                (1+ i-new))
              p :initial-value 0)
      (dotimes (i (length state-map))
        (assert (or (null (aref state-map i))
                    (>= (aref state-map i) 0))))
      (fa-prune (fa-renumber dfa :state-map state-map)))))


(defun dfa->string-matcher (dfa)
  "Return a (lambda (string)) predicate to test if dfa matches string."
  (let ((mover (fa-mover dfa))
        (start (car (fa-start dfa)))
        (accept (fa-accept dfa)))
    (lambda (string)
      (find (reduce (lambda (state x &aux (i-z (position x (fa-tokens dfa))))
                      (when (and state i-z)
                        (car (funcall mover state i-z))))
                    string :initial-value start)
            accept))))
