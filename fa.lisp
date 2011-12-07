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

;;; Utils

(defun intersectionp (a b &optional (test #'eql))
  (cond
    ((or (null a) (null b))
     nil)
    ((funcall test (car a) (car b))
     t)
    (t (or (intersectionp a (cdr b) test)
           (intersectionp (cdr a)  b test)))))


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
  (fa-map-edge-lists result-type
                     (lambda (e) (funcall function (first e) (second e) (third e)))
                     fa))


(defun fa-subset-indices (subset sequence)
  "return a list of indices of sequence elements that intersect with subset"
  (let ((i -1))
    (reduce (lambda (indices s)
              (incf i)
              (if (intersectionp subset s)
                  (cons i indices)
                  indices))
         sequence :initial-value nil)))


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
      (dolist (e edges)
        (destructuring-bind (q0 z q1) e
          (unless (gethash z token-hash)
            (setf (gethash z token-hash) (incf token-counter)))
          (push (list (new-state q0) (gethash z token-hash) (new-state q1))
                nedges)))
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
    (dotimes (i (length token-indices))
      (setf (aref token-old
                  (aref token-indices i))
            i))
    ;; sort state
    (if preserve-states
        ;; use original states
        (progn
          (dotimes (i (length state-indices)) (setf (aref state-indices i) i))
          (sort state-indices
                (lambda (a b) (symbol-list-compare  (aref (fa-states fa) a)
                                               (aref (fa-states fa) b))))
          (dotimes (i (length state-indices))
            (setf (aref state-old
                        (aref state-indices i))
                  i)))
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
                         (dotimes (i-z (length (fa-tokens fa)))
                           (visit (car (funcall mover i-q (aref token-old i-z))))))))
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

(defun fa-mover (fa)
  (let ((move (make-array (list (length (fa-states fa)) (length (fa-tokens fa)))
                          :initial-element nil)))
    (fa-map-edges nil (lambda (q0 z q1)
                        (push q1 (aref move q0 z)))
                  fa)
    (lambda (i-state i-token) (aref move i-state i-token))))

(defun fa-inv-mover (fa)
  (let ((move (make-array (list (length (fa-states fa)) (length (fa-tokens fa)))
                          :initial-element nil)))
    (fa-map-edges nil (lambda (q0 z q1)
                        (push q0 (aref move q1 z)))
                  fa)
    (lambda (i-state i-token) (aref move i-state i-token))))

(defun fa-successor-array (fa)
  "Make array indexed by original state of edge lists from that state."
  (let ((array (make-array (length (fa-states fa)) :initial-element nil)))
    (map nil (lambda (e) (push e (aref array (car e)))) (fa-edges fa))
    array))

(defun fa-predecessor-array (fa)
  "Make array indexed by final state of edge lists to that state."
  (let ((array (make-array (length (fa-states fa)) :initial-element nil)))
    (map nil (lambda (e) (push e (aref array (caddr e)))) (fa-edges fa))
    array))

(defun fa-dot (fa &optional output )
  "Graphviz output of dfa.
d: list of transitions '((state-0 token state-1)...)
final: list of accept states
start: start state
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
             ;; state labels
             (format s "~:{~&  ~A[label=\"~A\"];~}"
                     (loop for i below (length (fa-states fa))
                          collect (list i (state-label i))))
             ;; start state
             (when (fa-start fa)
               (format s "~&  start[shape=none];")
               (format s "~{~&  start -> ~A;~}"
                       (map 'list #'identity
                            (alexandria:ensure-list (fa-start fa)))))
             ;; accept state
             (format s "~{~&  ~A [ shape=doublecircle ];~}"
                     (map 'list #'identity
                          (alexandria:ensure-list (fa-accept fa))))
             (fa-map-edges nil (lambda (q0 z q1)
                                 (format s "~&  ~A -> ~A [label=\"~A\"];~%"
                                         q0 q1 (token-label z)))
                           fa)
             (format s "~&}~%"))
           (dot (ext)
             (let ((p (sb-ext:run-program "dot" (list (concatenate 'string "-T" ext))
                                          :wait nil :search t :input :stream :output output
                                          :if-output-exists :supersede)))
               (helper (sb-ext:process-input p))
               (close (sb-ext:process-input p))
               (sb-ext:process-wait p))))
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


(defun fa-renumber (fa &key state-map token-map sort)
  "Reorder, merge, or remove states and tokens in the FA.
STATE-MAP: (aref state-map old-index) => (or new-index nil)
TOKEN-MAP: (aref token-map old-index) => new-index"
  (labels ((new-state (i)
             (if state-map
                 (aref state-map i)
                 i))
           (new-token (i)
             (if token-map
                 (aref token-map i)
                 i))
           (state-translate (list)
             (mapcan (lambda (i) (when (new-state i)
                              (list (new-state i))))
                     list)))
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
                                      (loop with i = 0
                                         for x across token-map
                                         when x
                                         do (setf (aref tokens (aref token-map i))
                                                  (aref (fa-tokens fa) i)))
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
              (fa-accept fa) (sort (fa-start fa) #'<)))
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
  (let ((start 0)
        (counter 1)
        (edges nil))
    (labels ((visit (start tree &aux (end (incf counter)))
               ;; recursively visit the tree
               (cond
                 ((atom tree)
                  (push (list start tree end) edges)
                  end)
                 ((eq :concatenation (car tree))
                  (reduce #'visit (cdr tree)
                          :initial-value start))
                 ((eq :union (car tree))
                  (let ((end (incf counter)))
                    (mapcar (lambda (tree)
                              (push (list (visit start tree) :epsilon end)
                                    edges))
                          (cdr tree))
                    end))
                 ((eq :closure (car tree))
                  (assert (= 2 (length tree)))
                  (let* ((start2 (incf counter))
                         (end (visit start2 (cadr tree))))
                    (push (list start :epsilon start2) edges)
                    (push (list start :epsilon end) edges)
                    (push (list end :epsilon start2) edges)
                    end))
                 (t (error "Unknown tree ~A" tree)))))
      ;; visit the start
      (let ((final (visit start regex)))
        (make-fa edges start final)))))



(defun fa-index (edges)
  "Returns a hash table whose
- keys are the states
- values are lists of (token state-1)"
  (let ((h (make-hash-table :test #'equal)))
    (dolist (e edges)
      (push (cdr e) (gethash (car e) h)))
    h))



;;; See Aho, 2nd p. 153-154. These closure computations are a
;;; functional variation thereof.

(defun nfa-e-closure (states mover
                      &optional (closure nil))
  "epsilon-closure of list STATES.
STATES: list of states
MOVER: fuction from (state-0 token) => (list state-1-0 state-1-1...)"
  (if states
      (nfa-e-closure (cdr states)
                     mover
                     (if (find (car states) closure)
                         ;; already checked this state
                         closure
                         ;; new state
                         (nfa-e-closure (funcall mover (car states) 0)
                                        mover
                                        (cons (car states) closure))))
      ;; end of list
      closure))

(defun nfa-move-e-closure (states z mover)
  "epsilon-closure of list STATES transitioned by token Z.
STATES: list of states
Z: token
MOVER: fuction from (state-0 token) => (list state-1-0 state-1-1...)"
  (reduce (lambda (closure state)
            (nfa-e-closure (funcall mover state z) mover closure))
          states
          :initial-value nil))

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

;; Brzozowski's Algorithm
(defun dfa-minimize-brzozowski (dfa)
  (nfa->dfa (fa-reverse (nfa->dfa (fa-reverse dfa)))))

;; Hopcroft's Algorithm
(defun dfa-minimize-hopcroft (dfa)
  (let* ((dfa (fa-prune dfa)) ; pruning first should make things faster
         (p (cons (fa-accept dfa)
                  (let ((x (set-difference (loop for i below
                                                (length (fa-states dfa))
                                              collect i)
                                           (fa-accept dfa))))
                    (when x (list x))))))
    ;; build minimal states
    (loop
       with imover = (fa-inv-mover dfa)
       with q = (list (fa-accept dfa))
       while q
       for a = (pop q)
       do (loop for c below (length (fa-tokens dfa))
             ;; x: predecessors of A for token c
             for x = (reduce (lambda (x i-q)
                               (union x (funcall imover i-q c)))
                             a :initial-value nil)
             when x
             do (loop for yy on p
                   for y = (car yy)
                   for i = (and (cdr y)
                                (intersection y x))
                   for j = (and i (set-difference y x))
                   when (and i j)
                   do (when (< (length j) (length i))
                        (rotatef i j))  ; i is smaller
                     (loop for zz on q
                        when (equal y (car zz))
                        do (rplaca zz j))
                     (push i q)
                     (rplaca yy i)
                     (rplacd yy (cons j (cdr yy))))))
    (assert (= (length (fa-states dfa))
               (loop for part in p summing (length part))))
    (assert (equal (sort (copy-list (reduce #'union p)) #'<)
                   (loop for i below (length (fa-states dfa)) collect i)))
    (let ((state-map (make-array (length (fa-states dfa)) :initial-element -1)))
      (loop
         for part in p
         for i-new from 0
         do (loop for i-old in part
               do (progn (assert (= -1 (aref state-map i-old)))
                         (setf (aref state-map i-old) i-new))))
      (dotimes (i (length state-map))
        (assert (or (null (aref state-map i))
                    (>= (aref state-map i) 0))))
      (fa-renumber dfa :state-map state-map))))


(defun dfa->string-matcher (dfa)
  (let ((mover (fa-mover dfa))
        (start (car (fa-start dfa)))
        (accept (fa-accept dfa)))
    (lambda (string)
      (labels ((visit (state i)
                 (cond
                   ((null state) nil)
                   ((>= i (length string))
                    (find state accept))
                   (t (let ((i-z (position (aref string i)
                                           (fa-tokens dfa))))
                        (when i-z
                          (visit (car (funcall mover state
                                               i-z))
                                 (1+ i))))))))
        (visit start 0)))))

;; (defun nfa-e-close (n e-lookup)
;;   "Returns an array where each element is the epsilon-closure of the ith state.
;; N: number of states when numbered starting with 0
;; E-LOOKUP: function from state number to list epsilon moves"
;;   (let ((e-closure (make-array n :initial-element nil)))
;;     (labels ((lookup (i)
;;                (funcall e-lookup i))
;;              (e-close (i)
;;                (unless (aref e-closure i)
;;                  (setf (aref e-closure i)
;;                        (reduce (lambda (set i) (union set (e-close i)))
;;                                (lookup i)
;;                                :initial-value (list i))))
;;                (aref e-closure i)))
;;       (dotimes (i n)
;;         (e-close i)))
;;     e-closure))



;; (defun fa-numerize (fa)
;;   "Returns an edge list with all states and transitions as fixnums.
;; 0 is epsilon"
;;   (let ((state-hash (make-hash-table :test #'equal))
;;         (token-hash (make-hash-table :test #'equal))
;;         (state-counter -1)
;;         (token-counter 0)
;;         nedges)
;;     (setf (gethash :epsilon token-hash) 0)
;;     ;; start states
;;     (map nil (lambda (x)
;;                (setf (gethash x state-hash)
;;                      (incf state-counter)))
;;          (fa-start fa))
;;     ;; build hashes
;;     (dolist (e (fa-edges fa))
;;       (destructuring-bind (q0 z q1) e
;;         (unless (gethash q0 state-hash)
;;           (setf (gethash q0 state-hash) (incf state-counter)))
;;         (unless (gethash q1 state-hash)
;;           (setf (gethash q1 state-hash) (incf state-counter)))
;;         (unless (gethash z token-hash)
;;           (setf (gethash z token-hash) (incf token-counter)))
;;         (push (list (gethash q0 state-hash)
;;                     (gethash z token-hash)
;;                     (gethash q1 state-hash))
;;               nedges)))
;;     ;; map hashes to build new edges and mapping arrays
;;     (let ((state-array (make-array (1+ state-counter)))   ; i -> q
;;           (token-array (make-array (1+ token-counter))))  ; i -> z
;;       (maphash (lambda (n d) (setf (aref state-array d) n))
;;                state-hash)
;;       (maphash (lambda (n d) (setf (aref token-array d) n))
;;                token-hash)
;;       (values (make-fa nedges
;;                        (mapcar (lambda (x) (gethash x state-hash))
;;                                (fa-start fa))
;;                        (mapcar (lambda (x) (gethash x state-hash))
;;                                (fa-accept fa)))
;;               state-array token-array))))
