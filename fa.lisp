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


;;; FA
;;;  -(states, tokens, edges, start, accept),
;;;    - edges: (list state-0 token state-1)

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


(defun make-fa-renumber (edges start accept)
  (let ((state-hash (make-hash-table :test #'equal))
        (token-hash (make-hash-table :test #'equal))
        (state-counter -1)
        (token-counter 0)
        nedges)
    (setf (gethash :epsilon token-hash) 0)
    ;; start/accept states
    (map nil (lambda (x)
               (setf (gethash x state-hash)
                     (incf state-counter)))
         (alexandria:ensure-list start))
    (map nil (lambda (x)
               (setf (gethash x state-hash)
                     (incf state-counter)))
         (alexandria:ensure-list accept))
    ;; build hashes
    (dolist (e edges)
      (destructuring-bind (q0 z q1) e
        (unless (gethash q0 state-hash)
          (setf (gethash q0 state-hash) (incf state-counter)))
        (unless (gethash q1 state-hash)
          (setf (gethash q1 state-hash) (incf state-counter)))
        (unless (gethash z token-hash)
          (setf (gethash z token-hash) (incf token-counter)))
        (push (list (gethash q0 state-hash)
                    (gethash z token-hash)
                    (gethash q1 state-hash))
              nedges)))
    ;; map hashes to build new edges and mapping arrays
    (let ((state-array (make-array (1+ state-counter)))   ; i -> q
          (token-array (make-array (1+ token-counter))))  ; i -> z
      (maphash (lambda (n d) (setf (aref state-array d) n))
               state-hash)
      (maphash (lambda (n d) (setf (aref token-array d) n))
               token-hash)
      (%make-fa :states state-array
                :tokens token-array
                :edges nedges
                :start (mapcar (lambda (x) (gethash x state-hash))
                               (alexandria:ensure-list start))
                :accept (mapcar (lambda (x) (gethash x state-hash))
                               (alexandria:ensure-list accept))))))


(defun make-fa (edges start accept &key (renumber-symbols t))
  (if renumber-symbols
      (make-fa-renumber edges start accept)
      (make-fa-simple edges start accept)))

(defun fa-mover (fa)
  (let ((move (make-array (list (length (fa-states fa)) (length (fa-tokens fa)))
                          :initial-element nil)))
    (loop for (q0 z q1) in (fa-edges fa)
       do (push q1 (aref move q0 z)))
    (lambda (i-state i-token) (aref move i-state i-token))))


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
             (when (fa-start fa)
               (format s "~&  start[shape=none];")
               (format s "~{~&  start -> ~A;~}"
                       (map 'list #'state-label (alexandria:ensure-list (fa-start fa)))))
             (format s "~{~&  ~A [ shape=doublecircle ];~}"
                     (map 'list #'state-label (alexandria:ensure-list (fa-accept fa))))
             (map 'nil (lambda (x)
                         (format s "~&  ~A -> ~A [label=\"~A\"];~%"
                                 (state-label (first x))
                                 (state-label (third x) )
                                 (token-label (second x))))
                  (fa-edges fa))
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
                      (when (not (gethash u d-hash))
                        (setf (gethash u d-hash) (length d-states))
                        (vector-push-extend u d-states))
                      (push (list mark (fa-token-name nfa i-z) (gethash u d-hash))
                            d-edge)))))
      ;; result
      (make-fa d-edge
               (loop ; start
                  for q across d-states
                  for i from 0
                  when (intersection q (fa-start nfa))
                  collect i)
               (loop ; accept
                  for q across d-states
                  for i from 0
                  when (intersection q (fa-accept nfa))
                  collect i)))))


;; (defun dfa-minimize (dfa)
;;   "Give state-minimized equivalent dfa"
;;   (let ((succs (make-array (length (fa-states dfa)) :initial-element nil))
;;         (preds (make-array (length (fa-states dfa)) :initial-element nil)))
;;     (loop for (q0 z q1) in (fa-edges dfa)
;;        do (progn (push q1 (aref succs q0))
;;                  (push q0 (aref preds q1))))
;;     (labels ((partition (group &optional (remaining group))
;;                (let ((x (car remaining)))

;;                (cond
;;                  (remaining
;;                   (if (subsetp (aref succs (car remaining))
;;                                group)
;;                       (partition group (cdr remaining)
;;                                  (cons (car remaining) group-1)
;;                                  group-2)
;;                       (partition group (cdr remaining)
;;                                  group-1 (cons (car remaining)
;;                                                group-2))))
;;                  ((null group-2) (list group-1))
;;                  ((null group-1) (list group-2))
;;                  (t (assert group-1)
;;                     (append (partition group-1)
;;                             (partition group-2))))))
;;       (append (partition (fa-accept dfa))
;;               (partition (set-difference (fa-states dfa)
;;                                          (fa-accept dfa)))))))


      ;;(map 'vector (lambda (s)
                     ;;(map 'list (lambda (i) (aref i-states i)) s))
           ;;succs))))


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
