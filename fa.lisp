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


;;; DFA
;;;  -(Q, E, d, q0, F),
;;;    - Q: states
;;;    - E: alphabet
;;;    - d: Q*E :-> Q
;;;    - q0: start state
;;;    - F: accept state


(defun dfa-dot (d &key output final start)
  "Graphviz output of dfa.
d: list of transitions '((state-0 token state-1)...)
final: list of accept states
start: start state
output: output file, type determined by suffix (png,pdf,eps)"
  (labels ((token-label (name)
             (if (eq :epsilon name)
                 "&epsilon;" ; print epsilons in greek
                 name))
           (helper (s)
             (format s "~&digraph {~%")
             (when start
               (format s "~&  start[shape=none];")
               (format s "~&  start -> ~A;" start))
             (format s "~{~&  ~A [ shape=doublecircle ];~}" final)
             (map 'nil (lambda (x)
                         (format s "~&  ~A -> ~A [label=\"~A\"];~%"
                                 (first x) (third x) (token-label (second x))))
                  d)
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
                    (push (list end :epsilon start2) edges)
                    end))
                 (t (error "Unknown tree ~A" tree)))))
      ;; visit the start
      (let ((final (visit start regex)))
        (list edges start (list final))))))


(defun fa-index (edges)
  "Returns a hash table whose
- keys are the states
- values are lists of (token state-1)"
  (let ((h (make-hash-table :test #'equal)))
    (dolist (e edges)
      (push (cdr e) (gethash (car e) h)))
    h))


(defun fa-numerize (edges start)
  "Returns an edge list with all states and transitions as fixnums.
0 is epsilon"
  (let ((state-hash (make-hash-table :test #'equal))
        (token-hash (make-hash-table :test #'equal))
        (state-counter 0)
        (token-counter 0)
        nedges)
    (setf (gethash :epsilon token-hash) 0)
    (setf (gethash start state-hash) 0)
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
      (values nedges (gethash start state-hash)
              state-array token-array))))



(defun nfa->dfa (edges start)
  (multiple-value-bind (i-edges i-start i-states i-tokens) (fa-numerize edges start)
    (let ((e-closure (make-array (length i-states) :initial-element nil))
          (move (make-array (list (length i-states) (length i-tokens))
                            :initial-element nil))
          (et-closure (make-array (list (length i-states) (length i-tokens))
                                 :initial-element nil)))
      ;; index moves
      (loop for (q0 z q1) in i-edges
         do (push q1 (aref move q0 z)))
      ;; e-close of state i
      (labels ((e-close (i)
                 (unless (aref e-closure i)
                   (setf (aref e-closure i)
                         (reduce (lambda (set i) (union set (e-close i)))
                                 (aref move i 0)
                                 :initial-value (list i))))
                 (aref e-closure i)))
        (dotimes (i (length i-states))
          (e-close i)))
      ;; d-states
      (let ((d-states (make-array 0 :adjustable t :fill-pointer t))
            (d-hash (make-hash-table :test #'equal))
            (d-edge))
        (vector-push-extend (aref e-closure i-start) d-states)
        (setf (gethash (aref d-states 0) d-hash) 0)
        (princ d-states)
        (loop for mark = 0 then (1+ mark)
           while (< mark (length d-states))
           for ds = (aref d-states mark)
           do (progn
                (format t "~&d-states: ~A" ds)
                (loop
                   for i-z below (length i-tokens)
                   for u = (reduce (lambda (set state)
                                     (format t "~&  z: ~A i-state: ~A" i-z state)
                                     (union set
                                            (apply #'append
                                                   (mapcar (lambda (i) (aref e-closure i))
                                                           (aref move state i-z)))))
                                   ds :initial-value nil)
                   for up = (sort (copy-list U) #'<)
                   when up
                   do (progn
                        (when (not (gethash up d-hash))
                          (setf (gethash up d-hash) (length d-states))
                          (vector-push-extend up d-states))
                        (format t "~&   new d-state ~A ~A : ~A ~A"
                                mark i-z (gethash up d-hash) up)
                        (push (list mark (aref i-tokens i-z) (gethash up d-hash))
                              d-edge)))))
      (values (reverse d-edge) d-states move e-closure )))))
