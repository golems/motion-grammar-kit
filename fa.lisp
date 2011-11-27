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



(defun dfa-dot (d &key file)
  (labels ((helper (s)
             (format s "~&digraph {~%")
             (map 'nil (lambda (x)
                         (format s "~&  ~A -> ~A [label=\"~A\"];~%"
                                 (first x) (third x) (second x)))
                  d)
             (format s "~&}~%"))
           (dot (ext)
             (let ((p (sb-ext:run-program "dot" (list (concatenate 'string "-T" ext))
                                          :wait nil :search t :input :stream :output file
                                          :if-output-exists :supersede)))
               (helper (sb-ext:process-input p))
               (close (sb-ext:process-input p))
               (sb-ext:process-wait p))))
    (cond
      ((and (stringp file) (ppcre:scan ".png$" file))
       (dot "png"))
      ((and (stringp file) (ppcre:scan ".ps$" file))
       (dot "ps"))
      ((and (stringp file) (ppcre:scan ".eps$" file))
       (dot "eps"))
      ((and (stringp file) (ppcre:scan ".pdf$" file))
       (dot "pdf"))
      ((streamp file)
       (helper file))
      (t
       (with-output-to-string (s)
         (helper s))))))

