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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Some Environment Setup to make this script probably just work ;;;
;;; If running from Slime etc, this should not be necessary       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Try to load Quicklisp or ASDF
(unless (find-package :quicklisp)
  (let ((ql (find-if #'probe-file
                     (map 'list (lambda (setup) (merge-pathnames setup (user-homedir-pathname)))
                          '("quicklisp/setup.lisp" ".quicklisp/setup.lisp" "Quicklisp/setup.lisp")))))
    (cond
      (ql (load ql))
      ((not (find-package :asdf))
       (require :asdf)))))

;; Guess where some ASDF files lives
(loop for pathname in (list "../src/"
                            (merge-pathnames ".asdf/systems/"
                                             (user-homedir-pathname))
                            (merge-pathnames ".sbcl/systems/"
                                             (user-homedir-pathname)))

   do (when (probe-file pathname)
        (pushnew pathname asdf:*central-registry* :test #'equal)))

;; Load Motion-Grammar-Kit
(if (find-package :quicklisp)
    (funcall (intern "QUICKLOAD" :ql) :motion-grammar-kit)
    (require :motion-grammar-kit))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; End Environment Setup ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Now the actual example code ;;;

;; Simple C predictive parser
(let ((grammar '((s not-done load s unload)
                 (s done))))
  (mg::grammar->c-predictive-parser
   grammar
   :function-name "load_parse"
   :context-type "context_load_t *"
   :output "load_parse.c"))

;; Supervised C predictive parser
(let* ((grammar '((s not-done s-p)
                  (s done)
                  (s-p load-a s unload)
                  (s-p load-b s unload)))
       (terminals (mg::grammar-terminals grammar))
       (super (mg:regex->dfa (mg:regex-sweeten `(:concatenation (:closure (:concatenation not-done load-a
                                                                                          not-done load-b))
                                                                done
                                                                (:closure :.))
                                               terminals)
                             terminals)))
  (mg::supervisor-table-output super
                               "super.dat")
  (mg::grammar->c-supervised-predictive-parser
   grammar
   :function-name "super_load_parse"
   :context-type "context_load_t *"
   :output "super_load_parse.c"))
