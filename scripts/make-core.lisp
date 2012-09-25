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
(loop for pathname in (list "./src/"
                            (merge-pathnames ".asdf/systems/"
                                             (user-homedir-pathname))
                            (merge-pathnames ".sbcl/systems/"
                                             (user-homedir-pathname)))

   do (when (probe-file pathname)
        (pushnew pathname asdf:*central-registry* :test #'equal)))

;; Load Motion-Grammar-Kit
(progn
  (if (find-package :quicklisp)
      (funcall (intern "QUICKLOAD" :ql) :motion-grammar-kit)
      (require :motion-grammar-kit)))

;; Make load motion-grammar-kit-ach
(when (eq :yes mg-conf::*ac-have-ach*)
  (if (find-package :quicklisp)
      (funcall (intern "QUICKLOAD" :ql) :motion-grammar-kit-ach)
      (require :motion-grammar-kit-ach)))


;; save core
(if (find-package :motion-grammar-kit)
    (progn
      #+sbcl
      (sb-ext:save-lisp-and-die "motgramlisp"
                                :executable t)
      #+ccl
      (ccl:save-application "motgramlisp" :prepend-kernel t))
    (progn
      #+sbcl
      (sb-ext:quit :code -1)
      #+ccl
      (ccl:quit -1)))
