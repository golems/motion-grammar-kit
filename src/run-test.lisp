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



;; Try to load quicklisp
(unless (find-package :quicklisp)
  (let ((ql0 (merge-pathnames "quicklisp/setup.lisp"
                              (user-homedir-pathname)))
        (ql1 (merge-pathnames ".quicklisp/setup.lisp"
                              (user-homedir-pathname))))
    (cond
      ((probe-file ql0)
       (load ql0))
      ((probe-file ql1)
       (load ql1)))))

;; Try to load ASDF load
(require :asdf)

;; Try to add standard asdf place
(pushnew (merge-pathnames ".asdf/systems/"
           (user-homedir-pathname))
         asdf:*central-registry*
         :test #'equal)
(pushnew (make-pathname :directory '(:relative "."))
         asdf:*central-registry*
         :test #'equal)

;; Load necessary packages
(asdf:operate 'asdf:load-op :motion-grammar-kit)
(asdf:operate 'asdf:load-op :lisp-unit)

;; Load tests
(load "test.lisp")

;; Run the tests
(in-package :motion-grammar-kit)

(lisp-unit:run-tests :ALL)


#+sbcl
(sb-ext:quit)

#+ccl
sts(ccl:quit)
