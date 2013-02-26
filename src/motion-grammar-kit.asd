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




(asdf:defsystem motion-grammar-kit
  :version "0.0.20130121"
  :description "Motion-Grammar Kit"
  :depends-on (:cl-ppcre :alexandria :sycamore :cffi :clpython :swank)
  :weakly-depends-on (:lisp-unit :cl-fuzz)
  :components ((:module "base"
                        :pathname "."
                        :components ((:file "package")
                                     (:file "util" :depends-on ("package"))
                                     (:file "pattern" :depends-on ("util"))
                                     (:file "matcher" :depends-on ("package"))
                                     (:file "set" :depends-on ("package" "util"))
                                     (:file "fa" :depends-on ("package" "set" "util"))
                                     (:file "petri" :depends-on ("fa"))
                                     (:file "regex" :depends-on ("fa"))
                                     (:file "grammar" :depends-on ("package" "fa" "set" "util"))
                                     (:file "parse" :depends-on ("package" "set" "util" "grammar"))
                                     (:file "graph" :depends-on ("package"))
                                     (:file "pda" :depends-on ("fa" "grammar"))
                                     (:file "fuzz" :depends-on ("fa" "set" "util"))
                                     (:file "search" :depends-on ("fa"))
                                     (:file "supervisor" :depends-on ("fa"))
                                     (:file "codegen" :depends-on ("fa" "grammar" "matcher"))
                                     (:file "threadpool" :depends-on ("package"))
                                     (:file "compiler" :depends-on ("fa" "regex"))
                                     (:file "bnf" :depends-on ("compiler"))))
               (:module "python"
                        :depends-on ("base")
                        :components ((:file "package")
                                     (:file "mangle" :depends-on ("package"))
                                     (:file "fixup" :depends-on ("mangle"))
                                     (:file "bind" :depends-on ("mangle" "fixup"))))))
