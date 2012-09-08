;; Copyright (c) 2010-2012, Georgia Tech Research Corporation
;; All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;;
;; * Redistributions of source code must retain the above copyright
;;   notice, this list of conditions and the following disclaimer.
;;
;; * Redistributions in binary form must reproduce the above copyright
;;   notice, this list of conditions and the following disclaimer in
;;   the documentation and/or other materials provided with the
;;   distribution.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
;; FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
;; COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
;; INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
;; (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
;; SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
;; HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
;; STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
;; ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
;; OF THE POSSIBILITY OF SUCH DAMAGE.

(in-package :motion-grammar-kit)

;;;;;;;;;;;;;
;;; SPECS ;;;
;;;;;;;;;;;;;

;;; Mini-language for matching and binding symbol trees

;;; spec matching
;; spec-list := (spec* \(&rest rest?\) )
;; spec := NAME | KEYWORD | (spec-list) (NAME 'is type-spec)
;; type-spec := SYMBOL | 'or type-specs* | 'function SYMBOL | 'spec spec
;;
;; example: (:add a b)


;; TODO: compile this rather than iterate through a list

(defun spec-match (spec exp)
  (cond
    ((eq spec 'is) (error "Invalid spec: ~A" spec))
    ((null spec) (null exp))
    ((keywordp spec) (eq spec exp)) ; spec := KEYWORD
    ((symbolp spec) t)              ; spec := NAME
    ((consp spec)
     (cond
       ((eq (car spec) '&rest)
        (assert (= 2 (length spec)) () "Invalid spec ~A" spec )
        (listp exp))
       ((eq (car spec) '&ignore)
        (spec-match (cdr spec) exp))
       ((eq (car spec) '&optional)
        (listp exp))
       ((and (listp (cdr spec))
             (eq (cadr spec) 'is))        ; spec := (NAME is type-spec)
        (spec-match-type (cddr spec) exp))
       (t                           ; spec := (spec-list)
        (and (consp exp)
             (spec-match (car spec) (car exp))
             (spec-match (cdr spec) (cdr exp))))))
    (t (error "Invalid spec: ~A" spec))))


(defun spec-match-type (type-spec elt)
  ;;(format t "~&type-spec: ~A~&" type-spec)
  (cond
    ((symbolp type-spec)
     ;; type-spec := symbol
     (eq type-spec elt))
    ((and (consp type-spec)
          (eq 'spec (car type-spec)))
     ;; type-spec := 'spec spec
     (assert (= (length type-spec) 2))
     (spec-match (cadr type-spec) elt))
    ((and (consp type-spec)
          (eq 'or (car type-spec)))
     ;; type-spec := (or type-specs*)
     (find-if (lambda (type-spec) (spec-match-type type-spec elt))
              (cdr type-spec)))
    ((and (consp type-spec)
          (eq 'not (car type-spec)))
     ;; type-spec := (not type-spec)
     (assert (= (length type-spec) 2))
     (not (spec-match-type (cadr type-spec) elt)))
    ((and (consp type-spec)
          (consp (car type-spec)))
     (assert (= (length type-spec) 1))
     (spec-match-type (car type-spec) elt))
    ((and (consp type-spec)
          (eq 'function (car type-spec)))
     (funcall (symbol-function (cadr type-spec))
              elt))
    ((and (consp type-spec)
          (symbolp (car type-spec)))
     (assert (= (length type-spec) 1))
     (eq (car type-spec) elt))
     ;; type-spec := symbol-function function
    (t (error "Unknown type-spec: ~S" type-spec))))

(defun spec-var (spec &optional ignore)
  ;(print spec)
  (cond
    ((null spec) nil)
    ((keywordp spec) (let ((g (gensym (string spec))))
                       (values g (list g))))
    ((symbolp spec) (values spec
                            (when (and ignore
                                       (not (eq spec '&rest))
                                       (not (eq spec '&optional)))
                              (list spec))))
    ((consp spec)
     (cond
       ((eq '&ignore (car spec))
        (spec-var (cdr spec) t))
       ((and (symbolp (car spec))
             (eq 'is (cadr spec)))
        (values (car spec)
                (when ignore (list (car spec)))))
       ((eq (cadr spec) 'is)
        (spec-var (car spec) ignore))
       (t
     (multiple-value-bind (car-vars car-ignore)
         (spec-var (car spec) ignore)
       (multiple-value-bind (cdr-vars cdr-ignore) (spec-var (cdr spec) ignore)
         (values (cons car-vars cdr-vars)
                 (append (alexandria:ensure-list car-ignore) cdr-ignore)))))))
    (t (error "Unknown spec: ~A" spec))))

(defmacro destructuring-bind-spec (orig-spec exp &body body)
  (multiple-value-bind (spec ignore-syms)
      (spec-var orig-spec)
    (alexandria:with-gensyms (tree)
      `(let ((,tree ,exp))
         (assert (spec-match ',orig-spec ,tree) () "Spec mismatch")
         (,@(if (atom spec)
                `(let ((,spec ,tree)))
                `(destructuring-bind ,spec ,tree))
            (declare (ignore ,@ignore-syms))
            ,@body)))))

(defmacro spec-case (keyform &body cases)
  (alexandria:with-gensyms (e)
  `(let ((,e ,keyform))
     (cond ,@(mapcar (lambda (c)
                      `((spec-match ',(car c) ,e)
                        (destructuring-bind-spec ,(car c) ,e
                          ,@(cdr c))))
                    cases)))))
