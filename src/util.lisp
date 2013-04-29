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

(in-package :motion-grammar-kit)

;;(declaim (optimize (speed 3) (safety 0)))

(defun intersectionp (a b &optional (test #'eql))
  (map nil (lambda (a)
             (map nil (lambda (b)
                        (when (funcall test a b)
                          (return-from intersectionp t)))
                  b))
       a))

(defun multiple-value-reduce (function sequence &key initial-value-list)
  (let ((result initial-value-list))
    (map nil
         (lambda (&rest rest)
           (setq result (multiple-value-list
                         (apply function (append result rest)))))
         sequence)
    (apply #'values result)))

(defun tree-compact (tree)
  "Replace common subtrees with EQ objects."
  (let ((hash (make-hash-table :test #'equal)))
    (labels ((rec (symbol)
               (etypecase symbol
                 (atom
                  (if (gethash symbol hash)
                      (gethash symbol hash)
                      (setf (gethash symbol hash) symbol)))
                 (cons
                  (map nil #'rec symbol)
                  (if (gethash symbol hash)
                      (gethash symbol hash)
                      (setf (gethash symbol hash)
                            (cons (rec (car symbol))
                                  (rec (cdr symbol)))))))))
      (rec tree))))

(defun random-whole (count)
  (check-type count (integer 1 #.most-positive-fixnum))
  (1+ (random (1- count))))


(defmacro whereas* (bindings &body body)
  "Evaluate bindings in turn, continuing only if the value is true.
If all bindings are true, evaluate body."
  (destructuring-bind ((var value) &rest other-bindings) bindings
    `(let ((,var ,value))
       (when ,var
         ,@(if other-bindings
               `((whereas* ,other-bindings ,@body))
               body)))))

(defmacro whereas (bindings &body body)
  (let* ((vars (mapcar #'first bindings))
         (genvars (mapcar (lambda (v) (gensym (string v))) vars))
         (genbindings (mapcar (lambda (genvar binding) (cons genvar (cdr binding)))
                              genvars bindings))
         (rebindings (mapcar #'list vars genvars)))
    `(whereas* ,genbindings
       (let ,rebindings
         ,@body))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; HIGHER ORDER FUNCTIONS ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; the CL-standards are a bit clumsy for this

;; Use Alexandria's currying

;; (defun curry (function arg0)
;;   "Return a new unary function which applies arg0 as leftmost argument of FUNCTION."
;;   (declare (type function function))
;;   (lambda (arg1) (funcall function arg0 arg1)))


;; (defun curry-list (function arg0 &rest more-args)
;;   "Return a new vararg function which applies ARG0 and MORE-ARGS as leftmost arguments of FUNCTION."
;;   (declare (type function function))
;;   (let ((helper (lambda (&rest final-args)
;;                    (apply function arg0 final-args))))
;;     (if more-args
;;         (apply #'curry-list
;;                helper
;;                more-args)
;;         helper)))

;; (defun curry-right (function arg1)
;;   "Return a new unary function which applies arg0 as rightmost argument of FUNCTION."
;;   (declare (type function function))
;;   (lambda (arg0) (funcall function arg0 arg1)))

(defun chain (value &rest functions)
  (if functions
      (apply #'chain
             (funcall (the function (car functions)) value)
             (cdr functions))
      value))

(defun fold (function initial-value &rest lists)
  (declare (type function function))
  (let ((value initial-value))
    (apply #'map nil
           (lambda (&rest args)
             (setq value (apply function value args)))
           lists)
    value))

(defmacro thunk (&body body)
  "A lambda with no arguments."
  `(lambda () ,@body))

;;;;;;;;;;;;;;;;;;;;
;;; Comparisions ;;;
;;;;;;;;;;;;;;;;;;;;

(defmacro or-compare-2 (compare-exp-1 compare-exp-2)
  "Short-circuit evaluatation of arguments, returning the first one that is nonzero."
  (alexandria:with-gensyms (sym1)
   `(let ((,sym1 ,compare-exp-1))
      (declare (fixnum ,sym1))
      (if (zerop ,sym1)
          ,compare-exp-2
          ,sym1))))

(defmacro or-compare (&rest vals)
  "Short-circuit evaluatation of arguments, returning the first one that is nonzero."
  (cond
    ((null vals) 0)
    ((null (cdr vals)) (car vals))
    (t `(or-compare-2 ,(car vals)
                      (or-compare ,@(cdr vals))))))

(defun string-compare (a b)
  (declare (type string a b))
  (cond ((string< a b) -1)
        ((string> a b) 1)
        (t 0)))

(declaim (inline fixnum-compare))
(defun fixnum-compare (a b)
  (declare (type fixnum a b))
  (cond ((< a b) -1)
        ((> a b) 1)
        (t 0)))

;;;;;;;;;;;;;;;
;;; Strings ;;;
;;;;;;;;;;;;;;;

(defun string-upcase-p (str)
  (string= str (string-upcase str)))

(defun string-downcase-p (str)
  (string= str (string-downcase str)))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; GENERALIZED SYMBOLS ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(defun gsymbol-name (gsym)
  (format nil "~A" gsym))

(defun gsymbol-gen (gsym &optional (unique (gensym)))
  (cons gsym unique))

(defun gsymbol-compare-atom (a b)
  (etypecase a
    (fixnum
     (etypecase b
       (fixnum (fixnum-compare a b))
       (character 1)
       (string 1)
       (symbol 1)
       (tree-set 1)))
    (character
     (etypecase b
       (fixnum -1)
       (character (gsymbol-compare (char-code a) (char-code b)))
       (string 1)
       (symbol 1)
       (tree-set 1)))
    (string
     (etypecase b
       (fixnum -1)
       (character -1)
       (string (string-compare a b))
       (symbol 1)
       (tree-set 1)))
    (symbol
     (etypecase b
       (fixnum -1)
       (character -1)
       (string -1)
       (symbol (cond ((string< a b) -1)
                     ((string> a b) 1)
                     (t 0)))
       (tree-set 1)))
    (tree-set
     (etypecase b
       (fixnum -1)
       (character -1)
       (string -1)
       (symbol -1)
       (tree-set (tree-set-compare a b))))))

(defun gsymbol-compare (a b)
  (etypecase a
    (null (if b -1 0))
    (atom (etypecase b
            (null 1)
            (atom (gsymbol-compare-atom a b))
            (list -1)))
    (cons
     (etypecase b
       (atom 1)
       (list (or-compare (gsymbol-compare (car a) (car b))
                         (gsymbol-compare (cdr a) (cdr b))))))))



;; (defun gsymbol-compare (a b)
;;   (declare (type (or character list symbol string number)
;;                  a b))
;;   (cond
;;     ((equalp a b) 0)
;;     ((and a (null b))
;;      1)
;;     ((and (null a) b)
;;      -1)
;;     ((and (listp a)
;;           (listp b))
;;      (let ((c (gsymbol-compare (car a) (car b))))
;;        (if (zerop c)
;;            (gsymbol-compare (cdr a) (cdr b))
;;            c)))
;;     ((listp a) 1)
;;     ((listp b) -1)
;;     ((and (numberp a) (numberp b))
;;      (- a b))
;;     ((numberp a) -1)
;;     ((numberp b) 1)
;;     ((string< (string a) (string b))
;;      -1)
;;     (t 1)))

(defun gsymbol-predicate (a b)
  (< (the fixnum (gsymbol-compare a b)) 0))

(defun gsymbol-nsort (list)
  (sort list #'gsymbol-predicate))

(defun gsymbol-sort (list)
  (gsymbol-nsort (copy-list list)))


(defun epsilon-p (symbol)
  (or (null symbol)
      (eq symbol :epsilon)
      (equal symbol '(:epsilon))))

(defun gsymbol-equal (a b)
  (= 0 (gsymbol-compare a b)))

;;;;;;;;;;
;; TRIE ;;
;;;;;;;;;;


(defun common-prefix (a b &optional (test #'equal))
  (when (and a b (funcall test (car a) (car b)))
    (cons (car a) (common-prefix (cdr a) (cdr b) test))))

(defun prefix-split (a b &optional (test #'equal))
  "Returns (values common-prefix suffix-a suffix-b)"
  (if (and a b (funcall test (car a) (car b)))
      (multiple-value-bind (prefix suffix-a suffix-b)
          (prefix-split (cdr a) (cdr b) test)
        (values (cons (car a) prefix) suffix-a suffix-b))
      (values nil a b)))

(defun prefix-p (prefix list &optional (test #'equal))
  (if (null prefix)
      t
      (and (funcall test (car prefix) (car list))
           (prefix-p (cdr prefix) (cdr list) test))))

;; Trie: (list (prefix . trie) ...)
(defun trie-insert (trie value)
  ;;(print trie)
  (flet ((next () (cons (car trie) (trie-insert (cdr trie) value))))
    (cond
      ((null trie)
       (list (list value nil)))
      ((null value)
       (cons nil trie))
      ((null (car trie))
       (next))
      (t
       (destructuring-bind ((prefix . suffixes) &rest rest) trie
         ;;(print suffixes)
         (multiple-value-bind (new-prefix value-suffix old-prefix-suffix)
             (prefix-split value prefix)
           (if new-prefix
               ;; split this node
               (cons (cons new-prefix
                           (cond
                             ;; value equals old-prefix
                             ((and (null value-suffix) (null old-prefix-suffix))
                              `(nil ,@suffixes))
                             ;; value is a prefix of old-prefix
                             ((null value-suffix)
                              `(nil (,old-prefix-suffix . ,suffixes)))
                             ;; old-prefix is a prefix of value
                             ((null old-prefix-suffix)
                              (trie-insert suffixes value-suffix))
                             ;; unique members of value and old-prefix
                             (t
                              `((,value-suffix nil) (,old-prefix-suffix . ,suffixes)))))
                     rest)
               ;; next
               (next))))))))

(defun map-trie (function trie)
  (labels ((visit (trie accum)
             ;;(format t "~&t: ~A, a: ~A~&" trie accum)
             (if (car trie)
                 (destructuring-bind (prefix &rest suffixes) (car trie)
                   (visit suffixes (cons prefix accum)))
                 (funcall function (apply #'append (reverse accum))))
             (when (cdr trie)
               (visit (cdr trie) accum))))
    (when trie
      (visit trie nil))))



;;;;;;;;;
;; I/O ;;
;;;;;;;;;


(defun read-csv (file)
  (with-open-file (s file :direction :input)
    (loop
       with scanner = (ppcre:create-scanner " *, *")
       for line = (read-line s nil nil)
       while line
       collect (ppcre:split scanner line))))

(defun read-csv-matrix (file &optional parse-element)
  (let ((csv (read-csv file)))
    (let ((matrix (make-array (list (length csv) (length (car csv))))))
      (loop
         for row in csv
         for i from 0
         do (loop
               for x in row
               for px = (if parse-element (funcall parse-element x) x)
               for j from 0
               do (setf (aref matrix i j) px)))
      matrix)))


(defun output-function (function &optional output)
  (declare (type function function))
  (cond
    ((streamp output)
     (funcall function output))
    ((null output)
     (with-output-to-string (s) (output-function function s)))
    ((eq output t)
     (output-function function *standard-output*))
    ((or (stringp output)
         (pathnamep output))
     (with-open-file (s output :if-exists  :supersede :if-does-not-exist :create :direction :output)
       (output-function function s)))
    (t (error "Unknown output: ~A" output))))

#+sbcl
(defun output-dot-file (program output function lang)
  "Run `dot' on the output of FUNCTION.
OUTPUT: output filename
FUNCTION: (lambda (stream)) => nil, prints dot on STREAM
LANG: language output for dot, (or pdf ps eps png)"
  (let ((p (sb-ext:run-program program (list (concatenate 'string "-T" lang))
                               :wait nil :search t :input :stream :output output
                               :if-output-exists :supersede)))
    (unwind-protect
         (funcall function (sb-ext:process-input p))
      (close (sb-ext:process-input p))
      (sb-ext:process-wait p)
      (sb-ext:process-close p))))


(defun object->string (object)
  (let ((str (princ-to-string object))
        (vec (make-array 0 :adjustable t :fill-pointer t)))
    (dotimes (i (length str))
      (if (eql (aref str i) #\Newline)
          (progn (vector-push-extend #\\ vec)
                 (vector-push-extend #\n vec))
          (vector-push-extend (aref str i) vec)))
    (coerce vec 'string)))

(defun dot-gsymbol (gsymbol)
  (case gsymbol
    (:alpha "&alpha;")
    (:beta "&beta;")
    (:gamma "&gamma;")
    (:delta "&delta;")
    (:epsilon "&epsilon;")
    (:zeta "&zeta;")
    (:eta "&eta;")
    (:theta "&theta;")
    (:iota "&iota;")
    (:kappa "&kappa;")
    (:lambda "&lambda;")
    (:mu "&mu;")
    (:nu "&nu;")
    (:xi "&xi;")
    (:omicron "&omicron;")
    (:pi "&pi;")
    (:rho "&rho;")
    (:sigma "&sigma;")
    (:tau "&tau;")
    (:upsilon "&upsilon;")
    (:phi "&phi;")
    (:chi "&chi;")
    (:omega "&omega;")
    (t (object->string gsymbol))))

(defun dot-options (s &key
                    rankdir
                    font-size
                    node-shape
                    (node-font-size font-size))
  (when node-font-size
    (format s "  ~&node[fontsize=~D]~%" node-font-size))
  (when node-shape
    (format s "  ~&node[shape=~A]~%" node-shape))
  (when rankdir
    (format s "~&rankdir=\"~A\";~%" rankdir)))

(defun output-dot (output function &key
                   (program "dot")
                   (lang (and (stringp output) (car (last (ppcre:split "\\." output))))))
  "Produce graphiz output, dispatching on type of OUTPUT.
OUTPUT:  (or filename stream t nil)
FUNCTION: (lambda (stream)) => nil, prints dot text on STREAM
LANG: language output for dot, (or pdf ps eps png)"
  (if (and (or (pathnamep output)
               (stringp output))
           (not (string= lang "dot")))
      (output-dot-file program output function lang)
      (output-function function output)))
