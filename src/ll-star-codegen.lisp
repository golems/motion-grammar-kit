(defun grammar->c-ll-star-parser (grammar &key (output "/tmp/ll-star.c"))
  (let* ((atn (grammar->ATN grammar))
         (nonterminals (grammar-nonterminals grammar))
         (dfas (map 'list (curry #'atn-create-dfa atn) nonterminals))
         (pred-dfas (mapcar (curry #'fa-rewrite-edges (lambda (edge) (if (mutatorp edge) edge (list 'pred edge)))) dfas))
         (predicates (extract-predicates grammar pred-dfas))
         (mutators (remove-duplicates (map 'list #'terminal-get-effect (remove-if-not #'mutatorp (apply #'append grammar))))))
    (with-c-output output
      (c-indent-format "/*************************/~&")
      (c-indent-format "/* LL-STAR MOTION PARSER */~&")
      (c-indent-format "/*************************/~&")
      (emit-observation-type predicates)
      (emit-driver)
      (map-finite-set nil #'emit-mutator-stub mutators)
      (emit-make-observation-stub)
      (map-finite-set nil #'emit-predicate predicates)
      (map-finite-set nil #'emit-extern-nonterminal-declaration nonterminals)
      (grammar-map-numbered nil #'emit-production grammar)
      (loop for pred-dfa in pred-dfas for nonterminal in nonterminals do (emit-automata pred-dfa nonterminal))
      (grammar-map-grouped nil #'emit-nonterminal grammar)
      (emit-start-nonterminal-reference (grammar-start-nonterminal grammar))
      (emit-main))))

(defun symbol-c-value (sym)
  (cond
    ((nonterminalp sym) (csymbol sym "nonterminal_"))
    ((mutatorp sym) (csymbol (second sym) "mutator_"))
    ((predicatep sym) (csymbol (second sym) "predicate_"))
    ((kleenep sym) (csymbol (second sym) "predicate_"))
    (t (error (format nil "Unknown symbol type ~A" sym)))))

(defun symbol-c-pvalue (sym)
  (concatenate 'string "&" (symbol-c-value sym)))

(defun symbol-c-type (sym)
  (cond
    ((nonterminalp sym) "NONTERMINAL")
    ((mutatorp sym) "MUTATOR")
    ((predicatep sym) "PREDICATE")
    ((kleenep sym) "KLEENE")
    (t (error (format nil "Unknown symbol type ~A" sym)))))

(defun c-list (elements)
  (format nil  "{ ~{~A~^, ~} }" elements))

(defun c-list-map (function elements)
  (c-list (map 'list function elements)))

(defun emit-driver ()
  (c-indent-format (alexandria:read-file-into-string "src/driver.c")))

(defun extract-predicates (grammar dfas)
  (let ((all-symbols (remove-duplicates (apply #'append `(,@grammar ,@(mapcar (compose #'finite-set-list #'fa-terminals) dfas))) :test #'gsymbol-equal)))
    (mapcar #'terminal-get-prop (remove-if-not #'has-proposition-p all-symbols))))

(defun emit-observation-type (predicates)
  (let ((variables (fold #'finite-set-union (make-finite-set) (finite-set-map 'list #'logic-variables predicates))))
    (c-indent-format "typedef struct observation {~&    ~{int ~A;~^~&    ~}~&} observation_t;" (mapcar #'csymbol (finite-set-list variables)))))

(defun emit-predicate (predicate)
  (c-indent-format "bool_t predicate_~A (observation_t obs) {" (csymbol predicate) )
  (c-format-return (c-prop (prop->cnf predicate)))
  (c-indent-format "}"))

(defun emit-extern-nonterminal-declaration (nonterminal)
  (c-indent-format "extern nonterminal_t ~A;~&" (symbol-c-value nonterminal)))

(defun c-prop (e)
  ;; The rule is that you should parenthethize your children
  ;; Atm we assume cnf form (only not and and or)
  (cond
    ((atom e) (concatenate 'string "obs." (csymbol e)))
    ((equal 'not (car e)) (format nil "!(~A)" (c-prop (second e))))
    ((= 2 (length e)) (c-prop (second e)))
    (t (format nil "(~A) ~A (~A)" (c-prop (second e)) (if (equal 'and (first e)) "&&" "||") (c-prop (third e))))))

(defun emit-main ()
  (c-indent-format "int main () {~&")
  (c-format-statement "  run_nonterminal( start_nonterminal )")
  (c-format-statement "  printf(\"Congratulations, your motion grammar completed successfully! :)\\n\"  ) " )
  (c-indent-format "}"))

(defun emit-mutator-stub (mutator)
  (c-indent-format "void ~A () {~&" (symbol-c-value (list 'MU mutator)))
  (c-indent-format "  // XXX: STUB! ADD YOUR IMPLEMENTATION HERE!~&")
  (c-indent-format "}"))

(defun emit-make-observation-stub ()
  (c-indent-format "observation_t make_observation () {~&")
  (c-indent-format "  // XXX: STUB! ADD YOUR IMPLEMENTATION HERE!~&")
  (c-indent-format "}"))

(defun emit-start-nonterminal-reference (head)
  (c-indent-format "nonterminal_t *start_nonterminal = &nonterminal_~A;~&" (csymbol head)))

(defun addstring (str fun)
  (lambda (x) (concatenate 'string str (funcall fun x))))

(defun emit-production (head body id)
  (c-indent-format "production_t production_~A = { ~A, ~A, ~A };// Belonging to nonterminal ~A~&" id
                   (length body)
                   (c-list-map (addstring "(untyped_t)" #'symbol-c-pvalue) body)
                   (c-list-map #'symbol-c-type body)
                   head))

(defun emit-automata (pred-dfa nonterminal)
  (let* ((fa pred-dfa)
         (num-states (finite-set-length (fa-states fa)))
         (fun (fa-successors fa))
         (state-sequence (map-finite-set 'list #'identity (fa-states fa)))
         (edges-sequence (map-finite-set 'list fun state-sequence))
         (edges (apply #'append edges-sequence))
         (state-ids (make-hash-table)))
    (loop for i below num-states for state in state-sequence do (setf (gethash state state-ids) i))
    (c-indent-format "dfa_t automata_~A = { ~A, ~A, ~A, ~A, ~A, ~A, ~A };~&" (csymbol nonterminal)
                     num-states
                     ;(gethash "s0" state-ids) ;; TODO WTH does this not work???
                     (position "s0" state-sequence :test #'string=)
                     (c-list (loop for state in state-sequence
                                   if (finite-set-inp state (fa-accept fa))
                                   collect (format nil "&production_~A" (parse-integer state :start (length "final=>"))) else collect "NULL"))
                     (c-list (cons 0 (loop for edges in edges-sequence
                                           summing (length edges) into acc
                                           collecting acc)))
                     (c-list-map (addstring "(untyped_t)" (compose #'symbol-c-pvalue #'first)) edges)
                     (c-list-map (compose #'symbol-c-type #'first) edges)
                     (c-list-map (compose (rcurry #'position state-sequence :test #'string=) #'second) edges)
                     ;(c-list-map (compose (rcurry #'gethash state-ids) #'second) edges) ;; TODO this is also buggy ...
                     )))

(defun emit-nonterminal (head bodys)
  (declare (ignore bodys))
  (c-indent-format "nonterminal_t ~A = { \&automata_~A };~&" (symbol-c-value head) (csymbol head)))

(defun weather-grammar ()
  '((A (pred is-cold)       (mu make-it-hot)  B)
    (A (pred (not is-cold)) (mu make-it-cold) B)
    (B (pred keep-going) (mu count-up) A)
    (B (pred (not keep-going)) (mu get-excited) (pred is-cold) (mu dammit) A)
    (B (pred (not keep-going)) (mu get-excited) (pred (not is-cold)) (mu sweet-hotlanta))))
