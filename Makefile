
check: check-sbcl check-ccl check-clisp check-ecl


check-sbcl:
	@echo "-------- SBCL --------"
	cd src && sbcl --script run-test.lisp
	@echo ""

check-clisp:
	@echo "-------- CLISP --------"
	cd src && clisp  run-test.lisp

check-ccl:
	@echo "-------- CCL  --------"
	cd src && ccl --quiet --load run-test.lisp

check-ecl:
	@echo "-------- ECL  --------"
	cd src && ecl -load run-test.lisp -eval '(quit)'
