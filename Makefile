PROLOG_PROGRAM = swipl

test:
	@$(PROLOG_PROGRAM) tests/run_tests.pl

docs:
	@$(PROLOG_PROGRAM) src/docs.pl