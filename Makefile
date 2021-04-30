PROLOG_PROGRAM = swipl

.SILENT:

test:
	$(PROLOG_PROGRAM) tests/run_tests.pl

docs:
	$(PROLOG_PROGRAM) src/run_docs.pl