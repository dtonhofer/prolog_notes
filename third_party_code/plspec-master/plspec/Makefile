tests:
	swipl -s plspec_test.pl -g "run_tests,halt."
	swipl -s validator_test.pl -g "run_tests,halt."

sicstusTests:
	sicstus -l plspec_test.pl --goal "plspec_test:run_tests,halt."
	sicstus -l validator_test.pl --goal "validator_test:run_tests,halt."

allTests: tests sicstusTests
