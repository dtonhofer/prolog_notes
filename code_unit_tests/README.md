# Unit tests

This directory contains unit tests for some SWI Prolog predicates written with [`plunit`](https://eu.swi-prolog.org/pldoc/doc_for?object=section(%27packages/plunit.html%27)), the unit testing framework.

`run_them_all.sh` is a bash script which applies [`run_tests`](https://eu.swi-prolog.org/pldoc/doc_for?object=run_tests/0)
to all the `plunit` blocks found to listed files. For now, the files have to be listed explicitly in `run_them_all.sh`,
there is no automatic collection.

- `simplest` - Simplest possible unit tests to demonstrate unit testing.
- `builtin_demo` - Unit test for some SWI-Prolog built-in predicates



