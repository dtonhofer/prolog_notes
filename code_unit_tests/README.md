# Unit tests

This directory contains unit tests for some SWI Prolog predicates written with [`plunit`](https://eu.swi-prolog.org/pldoc/doc_for?object=section(%27packages/plunit.html%27)), the unit testing framework.

`run_them_all.sh` is a bash script which, for each file listed (for now, the files have to be listed explicitly in `run_them_all.sh`,
there is no automatic collection) starts a SWI-Prolog process which loads the file (which is supposed to contain one or more _plunit_ test blocks)
and the calls [`run_tests/0`](https://eu.swi-prolog.org/pldoc/doc_for?object=run_tests/0), executing all the _plunit_ blocks. Ouput to STDERR or STDOUT
is captured in a newly created file, one for every listed file (this is a bit impractical and will have to be changed)

The command run the _plunit_ looks more or less like this:

```text
swipl -g ""set_test_options([silent(true)]) , (run_tests -> halt(0) ; halt(1))" $test_file 1>$tmp_file 2>&1
```

## Directories

- [`simplest`](simplest/) - Simplest possible unit tests to demonstrate unit testing itself. Take a look once as introduction.
- [`builtin_demo`](builtin_demo/) - Unit test for some SWI-Prolog built-in predicates. These are useful for documentation as they say a
  lot more than a long description in prose and they have a formalized, clear format. Plus, they are executable. 
  **Use unit tests for documentation!**



