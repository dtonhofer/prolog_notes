# Adding unit test files to the SWIPL distro

Test files that are run after compilation (via ninja via CBUILD) are in `$DISTRO/src/Tests/`, in particular for builtins in `$DISTRO/src/Tests/core`

The `README` in that directory says

> These directories contain tests called from   the main test-script. Each
> subdirectory holds test-files. Each test file   must  obey the following
> rules:
>
>        * The filename must be unique in the whole test-pool
>
>        * It must be a module-file and the module name must be the
>          filename
>
>        * It must export a single predicate whose name is the name of
>          the file and the predicate should succeed without output if
>          the test succeeds.
                   
All the files are named `test_${subject}.pl`, e.g. `test_sort.pl`.

Let's examine `test_sort.pl`

Leaving out the GNU LGPL header: 

```
:- module(test_sort, [test_sort/0]).
:- use_module(library(plunit)).

test_sort :-
        run_tests([ sort,
                    msort,
                    keysort,
                    sort4
                  ]).

:- begin_tests(sort).

test(empty, R == []) :-
        sort([], R).

....

:- end_tests(sort).
```

