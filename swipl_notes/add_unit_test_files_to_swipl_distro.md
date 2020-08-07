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
                   
All the files are named `test_${subject}.pl`, e.g. `test_sort.pl` (**note: not `test_sort.plt`**)

To be clarified: should these be called .pl or .plt? Should they be modules or is the module approach only used here?

The manual for [`plunit`](https://eu.swi-prolog.org/pldoc/doc_for?object=section(%27packages/plunit.html%27)) says:

> Test-units can be embedded in normal Prolog source-files. Alternatively, tests for a source-file can be 
> placed in another file alongside the file to be tested. Test files use the extension .plt. The predicate
> load_test_files/1 can load all files that are related to source-files loaded into the current project. 

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

