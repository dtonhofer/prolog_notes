# "Throwing in Style" and avoiding cleartext exception texts littering you code

This is a more flexible proposal than inserting error texts everywhere an exception needs
to be thrown (originally made for the Prolog-Java bridge [JPL](https://eu.swi-prolog.org/pldoc/doc_for?object=section(%27packages/jpl.html%27))).

The idea is that neither text nor exception type are coded inside the program proper.
Instead all exception code is thrown via calls to a dedicated predicate `throwme/2`
and all exceptions are described by clauses of the predicate `exc_desc/4`

## All exception code is thrown via calls to `throwme/2`

Instead of writing:

```text
foo(X,Y) :- X<0,throw(error(contract_error(arg1,X),context(_,"The first argument must be >= 0"))).
```

which becomes hard to read and maintain if there are lots of `throw` instructions, one writes

```text
foo(X,Y) :- X<0,throwme(foo,parameter_wrong(arg1,X)).
```

And one adds an _exception descriptor_ for the desired *somewhere at the end of the source file* or
in a *separate file that can be included*, easily inspectable and updateable:

```text
exc_desc(foo,parameter_wrong(Where,What),  % lookup values found in the throwme/2 call
         _,                                % "location" that will be in the context term on 1st position
         contract_error(Where,What),       % "formal" term of the exception (here, non-ISO standard)
         '1st arg must be >= 0').          % cleartext message that will be in the context term on 2nd position
```

One can of course try variations, e.g with dynamic messages:

```text
exc_desc(foo,parameter_wrong(Where,What),  % lookup values found in the throwme/2 call
         _,                                % "location" that will be in the context term on 1st position
         contract_error(Where,What),       % "formal" term of the exception (here, non-ISO standard)
         Msg) :-
   textize("1st arg must be >= 0 but is ~d",[What],Msg).
```

The basic file to pull in is here, it contains further explanations. It is actually not a module, you have to include the
file with [`include/1`](https://eu.swi-prolog.org/pldoc/doc_for?object=include/1) into those modules which use it, duplication the
(small) code.

This is because the `exc_desc/5` clauses should module-specific and not be exported, so if this _were_ a module, `throwme/2` 
could not access the module-internal `exc_desc/5` clauses.

[`throwme_nonmodular.pl`](../../code/heavycarbon/support/throwme_nonmodular.pl)

## Making sure the `throwme/2` calls are correct

To test whether all calls to `throwme/2`  have been coded correctly, you first need to
extract all those calls from the source code, then copy-paste them into a small plunit test block.
(This is best automated, but I haven done so yet).

The plunit test block:

[exception_testcode.pl](code/exception_testcode.pl)

A Perl program to get those `throwme/2` calls out of a a Prolog program read from STDIN (should
be rewritten to Prolog; maybe later):

[perl_throwme_extractor.pl](code/perl_throwme_extractor.pl)
