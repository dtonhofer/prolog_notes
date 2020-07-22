# Better type tests

Begin here: [Type tests in Prolog](https://www.youtube.com/watch?v=ZIv0G4b1xBQ) on Youtube by Markus Triska (3 Apr 2019).

The above presents predicates `atom_si/1`, `integer_si/1`,  `list_si/1`  (`si` standing for "sufficiently instantiated"), which make more sense than the current type test predicates: Instead of failing silently on fresh variables, they throw a `type_error`.

Also introduced are `must_be/2` and `can_be/2` which test whether the computational state allows one to assert that some term is of some type (and will stay that way) or whether there still is the possibility that some term is of some type (even though that may change).

See also:

- https://www.quora.com/If-prolog-were-being-invented-today-with-no-concern-for-backward-compatibility-or-the-existing-standardization-how-would-it-differ-from-standard-prolog?share=1 where Markus Triska writes:

> Currently, we have to slowly move to better type tests, using `must_be/2`, `can_be/2`, `atom_si/1` etc. It will
> take many years to propagate these predicates to application programmers, an errand that could have easily
> been avoided.

- https://stackoverflow.com/questions/24017420/which-meanings-of-type-are-used-in-the-standard

- https://stackoverflow.com/questions/27306453/safer-type-tests-in-prolog

The arity-1 predicates `atom_si/1`, `integer_si/1`,  `list_si/1` etc. are proposed instead of arity-2 predicates like `has_type/2`, `must_be/2` etc.

- https://swi-prolog.discourse.group/t/must-be-2-and-is-of-type-2-types/1539

Indeed, what should a type test like `atom(_)` do?

- Say "I don't know!" is a possibility. Prolog is based on classical logic though, so there is no space for that. The nearest to "I don't know" is throwing an error.
- Freeze until more is known about `_`. But there won't be more info on `_`, ever.
- Start generating possible atoms. Although atoms are enumerable, there are still infinitely many of them.
- Generate a _template_ that can only be instantiated to an atom. That would be a "typed fresh term". Such a concept does not exist, but something rather near to it can be observed when you do `length(L,3)`: You get a template for things that fulfill `length(L,3)`, namely a list of 3 fresh variables (but the list _contents_ is not further templated)
- Fail silently? That's illogical, but that's what happens.

In SWI Prolog there is a [`must_be/2`](https://www.swi-prolog.org/pldoc/doc_for?object=must_be/2) in [`library(error)`](https://www.swi-prolog.org/pldoc/man?section=error). (But where is `can_be/2`?) 

However, that predicate is not satisfying. Although it throws if it has to work on a fresh variable, it also throws if the answer should simply be "no". 

Unit test illustrating these ideas: 

[unit_tests_for_must_be.pl](/code/unit_tests_for_must_be.pl)

The unit test code is also liked from the page for the SWI-Prolog predicate [`must_be/2`](https://eu.swi-prolog.org/pldoc/doc_for?object=must_be/2).

