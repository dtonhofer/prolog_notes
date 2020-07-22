# Compute "length of a difflist"

(For the whole explainer on difflists, see [here](../README.md))

## Straightforward difflist length computation

The following code defines predicate `length_dl/3` (and a `length_dl/5` which carries two additional flags).
The predicate relates a
difflist structured as `Tip-Fin` with its length (which is the number of items on the backbone between
`Tip` and `Fin`). Alternatively, if the difflist passed to `length_dl/3` is fresh, a difflist template,
containing only fresh variables, of successively larger length is generated.

If the passed difflist `Tip-Fin` turns out to be a proper/closed list at `Tip` position and a suffix of
`Tip` at `Fin` position, the predicate computes the length of the closed list (i.e. behaves as
[`length/2`](https://eu.swi-prolog.org/pldoc/doc_for?object=length/2)).

If the difflist does not follow difflist structure conventions, it throws.

To try it out, download these files (one zip file: [length_dl.zip](length_dl.zip))

- [misc.pl](misc.pl) - Miscellaneous helper predicates.
- [length_dl.pl](length_dl.pl) - Core predicates `length/3` and `length/5` 
- [length_dl_tests.pl](length_dl_tests.pl) - Unit tests for `length/5` 

Then:

- load file `length_dl_tests.pl` into SWI Prolog. This includes the two other files. 
- run predicate `rt/0` to start the unit tests.

```
Welcome to SWI-Prolog (threaded, 64 bits, version 8.1.24-33-g95b540cb9)
SWI-Prolog comes with ABSOLUTELY NO WARRANTY. This is free software.
Please run ?- license. for legal details.

For online help and background, visit https://www.swi-prolog.org
For built-in help, use ?- help(Topic). or ?- apropos(Word).

?- [length_dl_tests].
true.

?- rt.
% PL-Unit: length_dl ................................ done
% All 32 tests passed
true.

?- 
```

## Sly difflist length computation

Another approach at computing difflist length is given by this code (which is more experimental):

- [length_dl_sly.pl](length_dl_sly.pl)

This defines predicate `length_dl_sly/2`, which also relates a `Tip-Fin` difflist with its length, but uses 
an inner predicate which first closes the list (i.e. transforms the difflist into a proper list by constraining
Fin` to be `[]` , employs the standard `length/2` to determine the length of the now closed list, and finally
throws an exception to both roll back to the non-closed difflist state and get the length value "out of" the
state that is being rolled-back. This allows one to write simple code! It doesn't detect all cases of badly
formed difflists. 
