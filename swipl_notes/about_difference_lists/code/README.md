# Compute "length of a difflist"

(For the whole explainer on difflists, see [here](../README.md))

## Straightforward difflist length computation

The following code defines predicate `difflist_length/3` and a `difflist_length/5` which carries two
additional flags which are generally used in output mode. In case one just wants to determine the
length of an open list (instead of a "difference list") there is also `openlist_length/2`.

The predicate relates a difflist structured as `Tip-Fin` with its length. The length is the number of
elements on the backbone between `Tip` and `Fin`. 

Alternatively, if the difflist passed to `difflist_length/3` is fresh, a difflist template,
(containing only fresh variables) of successively larger length is generated.

If the passed difflist `Tip-Fin` turns out to be a proper/closed list at `Tip` and `Fin`
a suffix of the proper list `Tip`, the predicate computes the length of the closed list (i.e. behaves
as [`length/2`](https://eu.swi-prolog.org/pldoc/doc_for?object=length/2)).

If the difflist does not follow difflist structure conventions, an excpetion will be raised.

To try it out, download these files:

- [`difflist_length.pl`](../../../code/heavycarbon/utils/difflist_length.pl) - Core predicates (in a module)
- [`difflist_length.plt`](../../../code/heavycarbon/utils/difflist_length.plt) - Unit tests (plunit blocks)

And the support predicates in:

- [`throwme_nonmodular.pl`](../../../code/heavycarbon/support/throwme_nonmodular.pl) - Predicates to throw exceptions (not modularized, included with `include/1`)
- [`meta_helpers_nonmodular.pl`](../../../code/heavycarbon/support/meta_helpers_nonmodular.pl) - Support metapredicates (not modularized, included with `include/1`)
- [`utils.pl`](../../../code/heavycarbon/support/utils.pl) - Support predicates (in a module)

Set up a directory with the file hierarchy:

```text
code
└── heavycarbon
    ├── support
    │   ├── meta_helpers_nonmodular.pl
    │   ├── throwme_nonmodular.pl
    │   └── utils.pl
    └── utils
        ├── difflist_length.pl
        └── difflist_length.plt
```

Add the topmost directory (here, `code`) to your SWI-Prolog library path:

```text
?- assertz(file_search_path(library,'.../code')).
```

Finally, run the tests:

```
?- ['heavycarbon/utils/difflist_length.plt'].
true.

?- run_tests.
% PL-Unit: difflist_length ................................... done
% PL-Unit: openlist_length ....... done
% All 42 tests passed
true.
```

## Sly difflist length computation

(TODO: Review)

Another approach at computing difflist length is given by this code (which is more experimental):

- [length_dl_sly.pl](length_dl_sly.pl)

This defines predicate `length_dl_sly/2`, which also relates a `Tip-Fin` difflist with its length, but uses 
an inner predicate which first closes the list (i.e. transforms the difflist into a proper list by constraining
Fin` to be `[]` , employs the standard `length/2` to determine the length of the now closed list, and finally
throws an exception to both roll back to the non-closed difflist state and get the length value "out of" the
state that is being rolled-back. This allows one to write simple code! It doesn't detect all cases of badly
formed difflists. 
