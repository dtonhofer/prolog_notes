 
# Linear _foldl_ and _foldr_ in Prolog

A really simple exercise!

[`library(apply)`](https://www.swi-prolog.org/pldoc/man?section=apply) already has a
[`foldl`](https://www.swi-prolog.org/pldoc/doc_for?object=foldl/4). The
[implementation](https://www.swi-prolog.org/pldoc/doc/_SWI_/library/apply.pl?show=src#foldl/4)
of that is more less the same as the one given here.

Start with interesting functions (expressed as predicates) which might be called by a _foldl_ or _foldr_.

[foldy.pl](prolog_notes/fold_foldr/foldy.pl)

I always confuse _foldl_ and _foldr_, so here is a way to remember:

- _foldl_ implements the "Laughable recursion", subject to tail-call optimization.
- _foldr_ implements the "Real recursion", not subject to tail-call optimization.

## The implementation of _foldl_, called `foo_foldl/4`

In this file, complete with Unit Tests: [foo_foldl.pl](prolog_notes/fold_foldr/foo_foldl.pl)

```
[foldy],[foo_foldl]
rt.
```

## The implementation of _foldr_, called `foo_foldr/4`:

In this file, complete with Unit Tests: [foo_foldr.pl](prolog_notes/fold_foldr/foo_foldr.pl)

```
[foldy],[foo_foldr]
rt.
```

## An alternative implementation of _foldl_ based on `maplist/5`, called `foldl_maplist/4`.

In this file, complete with Unit Tests: [foldl_maplist.pl](prolog_notes/fold_foldr/foldl_maplist.pl)

```
[foldy],[foldl_maplist]
rt.
```

