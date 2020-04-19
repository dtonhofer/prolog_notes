 
# Linear _foldl_ and _foldr_ in Prolog

A really simple exercise!

[`library(apply)`](https://www.swi-prolog.org/pldoc/man?section=apply) already has a
[`foldl`](https://www.swi-prolog.org/pldoc/doc_for?object=foldl/4). The
[implementation](https://www.swi-prolog.org/pldoc/doc/_SWI_/library/apply.pl?show=src#foldl/4)
of that is more less the same as the one given here.

Start with interesting functions (expressed as predicates) which might be called by a _foldl_ or _foldr_.

[foldy.pl](foldy.pl)

I always confuse _foldl_ and _foldr_, so here is a way to remember:

- _foldl_ implements the "Laughable recursion", subject to tail-call optimization.
- _foldr_ implements the "Real recursion", not subject to tail-call optimization.

## The implementation of _foldl_, called `foo_foldl/4`

```logtalk
foo_foldl(_,[],ThreadEnd,ThreadEnd) :- !. % GREEN CUT

foo_foldl(Foldy,[Item|Ls],ThreadIn,ThreadOut) :-
   call(Foldy,Item,ThreadIn,Intermed),
   foo_foldl(Foldy,Ls,Intermed,ThreadOut).
```

In this file, complete with Unit Tests based on `foldy.pl`: [foo_foldl.pl](foo_foldl.pl)

Run tests:

```
?- [foldy],[foo_foldl].
true.

?- rt.
% PL-Unit: foo_foldl ........................ done
% All 24 tests passed
true.
```

## The implementation of _foldr_, called `foo_foldr/4`:

```logtalk
foo_foldr(_,[],ThreadEnd,ThreadEnd) :- !. % GREEN CUT

foo_foldr(Foldy,[Item|Ls],ThreadIn,ThreadOut) :-
   foo_foldr(Foldy,Ls,ThreadIn,Intermed),
   call(Foldy,Item,Intermed,ThreadOut).
```

In this file, complete with Unit Tests based on `foldy.pl`: [foo_foldr.pl](foo_foldr.pl)

Run tests:

```
?- [foldy],[foo_foldr].
true.

?- rt.
% PL-Unit: foo_foldr ............ done
% All 12 tests passed
true.
```

## An alternative implementation of _foldl_ based on `maplist/5`, called `foldl_maplist/4`.

```logtalk
foldl_maplist(_,[],Starter,Starter) :- !. % GREEN CUT

foldl_maplist(Foldy,List,Starter,Out) :-
   length(List,Len),                    % Len >= 1
   succ(OtherLen,Len),                  % OtherLen <- Len-1
   length(OtherList,OtherLen),          % create a list of fresh variables
   List1 = [Starter|OtherList],         % List of length Len, fresh variables expect Starter item
   append(OtherList,[Out],List2),       % List of length Len, fresh variables, last item is Out
   maplist(Foldy,List,List1,List2).     % Call maplist/4 which constructs goals like Foldy(i1,i2,i3) and calls them
```

In this file, complete with Unit Tests based on `foldy.pl`: [foldl_maplist.pl](foldl_maplist.pl)

Run tests:

```
?- [foldy],[foldl_maplist].
true.

?- rt.
% PL-Unit: foldl_maplist ........................ done
% All 24 tests passed
true.
```

