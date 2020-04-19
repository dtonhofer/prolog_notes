 
# Linear _foldl_ and _foldr_ in Prolog

A really simple exercise!

## Preliminaries

I always confuse _foldl_ and _foldr_, so here is a way to remember:

- _foldl_ implements the "Laughable recursion", subject to tail-call optimization.
- _foldr_ implements the "Real recursion", not subject to tail-call optimization.

[`library(apply)`](https://www.swi-prolog.org/pldoc/man?section=apply) already has a
[`foldl`](https://www.swi-prolog.org/pldoc/doc_for?object=foldl/4). The
[implementation](https://www.swi-prolog.org/pldoc/doc/_SWI_/library/apply.pl?show=src#foldl/4)
of that is more less the same as the one given here. It uses [`apply/2`](https://www.swi-prolog.org/pldoc/man?section=apply
) instead of [`call/_`](https://www.swi-prolog.org/pldoc/doc_for?object=call/2) as done below. No matter. (Except that `apply/2` is marked _deprecated_; I still like it more).

**Further reading:**

- [Wikipedia entry for "linear folds"](https://en.wikipedia.org/wiki/Fold_%28higher-order_function%29#Linear_folds)
- ['Catamorphism'/Prolog entry at rosettacode.org](http://rosettacode.org/wiki/Catamorphism#Prolog)

## Functions to be used in tests

Start with interesting functions (expressed as predicates) which might be called by a _foldl_ or _foldr_.

Interesting functions to be passed to `call/_` inside the _folds_:  [foldy.pl](foldy.pl)

## About linear _foldl_

### The implementation of _foldl_, called `foo_foldl/4`

```logtalk
foo_foldl(_,[],ThreadEnd,ThreadEnd) :- !. % GREEN CUT

foo_foldl(Foldy,[Item|Ls],ThreadIn,ThreadOut) :-
   call(Foldy,Item,ThreadIn,Intermed),
   foo_foldl(Foldy,Ls,Intermed,ThreadOut).
```

In this file, complete with Unit Tests based on `foldy.pl`: [foo_foldl.pl](foo_foldl.pl)

**Run tests:**

```
?- [foldy],[foo_foldl].
true.

?- rt.
% PL-Unit: foo_foldl ........................ done
% All 24 tests passed
true.
```

### An alternative implementation of linear _foldl_ based on `maplist/4`, called `foldl_maplist/4`.

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

**Run tests:**

```
?- [foldy],[foldl_maplist].
true.

?- rt.
% PL-Unit: foldl_maplist ........................ done
% All 24 tests passed
true.
```

## About linear _foldr_

Take this list:

```
+---+---+---+---[]    <-- list backbone/spine, composed of nodes, terminating in the empty list
|   |   |   |
a   b   c   d         <-- list items/entries/elements/members
```

Folding this list (aka. reducing, accumulating) according to _foldr_ means:

- Recurse down the the list backbone
- ... when you hit the end
- ... come back up from recursion, and on each node of the list backbone, perform computation according to a two-place function _f_.

It could be called _Deepest First Reduction._

_foldr_ is not amenable to tail-call-optimization as it needs to perform computation in the current
context/stack-frame/activation record after return from the recursive call.

_f_ computes the final output _Out_ from two values at each list backbone node, starting with a starter value:

- the value returned from the recursive call made in the current stack-frame
- the current list item

like this, in a pseudo "data flow notation":

```
Out--<--f--<--f--<--f--<--f--<--starter
        |     |     |     |
        a     b     c     d
```

In particular, if the starter value is `[]` and the function applied
is _cons_, i.e. list construction, _foldr_ leaves the list passed in as it was 
(as long as the argument are given to the function in the right order). 
It is the identity transformation/mapping. 

```
+---+---+---+---[]    <-- list backbone/spine, composed of nodes, terminating in the empty list
|   |   |   |
a   b   c   d      
```

It is also one of the transformations that preserves the information content of the structure (i.e there is a 
bijection between the input structure and the ouput structure, nothing is lost. Note that, without outside
sources of information, which don't occur here, nothing can be gained in any case.)

There are two ways to call _f_: either _f(recursive_value,item)_  or _f(item,recursive_value)_. It doesn't
really matter, one just needs to be consistent.

If the function _f_ is called as _f(item,recursive_value)_, then.

```
Out = f(a,f(b,f(c,f(d,starter))))
```

or in infix operator notation "head-to-tail, right associative":

```
Out = a*(b*(c*(d*starter)))   
```

If the function _f_ is called as _f(item,recursive_value)_ instead, then:

```
Out = f(f(f(f(starter,d),c),b),a)
```

or in infix operator notation "tail-to-head, left associative":

```
Out = ((((starter*d)*c)*b)*a)  
```

### The implementation of linear _foldr_, called `foo_foldr/4`:

```logtalk
foo_foldr(_,[],ThreadEnd,ThreadEnd) :- !. % GREEN CUT

foo_foldr(Foldy,[Item|Ls],ThreadIn,ThreadOut) :-
   foo_foldr(Foldy,Ls,ThreadIn,Intermed),
   call(Foldy,Item,Intermed,ThreadOut).
```

In this file, complete with Unit Tests based on `foldy.pl`: [foo_foldr.pl](foo_foldr.pl)

Note that for some reason, there is no `foldr/4` in `library(apply)` of SWI Prolog, so comparison tests are not made.

**Run tests:**

```
?- [foldy],[foo_foldr].
true.

?- rt.
% PL-Unit: foo_foldr ............ done
% All 12 tests passed
true.
```



