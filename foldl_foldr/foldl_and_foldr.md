 
# Linear _foldl_ and _foldr_ in Prolog

A really simple exercise!

## Preliminaries

I always confuse _foldl_ and _foldr_, so here is a way to remember:

- _foldl_ ("fold left") implements the **"Laughable Recursion"**, subject to tail-call optimization.
- _foldr_ ("fold right") implements the **"Real Recursion"**, not subject to tail-call optimization.

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

Consider this list:

```
+---+---+---+---[]    <-- list backbone/spine, composed of nodes, terminating in the empty list
|   |   |   |
a   b   c   d         <-- list items/entries/elements/members
```

Folding this list (aka. reducing, accumulating) according to _foldl_ means:

- Recurse down the list backbone, and in each new frame:
   1. Perform computation according to a two-place function _f_, which generates a new value from the local list item and a value passed down from the higher frame. A _starter value_ is passed to the first frame in the recursion chain.
   2. Perform a recursive call with this new value as parameter.
- When you hit the end of the list, you are done: Come back up from recursion with value passed to the last frame as a final result.

Like this, in a "data flow notation":

```
starter value -->--f-->--f-->--f-->--f-->-- Out
                   |     |     |     | 
                   a     b     c     d
```

It could be called _Shallowest First Reduction._

_foldl_ is amenable to [tail-call optimization](https://en.wikipedia.org/wiki/Tail_call): the process of "coming up from recursion" can be replaced by a single "jump up to the stack frame of the first recursive call". The stack frame chain can even be compressed into a single frame, by doing a variable renaming and looping instead of a recursive call. 

As a special case, if the starter value is `[]` and the function applied
is _cons_, i.e. list construction, _foldl_ reverses (not "mirrors"; for that more dimensions are needed) the list
(as long as the argument are given to the `f` function in the right order). This transformation does not destroy information: there is a bijective mapping between the input structure and the output structure.

```
+---+---+---+---[]            +---+---+---+---[] 
|   |   |   |          >>     |   |   |   |
a   b   c   d                 d   c   b   a
```

There are two ways to call _f_: either 

- _f(recursive_value,item)_  or 
- _f(item,recursive_value)_ 

It doesn't really matter, one just needs to be consistent.

If _f_ is called as _f(item,recursive_value)_, then:

```
Out = f(d,f(c,f(b,f(a,starter))))
```

or in infix operator notation "tail-to-head, right associative":

```
Out = d*(c*(b*(a*starter)))   
```

If the function _f_ is called as _f(recursive_value,item)_ instead, then:

```
Out = f(f(f(f(starter,a),b),c),d)
```

or in infix operator notation "head-to-tail, left associative":

```
Out = ((((starter*a)*b)*c)*d)  
```

### The implementation of _foldl_, called `foo_foldl/4`

- `Foldy` is the name of the predicate that shall be called at each node as `Foldy(Iten,ThreadIn,Intermed)`, a stand-in for _f_.
- `ThreadIn` is an accumulator going "down":
   - At each stack frame, the preceding value is passed in through `ThreadIn`,
   - a new value `Intermed` is computed from that,
   - and that value is passed to the next recursive call in place of the previous `ThreadIn` (hence, "variable renaming").
- `ThreadOut` is the communication channel coming "up": The final value will eventually be unified with `ThreadOut`. As `ThreadOut` is really a reference to a global term container (as is the case for all Prolog variables), the result of the unification will be available to the caller of the recursion chain, immediately. No further copying is needed,

```logtalk
foo_foldl(_,[],ThreadEnd,ThreadEnd) :- !. % GREEN CUT

foo_foldl(Foldy,[Item|Ls],ThreadIn,ThreadOut) :-
   call(Foldy,Item,ThreadIn,Intermed),
   foo_foldl(Foldy,Ls,Intermed,ThreadOut).
```

It's in this file, complete with Unit Tests based on `foldy.pl`: [foo_foldl.pl](foo_foldl.pl)

**Run tests:**

```
?- [foldy],[foo_foldl].
true.

?- rt.
% PL-Unit: foo_foldl ........................ done
% All 24 tests passed
true.
```

### An alternative implementation of linear _foldl_ based on DCGs

DCGs are made for traversing lists front-to-back, computing things as they go. Parsing is one of the applications: in that case, the list is a list of tokens. The processed list is not explicit, one just pops items off it using the `[Item]` line. The end of the list is reached when the only fitting DCG rule is the one with `--> []`.

```logtalk
% Wrap the call to DCG processing

dcg_foldl(Foldy,List,Starter,Result) :-
   phrase(recognizer(Foldy,Starter,Result),List).

% Whenever there is an "Item" in the list, first call Foldy, then recurse.

recognizer(Foldy,ThreadIn,ThreadOut) --> 
   [Item],
   !,                                         % GREEN CUT
   { call(Foldy,Item,ThreadIn,Intermed) },     
   recognizer(Foldy,Intermed,ThreadOut).

% When there is nothing in the list, "bounce the accumulator" by unifying
% "ThreadIn" with "ThreadOut", i.e. with "Result"

recognizer(_Foldy,ThreadEnd,ThreadEnd) --> [].
```

### An alternative implementation of linear _foldl_ based on `maplist/4`, called `maplist_foldl/4`.

```logtalk
maplist_foldl(_,[],Starter,Starter) :- !. % GREEN CUT

maplist_foldl(Foldy,List,Starter,Out) :-
   length(List,Len),                    % Len >= 1
   succ(OtherLen,Len),                  % OtherLen <- Len-1
   length(OtherList,OtherLen),          % create a list of fresh variables
   List1 = [Starter|OtherList],         % List of length Len, fresh variables expect Starter item
   append(OtherList,[Out],List2),       % List of length Len, fresh variables, last item is Out
   maplist(Foldy,List,List1,List2).     % Call maplist/4 which constructs goals like Foldy(i1,i2,i3) and calls them
```

It's in this file, complete with Unit Tests based on `foldy.pl`: [foldl_maplist.pl](foldl_maplist.pl)

**Run tests:**

```
?- [foldy],[maplist_foldl].
true.

?- rt.
% PL-Unit: maplist_foldl ........................ done
% All 24 tests passed
true.
```

## About linear _foldr_

As previously, consider this list:

```
+---+---+---+---[]    <-- list backbone/spine, composed of nodes, terminating in the empty list
|   |   |   |
a   b   c   d         <-- list items/entries/elements/members
```

Folding this list (aka. reducing, accumulating) according to _foldr_ means:

- Recurse down the list backbone, and in each new frame: 
   - When you hit the end of the list, it begins: Return a _starter value_ (generally passed down the recursion chain unmodified).
   - Otherwise:
      1. Perform a recursive call as first action. 
      2. Having obtained a value from said recursive call, perform computation according to a two-place function _f_, which generates a new value from the local list item and the previously returned value. Return that new value.

Like this, in a "data flow notation":

```
Out --<--f--<--f--<--f--<--f--<-- starter value
         |     |     |     |
         a     b     c     d
```

It could be called _Deepest First Reduction._

_foldr_ is _not_ amenable to tail-call-optimization because it needs to perform computation in the current
context/stack-frame/activation record after return from the recursive call.

As a special case, if the starter value is `[]` and the function applied
is _cons_, i.e. list construction, _foldr_ leaves the list passed in unmodified 
(as long as the argument are given to the `f` function in the right order, otherwise it doesn't return a list at all). 
This is the _identity transformation/mapping_.  Again, this transformation does not destroy information:
there is a bijective mapping between the input structure and the output structure.

```
+---+---+---+---[]            +---+---+---+---[] 
|   |   |   |          >>     |   |   |   |
a   b   c   d                 a   b   b   d
```

There are two ways to call _f_: either 

- _f(recursive_value,item)_  or 
- _f(item,recursive_value)_ 

It doesn't really matter, one just needs to be consistent.

If _f_ is called as _f(item,recursive_value)_, then:

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

It's in this file, complete with Unit Tests based on `foldy.pl`: [foo_foldr.pl](foo_foldr.pl)

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


