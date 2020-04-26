# Write a linear "foldl" with `maplist/4`

A linear "foldl" can be easily built
with [`maplist/4`](https://www.swi-prolog.org/pldoc/doc_for?object=maplist/4) -- if one doesn't want to use the [`foldl/4`](https://www.swi-prolog.org/pldoc/doc_for?object=foldl/4) one which comes with [library(apply)](https://www.swi-prolog.org/pldoc/man?predicate=foldl/4). 

This discussion is based on SWI-Prolog 8.1.x

See also:

- [Wikipedia entry for "linear folds"](https://en.wikipedia.org/wiki/Fold_%28higher-order_function%29#Linear_folds)
- ['Catamorphism'/Prolog entry at rosettacode.org](http://rosettacode.org/wiki/Catamorphism#Prolog)

## About

What does `linear foldl` do?

Take the following "list backbone", which is the structure underlying the list representation `[a,b,c,d]`:

```
+---+---+---+---[]    <-- list backbone/spine, terminating in the empty list
|   |   |   |
a   b   c   d         <-- list items/entries/elements/members
```

(The termination as empty list is SWI-Prolog specific, other implementation may have NULL/nil instead)

Folding this list (aka. reducing, accumulating) according to "foldl" means:

- Recurse down the list backbone
- ... computing a running value according to a two-place function `f` 
- ... when you hit the end
- ... come back up from recursion (or output the value; in any case, no further computation needed).

It could be called "Shallowest First Reduction.".

`foldl` is subject to [tail call optimization](https://en.wikipedia.org/wiki/Tail_call). The "come up from recursion" 
can be replaced by a "jump up" and stack frames may even be collapsed into a single stack frame, transforming
recursion into iteration. In Prolog, there must be no choice-points for that to work, which should be the case here!

`f` is iteratively computing the final output `Out` from two values at each list backbone node, starting with a 
specifiable starter value:

- the previous running value
- the current list item

like this, in a pseudo "data flow notation":

```
starter-->--f-->--f-->--f-->--f-->--Out
            |     |     |     |
            a     b     c     d
```

There are two ways to call `f`: either `f(running_value,list_item)`  or `f(list_item,running_value)`. It doesn't
really matter, one just needs to be consistent.

If the function `f` is called as `f(list_item,running_value)`, then

```
Out = f(d,f(c,f(b,f(a,starter))))
```

or in infix operator notation "tail-to-head, right associative":

```
Out = d*(c*(b*(a*starter)))   
```

If the function `f` is called as `f(running_value,list_item)` instead, then

```
Out = f(f(f(f(starter,a),b),c),d)
```

or in infix operator notation "head-to-tail, left associative":

```
Out = ((((starter*a)*b)*c)*d)  
```

## Implementation

[`maplist/4`](https://www.swi-prolog.org/pldoc/doc_for?object=maplist/4) takes three lists which must be of equal
length, then calls a goal for each triple of values from the respective lists.

We set it up thus:

```
List1: Take the input list        a   b   c   d   e   f
List2: And a second input list   st  _1  _2  _3  _4  _5  with the starter value and otherwise fresh variables
List3: And the "output" list     _1  _2  _3  _4  _5  _R  with the same fresh variables as in List2, and _R appended
```

Then the result will appear in `_R` if the called goal unifies every triple as `Arg3 is f(Arg1,Arg2)`

Use "head-to-tail, left associative" calling convention.

This matches what the [foldl/4](https://www.swi-prolog.org/pldoc/doc_for?object=foldl/4) from [library(apply)](https://www.swi-prolog.org/pldoc/man?section=apply) expects and does, so we can immediately extend the test cases
to test that predicates's behaviour too. Note that `foldl/4` is doing the standard thing: recurse over the input list and call `apply` at each node: [source code](https://eu.swi-prolog.org/pldoc/doc/_SWI_/library/apply.pl?show=src#foldl/4).

- The functions of interest which can be applied by a fold are here: [foldy.pl](foldy.pl)
- The code with the unit tests is here: [maplist_foldl.pl](maplist_foldl.pl)

