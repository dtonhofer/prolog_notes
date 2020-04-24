# Examples for the Prolog predicate `maplist/2` (as run with SWI-Prolog)

- This page can also be accessed as  http://bit.ly/2IrNfuG_prolog
- For examples about `maplist/3`, see [this page](maplist_3_examples.md)

## About this page

A few examples for the predicate [`maplist/2`](https://www.swi-prolog.org/pldoc/doc_for?object=maplist/2) 
from [library(apply)](https://eu.swi-prolog.org/pldoc/man?section=apply) as run with SWI-Prolog.

> `library(apply)`
>
> This module defines meta-predicates that apply a predicate on all members of a list.

In order for lists to be printed fully instead of elided at their end with ellipses ("`|...`") you may have
to first call:

```logtalk
?- set_prolog_flag(answer_write_options,[max_depth(0)]).
```

## Intro

The description for [`maplist/2`](https://eu.swi-prolog.org/pldoc/doc_for?object=maplist/2) says:

> `maplist(:Goal, ?List)`
>
> True if Goal can successfully be applied on all elements of List. Arguments are reordered to 
> gain performance as well as to make the predicate deterministic under normal circumstances.

That actually sounds like a `for` loop (or a Perl [`foreach`](https://perldoc.perl.org/perlsyn.html#Foreach-Loops)). 
If you are using a functional programming language already, you know what this is. If you are using Java, 
this corresponds to [`java.util.stream.Streams.map`](https://docs.oracle.com/javase/8/docs/api/java/util/stream/Stream.html#map-java.util.function.Function-) (See also this DZone article: [Java 8 Map, Filter, and Collect Examples](https://dzone.com/articles/how-to-use-map-filter-collect-of-stream-in-java-8)).

It's all about calling a predicate (or a more complex goal) for each element of a list. An elements may be fresh variables, partially or fully ground. The list must be of **known length**, as there is no way to have the called predicate or goal tell `maplist/2` that it should stop going through the list: returning `false` will cause `maplist/2` to fail, and variable bindings built up during its run will be rolled back (but what happens if you `maplist/2` a [lazy list](https://www.swi-prolog.org/pldoc/doc/_SWI_/library/lazy_lists.pl)?)

## Possible applications

Writing the list members (partially or fully ground) out to some data sink:

```logtalk
% writing terms, the last of which happens to be a (fresh) variable

?- maplist(write,["a->","b->","c->",X]).
a->b->c->_5626
true.
```

Reading the list members (partially or fully ground) from some data source:

```logtalk
?- maplist(read,[X,f(Y,Z),c]).
|: "Hello, World".
|: f(1000,2201).
|: c.

X = "Hello, World",
Y = 1000,
Z = 2201.
```

Applying a predicate to each list member. The predicate does nothing except succeed or fail, so we are
performing an conjunctive test on all members of the list: we are verifying list elements

```logtalk
?- maplist(atom,[a,c,d]).
true.

?- maplist(atom,[a,[],d]).
false.
````

An alternative to the above is given by Prolog's [`forall/2`](https://www.swi-prolog.org/pldoc/doc_for?object=forall/2):

> `forall(:Cond, :Action)`: For all alternative bindings of `Cond`, `Action` can be proven.

The syntax to use for `forall/2` is markedly different. You have to use a "generator predicate" as `Cond`, and an actual 
_call_ using the local variable (here, `I`) instead of the predicate's _name_ as for `maplist/2`:

```logtalk
?- forall(member(I,[a,b,d]),atom(I)).
true.

?- forall(member(I,[a,[],d]),atom(I)).
false.
```

Back to `maplist/2`. 

We can generate a list of random numbers (here using `library(yall)` notation, see further below):

```logtalk
?- length(L,6),                        % create a list of 6 fresh variables
   maplist([X]>>(X is random(100)),L). % each fresh var is unified with X, which is unified with the result of random(100)
   
L = [61, 15, 82, 74, 83, 31].
```

For a somewhat unhinged use of "applying a predicate to each list member", see the very end of this page.

## Links

- Markus Triska has a page on [metapredicates](https://www.metalevel.at/prolog/metapredicates), which includes `maplist/N`.

## More details on "verifying list elements"

We can use `maplist` to apply an _individual_ test on all items of a list. 
Why individual? There is no way to pass state between invocations of the Goal given to `maplist/2` (unless
you do dirty coding) so performing _max_, _min_ and similar aggregate operations cannot be done with `maplist/2`.
Use the neighboring `foldl`, `scanl` or homegrown predicates for this. 

Here we test whether all items, assumed to be numeric, are less than 5.
We are also performing side-effects by printing to `stdout`:

````logtalk
verify(X) :- format("verify(~d)\n", X), X < 5.
````

````logtalk
?- maplist(verify, [1,2,3,4]).
verify(1)
verify(2)
verify(3)
verify(4)
true.

?- maplist(verify, [1,2,3,4,5,6]).
verify(1)
verify(2)
verify(3)
verify(4)
verify(5)
false.
````

The above looks like a short-circuting test loop like this (Perl):

```perl
my $allok = 1;
for my $item (@list) {
   $allok = verify($item);
   last if (!$allok)
}
if ($allok) { ... }
```

The `verify` predicate may depend on a second value. Above, we hardocded the limit to be 5. Why not pass it as 
a parameter:

```logtalk
verify(Lim,X) :- format("verify(~d < ~d)\n", [X,Lim]), X < Lim.
```

In bare Prolog:

```logtalk
?- maplist(verify(6), [1,2,3,4,5]).
verify(1 < 6)
verify(2 < 6)
verify(3 < 6)
verify(4 < 6)
verify(5 < 6)
true.

?- maplist(verify(3), [1,2,3,4,5,6]).
verify(1 < 3)
verify(2 < 3)
verify(3 < 3)
false.
```

The syntax is a bit unusual: you pass a term `verify(3)` to `maplist/2`. This term may be regarded
as a half-parameterized call to `verify/2`, with the last argument still missing: a half-completed goal.
`maplist/2` will adjoin the current list element as the missing argument to form a complete 2-argument goal and 
then call that.

Note that the Goal cannot be complex. Syntax restriction!

```logtalk
?- maplist((verify(6),verify(2)),[1,2,3,4,5,6]).
ERROR: Unknown procedure: (',')/3
```

A smoother syntax would be to use a Lambda expression to explicitly show the missing parameter, as in:

```logtalk
?- maplist(λX.verify(3,X), [1,2,3,4,5]).
```

or to stay in ASCIIland:

```logtalk
?- maplist(\X.verify(3,X), [1,2,3,4,5]).
```

Such a notation is defined in the the ISO Prolog specification - but there are add-ons:

- [`library(yall)`](https://www.swi-prolog.org/pldoc/doc/_SWI_/library/yall.pl) which comes from the `Logtalk`
  language (see the [description in the Logtalk manual](https://logtalk.org/manuals/userman/predicates.html#lambda-expressions)).

or alternatively:

- [pack `lambda`](https://www.swi-prolog.org/pack/file_details/lambda/prolog/lambda.pl)

We will be using `library(yall)` here.

### Library `yall` can help you all!

Suppose `verify/2` is the same as before but with reversed arguments:

```logtalk
verify(X, Lim) :- format("verify(~d < ~d)\n", [X,Lim]), X < Lim.
```

```logtalk
?- maplist(verify(3), [1,2,3,4,5]).
verify(3 < 1)
false.
```

is not what we want. We want to have the 3 appear as second argument to `verify/2`.

We could do the following:

```logtalk
my_verify(Lim,X) :- verify(X, Lim)
```

```logtalk
?- maplist(my_verify(3), [1,2,3,4,5]).
verify(1 < 3)
verify(2 < 3)
verify(3 < 3)
false.
```

So that works. But with `yall` notation we can do a one-liner, creating an 
anonymous predicate of one 
variable `X`, which calls `verify/2` with `X` on first position and the limit 3 on second position:

```logtalk
?- maplist([X]>>verify(X,3), [1,2,3,4,5]).
verify(1 < 3)
verify(2 < 3)
verify(3 < 3)
false.
```

This actually "shims" or "wraps" the predicate `verify/2`. As an non-inconsiderable bonus, the above is actually readable.

Would you rather _puzzle_ over this obscure line:

```logtalk
maplist(my_verify(3), [1,2,3,4,5]).
```

than _know_ with this elegant line:

```logtalk
maplist([X]>>verify(3,X), [1,2,3,4,5]).
```

Similary, this notation allows us to invoke complex goals:

```logtalk
?- maplist([X]>>(verify(X,6),verify(X,2)),[1,2,3,4,5,6]).
verify(1 < 6)
verify(1 < 2)
verify(2 < 6)
verify(2 < 2)
false.
```

## Example usage: Computing a result for each element of a list

Although it is far better to use [`maplist/3`](https://www.swi-prolog.org/search?for=maplist%2F3)
for this, `maplist/2` can also be used for computing results:

```logtalk
compute_sqrt([I,O]) :- O is sqrt(I).
```

Again, `compute_sqrt/0` will be upgraded to a term of arity 1 with a list item as argument
and then called:

```logtalk
?- maplist(compute_sqrt,[[1,S0],[2,S1],[3,S2],[4,S3]]).
S0 = 1.0,
S1 = 1.4142135623730951,
S2 = 1.7320508075688772,
S3 = 2.0.
```

Sounds a bit artificial. Again, you are better off with `maplist/3` than `maplist/2` for this application-

## Example usage: Generating lists

For example, to unify `L` with successively larger lists of 1s:

```logtalk
?- maplist([X]>>(X=1),L).
L = [] ;
L = [1] ;
L = [1, 1] ;
L = [1, 1, 1] ;
L = [1, 1, 1, 1] ;
L = [1, 1, 1, 1, 1] ;
...
```

Or just with a list of specific length, even without `yall` Lambda notation:

```logtalk
length(L,10), maplist(=(1), L).
```

To unify all the items of a list L of known length with a random float:

```logtalk
?- length(L,5),maplist([X]>>(X is random_float),L).
L = [0.8203682301866675, 0.86789174167603, 0.9560836782052566, 0.2545485344026232, 0.7363884829219359].
```

The `random_float/0` is a 0-arity function (used on the RHS of `is/2`). It unsurprisingly evaluates to a random float: [`random_float/0`](https://www.swi-prolog.org/pldoc/doc_for?object=f(random_float/0)). (And with that, we are no longer in the land of logic, but getting fresh information from the Big Outside. We are interactive rather than purely deductive.)

The goal passed to `maplist/2` does not keep internal state (unless it has hidden internal state)
so this goal will at first generate a list of only 1s:

```logtalk
?- length(L,5),maplist( [X]>>(member(X,[1,2,3,4])) , L).
L = [1, 1, 1, 1, 1] ;
```

but backtracking will enumerate all 4^5 possibilites of 5-item lists with values from `[1,2,3,4]`:

```logtalk
?- length(L,5),maplist( [X]>>(member(X,[1,2,3,4])) , L).
L = [1, 1, 1, 1, 1] ;
L = [1, 1, 1, 1, 2] ;
L = [1, 1, 1, 1, 3] ;
L = [1, 1, 1, 1, 4] ;
L = [1, 1, 1, 2, 1] ;
L = [1, 1, 1, 2, 2] ;
...
```

This yields a simple way to generate all binary patterns of length 3 for example:

```logtalk
?- length(L,3),maplist( [X]>>(member(X,[0,1])) , L).
L = [0, 0, 0] ;
L = [0, 0, 1] ;
L = [0, 1, 0] ;
L = [0, 1, 1] ;
L = [1, 0, 0] ;
L = [1, 0, 1] ;
L = [1, 1, 0] ;
L = [1, 1, 1].
```

Consider what happens here: The above is equivalent to the query:

```logtalk
?- L=[I0,I1,I2], member(I0,[0,1]), member(I1,[0,1]), member(I2,[0,1]).
```

The predicate calls inside `maplist/2` are chained into a conjunction and, not called in turn. And the 
choicepoints are left open.


## Addendum: Crazy idea: Using `maplist/2` to copy a list the hard way

This code regenerates the list passed to `maplist/2` by re-constructing said list in the variable given by the first argument of the predicate passed to `maplist/2`. This variable gets updated step-by-step.

```logtalk
% "Append" Elem to the "List with dangling tail" "Conses" by going down the chain
% until the dangling tail is found, then setting it to another consbox containing
% Elem in head position

consify(Conses,Elem) :- 
   var(Conses),
   Conses = [Elem|_RestConses].
   
consify(Conses,Elem) :- 
   \+var(Conses),
   Conses = [_|RestConses],
   consify(RestConses,Elem).

% Make a list of "Conses" by constraining/patching its dangling "rightmost tail", 
% still a variable, with '[]'

listify(Conses) :-
   var(Conses),
   format("Conses is var and will now be constrained to be []. The end!\n"),
   Conses = [].
   
listify(Conses) :-
   \+var(Conses),
   Conses = [_|BackOfList], 
   format("Conses is not var but ~w\n",Conses),
   listify(BackOfList).
```
 
```logtalk
?- maplist(consify(Conses),[1,2,3,4,X,Y,Z]), 
   listify(Conses).
   
Conses is not var but [1,2,3,4,_26266,_26272,_26278|_26954]
Conses is not var but [2,3,4,_26266,_26272,_26278|_26954]
Conses is not var but [3,4,_26266,_26272,_26278|_26954]
Conses is not var but [4,_26266,_26272,_26278|_26954]
Conses is not var but [_26266,_26272,_26278|_26954]
Conses is not var but [_26272,_26278|_26954]
Conses is not var but [_26278|_26954]
Conses is var and will now be constrained to be []. The end!
Conses = [1, 2, 3, 4, X, Y, Z] ;
```

- It is somewhat similar to what one would do with a difference list. However, there is no way heree to append to the difference list pass the difference list to the next call for the next append. 
- The above looks like the self-modifying code of logic programming, the state of the computation has direct influence on the predicate values; there should probably be some special syntax to highlight this.
- I feel that there is some mixup in Prolog in "looking at what is the content of a variable without modifying it" (meta) and updating a variable with constraints (i.e. setting it to term, possibly containing other variables) (in-language).
