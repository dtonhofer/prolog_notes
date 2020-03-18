# Examples for the Prolog predicate `maplist/2` (as run with SWI-Prolog)

- This page can also be accessed as  http://bit.ly/2IrNfuG_prolog)
- For examples about `maplist/3`, see [this page](maplist_3_examples.md)

## About

A few examples for the predicate [`maplist/2`](https://www.swi-prolog.org/pldoc/doc_for?object=maplist/2) 
from [library(apply)](https://eu.swi-prolog.org/pldoc/man?section=apply) as run with SWI-Prolog.

`library(apply)`: _"This module defines meta-predicates that apply a predicate on all members of a list."_

In order for lists to be printed fully instead of elided at their end with ellipses ("`|...`") you may have
to first call:

````
?- set_prolog_flag(answer_write_options,[max_depth(0)]).
````

## Intro

The description for [`maplist/2`](https://eu.swi-prolog.org/pldoc/doc_for?object=maplist/2) says:

> `maplist(:Goal, ?List)`
>
> _"True if Goal can successfully be applied on all elements of List. Arguments are reordered to 
> gain performance as well as to make the predicate deterministic under normal circumstances."_

So, what can we do with this?

## Testing list items

For one, we can us it to apply an _individual_ test on all items (aka. member or elements) of a list. 
Why individual? There is no way to pass state between invocations of the Goal given to `maplist/2` (unless
you do dirty coding) so performing _max_, _min_ and similar full-list operations cannot be done with `maplist/2`.
Use the neighboring `foldl`, `scanl` or homegrown predicates for this. 

Here we test whether all items, assumed to be numeric, are less 5.
We are also performing side-effects by printing to STDOUT:

````
verify(X) :- format("verify(~d)\n", X), X < 5.
````

````
?- maplist(verify, [1,2,3,4]).
verify(1)
verify(2)
verify(3)
verify(4)
true.

?- maplist(verify, [1,2,3,4,5]).
verify(1)
verify(2)
verify(3)
verify(4)
verify(5)
false.
````

The above looks like a short-circuting test loop like this (Perl):

````
my $allok = 1;
for my $item (@list) {
   $allok = verify($item);
   last if (!$allok)
}
if ($allok) { ... }
````

The `verify` function may depend on a second value. Above, we hardocded the limit to be 5. Why not pass it as 
a parameter:

````
verify(Lim,X) :- format("verify(~d < ~d)\n", [X,Lim]), X < Lim.
````

````
?- maplist(verify(6), [1,2,3,4,5]).
verify(1 < 6)
verify(2 < 6)
verify(3 < 6)
verify(4 < 6)
verify(5 < 6)
true.

?- maplist(verify(3), [1,2,3,4,5]).
verify(1 < 3)
verify(2 < 3)
verify(3 < 3)
false.
````

Note that the syntax is a bit astonishing: you pass to `maplist/2` a term `verify(3)`, which can be regarded
as a half-parameterized call to `verify/2`, with the last argument still missing - a half-completed Goal.
`maplist/2` will adjoin the current list item as the missing argument to form a complete 2-argument Goal and 
then call that.

A better syntax would be to use a Lambda expression to explicitly show the missing parameter, as in:

````
?- maplist(λX.verify(3,X), [1,2,3,4,5]).
````

or to stay in ASCIIland:

````
?- maplist(\X.verify(3,X), [1,2,3,4,5]).
````

But that's not what we have.

Note that the Goal cannot be complex. Syntax restriction!

````
?- maplist((verify(6),verify(2)),[1,2,3,4,5,6]).
ERROR: Unknown procedure: (',')/3
````

However...

## Using library `yall`

There is library [library(yall)](https://www.swi-prolog.org/pldoc/man?section=yall) which provides lambda expressions.
(See also the [description in the Logtalk manual](https://logtalk.org/manuals/userman/predicates.html#lambda-expressions)).

`yall` functionality can be used to wrap an existing predicate, thus allowing the following:

Suppose `verify/2` is the same as before but with reversed arguments:

````
verify(X, Lim) :- format("verify(~d < ~d)\n", [X,Lim]), X < Lim.
````

````
?- maplist(verify(3), [1,2,3,4,5]).
verify(3 < 1)
false.
````

is not what we want. We want to have the 3 appear as second argument to `verify/2`.

We could do the following:

````
my_verify(Lim,X) :- verify(X, Lim)
````

````
?- maplist(my_verify(3), [1,2,3,4,5]).
verify(1 < 3)
verify(2 < 3)
verify(3 < 3)
false.
````

So that works. But with `yall` notation (also a Lambda notation), we can do a one-liner, creating an 
anonymous predicate of one 
variable `X`, which calls `verify/2` with `X` on first position and the limit 3 on second position:

````
?- maplist([X]>>verify(X,3), [1,2,3,4,5]).
verify(1 < 3)
verify(2 < 3)
verify(3 < 3)
false.
````

Similary, this notation allows us to invoke complex goals:

````
?- maplist([X]>>(verify(X,6),verify(X,2)),[1,2,3,4,5,6]).
verify(1 < 6)
verify(1 < 2)
verify(2 < 6)
verify(2 < 2)
false.
````

## Computing results

Although it is far better to use [`maplist/3`](https://www.swi-prolog.org/search?for=maplist%2F3)
for this, `maplist/2` can also be used for computing results:

````
compute_sqrt([I,O]) :- O is sqrt(I).
````

Again, `compute_sqrt/0` will be upgraded to a term of arity 1 with a list item as argument
and then called:

````
?- maplist(compute_sqrt,[[1,S0],[2,S1],[3,S2],[4,S3]]).
S0 = 1.0,
S1 = 1.4142135623730951,
S2 = 1.7320508075688772,
S3 = 2.0.
````

Sounds artificial? Well, yes.

## Generate lists

We can also generate lists.

For example, to unify `L` with successively larger lists of 1s:

````
?- maplist([X]>>(X=1),L).
L = [] ;
L = [1] ;
L = [1, 1] ;
L = [1, 1, 1] ;
L = [1, 1, 1, 1] ;
L = [1, 1, 1, 1, 1] ;
...
````

Or just with a list of specific length, even without `yall` Lambda notation:

````
length(L,10), maplist(=(1), L).
````

To unify all the items of a list L of known length with random floats:

````
?- length(L,5),maplist([X]>>(X is random_float),L).
L = [0.8203682301866675, 0.86789174167603, 0.9560836782052566, 0.2545485344026232, 0.7363884829219359].
````

The goal passed to `maplist/2` does not keep internal state (unless it has hidden internal state, 
reading from a channel for example) so this goal will at first generate a list of only 1s:

````
?- length(L,5),maplist( [X]>>(member(X,[1,2,3,4])) , L).
L = [1, 1, 1, 1, 1] ;
````

but backtracking will enumerate all 4^5 possibilites of 5-item lists with values from `[1,2,3,4]`:

````
?- length(L,5),maplist( [X]>>(member(X,[1,2,3,4])) , L).
L = [1, 1, 1, 1, 1] ;
L = [1, 1, 1, 1, 2] ;
L = [1, 1, 1, 1, 3] ;
L = [1, 1, 1, 1, 4] ;
L = [1, 1, 1, 2, 1] ;
L = [1, 1, 1, 2, 2] ;
...
````

This yields a simple way to generate all binary patterns of length 3 for example:

````
?- length(L,3),maplist( [X]>>(member(X,[0,1])) , L).
L = [0, 0, 0] ;
L = [0, 0, 1] ;
L = [0, 1, 0] ;
L = [0, 1, 1] ;
L = [1, 0, 0] ;
L = [1, 0, 1] ;
L = [1, 1, 0] ;
L = [1, 1, 1].
````
