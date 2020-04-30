# Examples for the Prolog predicate `maplist/2`

- Here: `maplist/2` (1 goal, 1 list to verify)
- For examples about `maplist/3` (1 goal, 2 lists to verify) see [this page](maplist_3_examples.md)
- For examples about `maplist/4` (1 goal, 3 lists to relate) see [this page](maplist_4_examples.md)

## About

Here we list a few examples for the predicate [`maplist/2`](https://eu.swi-prolog.org/pldoc/doc_for?object=maplist/2) 
from [library(apply)](https://eu.swi-prolog.org/pldoc/man?section=apply) as run with SWI-Prolog.

> `library(apply)`: This module defines meta-predicates that apply a predicate on all members of a list.

We use [SWI-Prolog](https://www.swi-prolog.org/) throughout. However, `maplist/N`, while not in the [ISO standard](https://en.wikipedia.org/wiki/Prolog#ISO_Prolog), at least [not yet](http://www.complang.tuwien.ac.at/ulrich/iso-prolog/prologue), is a common predicate ([GNU Prolog](http://gprolog.org/manual/gprolog.html#sec223), [SICStus Prolog](https://sicstus.sics.se/sicstus/docs/4.3.0/html/sicstus/lib_002dlists.html), [ECLiPSe](https://eclipseclp.org/doc/bips/lib/lists/index.html)). Other Prologs _should_ work the same. 

We also use the [`library(yall)`](https://www.swi-prolog.org/pldoc/man?section=yall) lambda notation imported from [Logtalk](https://logtalk.org/). This _is_ specific to SWI-Prolog.

In order for lists to be printed fully instead of elided at their end with ellipses ("`|...`") you may have
to first call:

````
?- set_prolog_flag(answer_write_options,[max_depth(0)]).
````

The description for [`maplist/2`](https://eu.swi-prolog.org/pldoc/doc_for?object=maplist/2) says:

> `maplist(:Goal, ?List)`
>
> _True if Goal can successfully be applied on all elements of List. Arguments are reordered to 
> gain performance as well as to make the predicate deterministic under normal circumstances._

Thus you have a single list, _List_, and `maplist/2` will call the _Goal_ (which must be a predicate) on each list item. `maplist/2` will fail at the first failed call of _Goal_.

Easy to understand. This will allow us to avoid writing failure-driven loops or explicit recursion, making code cleaner.

If you are using a functional programming language, you know all about the "map" function, which takes another function (a closure) and applies it to a sequence of items, however defined. If you are using Java, this corresponds to [`java.util.stream.Streams.map`](https://docs.oracle.com/javase/8/docs/api/java/util/stream/Stream.html#map-java.util.function.Function-) (See also this DZone article: [Java 8 Map, Filter, and Collect Examples](https://dzone.com/articles/how-to-use-map-filter-collect-of-stream-in-java-8)). 

Prolog's `maplist/N` additionally bring backtracking over possible solutions of the _Goal_ predicate into the mix.

On Wikipedia:

- [map: higher-order function](https://en.wikipedia.org/wiki/Map_(higher-order_function))
- [map: parallel pattern](https://en.wikipedia.org/wiki/Map_(parallel_pattern)) - Somewhat related in the context of parallel architectures or concurrent processing.

It's all about calling a predicate (or a more complex goal) for each item/element of a list. The list should be of **known length**, as there is no way to have the called predicate or goal tell `maplist/2` that it should stop going through the list: returning `false` from the called predicate will cause `maplist/2` to fail, and variable bindings built up during its run will be rolled back. For `maplist/3` and longer-argumented ` maplist/N`, the lists must all be of the same length although it suffices if the length of all lists can be constrained to be identical by `maplist/N`.

Here is `maplist/2` with a list of unconstrained length. It will backtrack over possible lengths, never stopping:

```logtalk
always_true(_).

?- maplist(always_true,E).
E = [] ;
E = [_1468] ;
E = [_1468, _2312] ;
E = [_1468, _2312, _3040] ;
E = [_1468, _2312, _3040, _3768] ;
...
```

(...what happens if you `maplist/2` a [lazy list](https://www.swi-prolog.org/pldoc/doc/_SWI_/library/lazy_lists.pl)?)

## See also

- Markus Triska has a page on [metapredicates](https://www.metalevel.at/prolog/metapredicates), which includes `maplist/N`.

## How to pass parameters to _Goal_

Consider this predicate, which tests whether a list item is less than 5.

In standard Prolog notation:

````logtalk
verify(I) :- format("verify(~d)\n", I), 5 > I.
````

````logtalk
?- maplist(verify, [1,2,3,4]).
verify(1)
verify(2)
verify(3)
verify(4)
true.
````

Alternatively

````logtalk
?- maplist(verify(), [1,2,3,4]).
verify(1)
verify(2)
verify(3)
verify(4)
true.
````

`maplist/2` is passed the name of the predicate (alternatively, a term of arity 0), and will build a new term from it by appending the current item (giving `verify(1)` for example), then call that.

The `verify` predicate may depend on a second value. Above, we hardcoded the upper limit to be 5. Why not pass it as a parameter:

```logtalk
verify(Lim,I) :- format("verify(~d > ~d)\n", [Lim,I]), Lim>I.
```

In standard Prolog notation:

```logtalk
?- maplist(verify(5), [1,2,3,4,5]).
verify(5 > 1)
verify(5 > 2)
verify(5 > 3)
verify(5 > 4)
verify(5 > 5)
false.
```

The same as earlier happens. You pass a term `verify(5)` to `maplist/2`. This term may be regarded
as a half-parameterized call to `verify/2`, with the last argument still missing, a half-completed goal.
`maplist/2` will append the current list item as the missing argument to form a complete 2-argument goal
(giving `verify(5,1)` for example), then call that.

Note that the goal given to `maplist/2` cannot be complex. Syntax restriction!

```logtalk
?- maplist((verify(6),verify(2)),[1,2,3,4,5,6]).
ERROR: Unknown procedure: (',')/3
```

### How about a little λ?

A much smoother syntax would be given by a [_lambda expression_](https://en.wikipedia.org/wiki/Lambda_calculus#Lambda_terms)
explicitly showing the "argument attach point", as in:

```logtalk
?- maplist(λI.verify(3,I), [1,2,3,4,5]).
```

It is immediately recognizable that the inner predicate takes one argument, `I`, and will use it on second position of `verify/2`.

You could even do:

```logtalk
?- maplist(λL.verify(L,3), [1,2,3,4,5]).
```

The inner predicate takes one argument, `L`, and will use it on _first_ position of `verify/2`. 

This cannot be done with standard Prolog notation - you have to use a helper predicate:

```logtalk
my_verify(I,L) :- verify(L,I).
```

```logtalk
?- maplist(my_verify(3), [5,4,3,2,1]).
verify(5 > 3)
verify(4 > 3)
verify(3 > 3)
false.
```

### Can we have that?

Yes! Something quite similar to λ-adorned notation can be had by using the following:

- [`library(yall)`](https://www.swi-prolog.org/pldoc/doc/_SWI_/library/yall.pl) which comes from the `Logtalk`
  language (see the [description in the Logtalk manual](https://logtalk.org/manuals/userman/predicates.html#lambda-expressions)).

or alternatively:

- [pack `lambda`](https://www.swi-prolog.org/pack/file_details/lambda/prolog/lambda.pl)

We will be using `library(yall)` here.

### Library `yall` can help you all with λ notation!

With `library(yall)` notation we can write a one-liner, creating an anonymous predicate of one 
variable `I`, which calls `verify/2` with the position of the argument evident and exchangeable:

```logtalk
?- maplist([I]>>verify(3,I), [1,2,3,4,5]).
verify(3 > 1)
verify(3 > 2)
verify(3 > 3)
false.

?- maplist([I]>>verify(I,3), [5,4,3,2,1]).
verify(5 > 3)
verify(4 > 3)
verify(3 > 3)
false.
```

So we use `[X]>>` instead of `λX.`, staying in ASCIIland.

This notation "shims" or "wraps" the predicate `verify/2`. The documentation also says this is a "closure" (though it's not 
_really_ a [closure](https://en.wikipedia.org/wiki/Closure_(computer_programming)) ... in fact in Prolog there are 
no non-local variables to "lexically close over" at all. It's just a goal.)

As an non-inconsiderable bonus, this notation is actually much more readable than the implied argument passing or standard Prolog notation.

This lambda notation allows us to invoke complex goals:

```logtalk
?- maplist([X]>>(verify(X,6),verify(X,2)),[1,2,3,4,5,6]).
verify(1 < 6)
verify(1 < 2)
verify(2 < 6)
verify(2 < 2)
false.
```

Note that the lambda shim, doing unification, allows modification on call and on return.
This is no use to `maplist/2` but here is an application of `maplist/4` which selects the minimum of two lists:

```logtalk
?- L1=[1,3,5,2,1],L2=[3,3,1,0,1],maplist([I1,I2,max(I1,I2)=O]>>(O is max(I1,I2)),L1,L2,L3).
L1 = [1, 3, 5, 2, 1],
L2 = [3, 3, 1, 0, 1],
L3 = [max(1, 3)=3, max(3, 3)=3, max(5, 1)=5, max(2, 0)=2, max(1, 1)=1].
```

## Possible applications of `maplist/2`

### Writing list items (partially or fully ground) out to some data sink

Evidently a non-logic side-effect.

```logtalk
% writing terms, the last of which happens to be a (fresh) variable

?- maplist(write,["a->","b->","c->",X]).
a->b->c->_5626
true.
```

### Reading list items from some data source

Evidently getting new information from a non-logical source of data.

```logtalk
?- maplist(read,[X,f(Y,Z),c]).
|: "Hello, World".
|: f(1000,2201).
|: c.

X = "Hello, World",
Y = 1000,
Z = 2201.
```

### Generating a list of random numbers

Again, getting new information from a non-logical source of data.

With standard notation, the helper predicate name given to `maplist/2` will be transformed into term `ur(L)` and then called:

```logtalk
ur(I) :- I is random(100).
```

```logtalk
?- length(L,6),maplist(ur,L).
L = [66, 19, 7, 30, 42, 75].
```

`library(yall)` lambda notation explicitly shows that the current item of a list appears as `I` in the called predicate:

```logtalk
?- length(L,6),                        % create a list of 6 fresh variables
   maplist([I]>>(I is random(100)),L). % each fresh variable is unified with the result of random(100)
   
L = [61, 15, 82, 74, 83, 31].
```

### Testing individual list items

As the called predicate naturally succeeds or fails, we are effectively performing an conjunctive _test_ on all list items. 

For example, using [`atom`](https://www.swi-prolog.org/pldoc/doc_for?object=atom/1), test whether they are atoms:

```logtalk
?- maplist(atom,[a,c,d]).
true.

?- maplist(atom,[a,[],d]).
false.
````

As seen akready, test whether all items, assumed to be numeric, are less than 5. We are also performing side-effects by printing to `stdout`:

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
verify(5) % breaks off here!
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

But that's **not really what happens**. What actually happens is:

```logtalk
?- verify(1),verify(2),verify(3),verify(4).
```

This is a conjunction of predicates. 

**If any of the predicates has choicepoints open, we can backtrack over them.**

A short-circuiting test loop is given by Prolog's [`forall/2`](https://www.swi-prolog.org/pldoc/doc_for?object=forall/2):

> `forall(:Cond, :Action)`: For all alternative bindings of `Cond`, `Action` can be proven.

The syntax to use for `forall/2` is markedly different from the one for `maplist/2`. You have to use a "generator predicate" as `Cond`, and an actual 
_call_ using the local variable (here, `I`) instead of the predicate's _name_ as for `maplist/2`:

```logtalk
?- L=[a,b,d],forall(member(I,L),atom(I)).
true.

?- L=[a,[],d],forall(member(I,L),atom(I)).
false.
```

*However*, forall just calls the predicate once for each list item:

```logtalk
?- L=[1-a,2-b,2-c,3-d],
forall(member(I,L),
        ( I=N-C,
          member(N-X,L),
          format("~w ",[N-C-X]) )).
          
1-a-a 2-b-b 2-c-b 3-d-d 
L = [1-a, 2-b, 2-c, 3-d].
```

whereas `maplist/N` chains the calls into a conjunction and leaves the choicepoints open. Note the same at all:

```logtalk
% with some output reformatting:

?- L=[1-a,2-b,2-c,3-d],
maplist( [N-C]>>(member(N-X,L),format("~w ",[N-C-X])) , L).

1-a-a 2-b-b 2-c-b 3-d-d  L = [1-a, 2-b, 2-c, 3-d] ;
            2-c-c 3-d-d  L = [1-a, 2-b, 2-c, 3-d] ;
      2-b-c 2-c-b 3-d-d  L = [1-a, 2-b, 2-c, 3-d] ;
            2-c-c 3-d-d  L = [1-a, 2-b, 2-c, 3-d] ;
false.
```

### Verifying the contract of predicates

Suppose your predicate has as [contract](https://en.wikipedia.org/wiki/Design_by_contract) that it only accepts list of 
integers >= 0.

(This is fraught with annoying edge cases... there really should be a declarative minilanguage for this; something like
[Hamcrest for Erlang](https://github.com/hyperthunk/hamcrest-erlang))

```Logtalk
% This will fail if List is not a list of integers, but succeed with `List=[]` 
% if `List` is a fresh variable.
  
my_predicate(List) :-
   maplist([X]>>(integer(X),X>=0),List).
   do_work(List).

% you also allow List to be a fresh variable itself

my_predicate(List) :-
   (var(List) -> true ; maplist([X]>>(integer(X),X>=0),List)).
   do_work(List).
   
% you also allow List to contain fresh variables

my_predicate(List) :-
   (var(List) -> true ; maplist( [X]>> (var(X) -> true; integer(X),X>=0 ) ,List)).
   do_work(List).
```

If you would rather throw instead of failing (When to throw, when to fail? Prolog guidelines 
[don't say anything](https://arxiv.org/abs/0911.2899), standard Prolog predicates are unsure/inconsistent,
and IMHO, it is up to the caller to demand a throw or a failure in any case using flags, 
but that's for another time entirely). See also
[Exception Handling](https://www.swi-prolog.org/pldoc/man?section=exception):

```Logtalk
ayyylmao(Msg,Args) :-
   % Msg is a "format/2" format string, Args a list of arguments.
   % List of arguments must *exactly* match the placeholders or you will get an Exception!
   % In a prod system, one would shield against further errors by catching any error that happens here!
   with_output_to(string(Buf),format(Msg,Args)),
   throw(Buf).

my_predicate(List) :-
   ((var(List) -> true ; maplist( [X]>> (var(X) -> true; integer(X),X>=0 ) ,List)) 
   -> true
   ; ayyylmao("You dun goofed, this list has problems: ~q",[List])),
   do_work(List).
```

Then:

```Logtalk
?- my_predicate([a,b,c]).
ERROR: Unhandled exception: "You dun goofed, this list has problems: [a,b,c]"
```

Okay, this is getting a bit off the mark.

### Generating lists

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

Or just with a list of specific length, even without `library(yall)` lambda notation:

```logtalk
length(L,10), maplist(=(1), L).
```

To unify all the items of a list L of known length with a random float:

```logtalk
?- length(L,5),maplist([X]>>(X is random_float),L).
L = [0.8203682301866675, 0.86789174167603, 0.9560836782052566, 0.2545485344026232, 0.7363884829219359].
```

The `random_float/0` is a 0-arity function (used on the RHS of `is/2`). It unsurprisingly evaluates to a random float: [`random_float/0`](https://www.swi-prolog.org/pldoc/doc_for?object=f(random_float/0)). We are not in logic land anymore. 

### Generating binary patterns

A simple way to generate all binary patterns of length 3 for example:

Using `library(yall)` notation, which explicitly shows that the current item of a list appears as `I` in the called predicate:

```logtalk
?- length(L,3),maplist( [I]>>(member(I,[0,1])) , L).
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

The predicate calls inside `maplist/2` are chained into a conjunction, **not simply called in turn**. And the 
choicepoints are left open. As long as all the calls succeed, we can **backtrack through possible solutions.**

In other words, `length(L,3),maplist( [I]>>(member(I,[0,1])) , L).` is a compressed way of writing

```logtalk
L=[L0,L1,L2],Vs=[0,1],member(L0,Vs),member(L1,Vs),member(L2,Vs).
```

Similarly, this goal will at first generate a list of only 1s:

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

You could alternatively "unroll" maplist and write:

```logtalk
L=[L0,L1,L2,L3,L4],Vs=[1,2,3,4],member(L0,Vs),member(L1,Vs),member(L2,Vs),member(L3,Vs),member(L4,Vs).
```

### Computing a value for each item of a list

Although it is better to use [`maplist/3`](https://www.swi-prolog.org/search?for=maplist%2F3)
for this (see also [`maplist/3` examples](maplist_3_examples.md)), `maplist/2` _can_ be used for computing values from list items.

You cannot do aggregation operations like `max`, `min` or summation over the list items because the goal passed to `maplist/2` does not keep internal state (unless it has hidden internal state, e.g by using `assert/2` and `retract/1` -- don't do that!) Value computation stays "individual". Use the neighboring [`foldl/N`](https://www.swi-prolog.org/pldoc/doc_for?object=foldl/4), [`scanl/N`](https://www.swi-prolog.org/pldoc/doc_for?object=scanl/4) or homegrown predicates for this. 

Using standard notation. The helper predicate name `compute_sqrt` given to `maplist/2` will be transformed to a term of arity 1 with a list item as argument, then called:

```logtalk
compute_sqrt([I,O]) :- O is sqrt(I).
```

```logtalk
?- maplist(compute_sqrt,[[1,S0],[2,S1],[3,S2],[4,S3]]).
S0 = 1.0,
S1 = 1.4142135623730951,
S2 = 1.7320508075688772,
S3 = 2.0.
```

Using `library(yall)` lambda notation, we can write a one-liner, properly showing the variables in use:

```logtalk
maplist([[I,O]]>>(O is sqrt(I)),[[1,S0],[2,S1],[3,S2],[4,S3]]).
S0 = 1.0,
S1 = 1.4142135623730951,
S2 = 1.7320508075688772,
S3 = 2.0.
```

Again, you are better off with `maplist/3` than `maplist/2` for this kind of application.

### Using `maplist/N` inside of `maplist/N`.

If you want to test a list of lists, with the condition that every sublist has the same length, you can use `maplist/N` inside of `maplist/N`:

```logtalk
% Are the items atoms? Test using atoms/1.

?- L=[[a,b],[c,d]], maplist(maplist(atom),L).
L = [[a, b], [c, d]].

% Print the items using write/1.

?- L=[[a,b],[c,d]], maplist(maplist(write),L).
abcd
L = [[a, b], [c, d]].
```

The lambda syntax of `library(yall)` shows what is happening in a more transparent way. The sublist appears as `S`
in the inner call to `maplist/2`, in which items appear as `I`: 

```logtalk
?- L=[[a,b],[c,d]], maplist([S]>>maplist([I]>>atom(I),S),L).
L = [[a, b], [c, d]].
```

### Addendum: Crazy idea: Using `maplist/2` to copy a list the hard way

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
