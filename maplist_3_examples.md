# Examples for the Prolog predicate `maplist/3` (as run with SWI-Prolog)

A few examples for the predicate [`maplist/3`](https://eu.swi-prolog.org/pldoc/doc_for?object=maplist/3) 
from [library(apply)](https://eu.swi-prolog.org/pldoc/man?section=apply) as run with SWI-Prolog.

`library(apply)`: _"This module defines meta-predicates that apply a predicate on all members of a list."_

In order for lists to be printed fully instead of elided at their end with ellipses ("`|...`") you may have
to first call:

````
?- set_prolog_flag(answer_write_options,[max_depth(0)]).
````

The description for [`maplist/3`](https://eu.swi-prolog.org/pldoc/doc_for?object=maplist/3) says:

> `maplist(:Goal, ?List1, ?List2)`
>
> _"As maplist/2, operating on pairs of elements from two lists."_

The two maps given to `maplist/3` can be fully grounded (i.e. fully constrained), which is the first thing one
would think of when one is used to functional or imperative programming:

````
myprint(X,Y) :- format("~w\n",[(X,Y)]).
?- maplist(myprint,[0,1,2,3],[a,b,c,d]).
0,a
1,b
2,c
3,d
true.
````

Instead of creating a separate predicate `myprint/2`, one can use the "lambda notation" of
[library(yall)](https://www.swi-prolog.org/pldoc/man?section=yall) to create a one-liner.
A  call to `myprint/2` is replaced by a call to an "anonymous predicate" written in-line:

````
?- maplist([X,Y]>>format("~w\n",[(X,Y)]),[0,1,2,3],[a,b,c,d]).
0,a
1,b
2,c
3,d
true.
````

If we leave the third argument (that is to say, the second _list_ argument) of `maplist/3` unconstrained, 
we can make `maplist/3` work in the same way as the 
[`map`](https://en.wikipedia.org/wiki/Map_(higher-order_function)) operation
available in functional or imperative programming languages.

Create a list of integers 0..10, then apply a function to each member.

(
In the following code, instead of typing the list in, you can also create the "input list" using `bagof/3`.:

````
?- bagof(X,between(0,10,X),In),maplist([X,Y]>>(X is Y/2),Out,In).
In = [0,1,2,3,4,5,6,7,8,9,10],
Out = [0,0.5,1,1.5,2,2.5,3,3.5,4,4.5,5].
````
)

**One way.** Second list argument is the result of applying, a function to the first list argument, member-by-member:

````
?- maplist([X,Y]>>(Y is 2*X),[0,1,2,3,4,5,6,7,8,9,10],Out).
Out = [0,2,4,6,8,10,12,14,16,18,20].
````

Note that `Out` is not even constrained to be a list at call time.

**The other way.** First list argument is the result of applying, a function to the second list argument, member-by-member: 

````
?- maplist([X,Y]>>(X is Y/2),Out,[0,1,2,3,4,5,6,7,8,9,10]).
Out = [0,0.5,1,1.5,2,2.5,3,3.5,4,4.5,5].
````


**The symmetric way**. With the [`#=`](https://eu.swi-prolog.org/pldoc/doc_for?object=%23%3D%20/%202) constraint
predicate of the library for "Constraint Logic Programming over Finite Domains", you can work "symmetrically":

````
:- use_module(library(clpfd)).
?- maplist([X,Y]>>(X #= Y-1),[0,S1,2],[S0,1,3]).
S1 = 0,
S0 = 1.
````

Of course we have the usual possibilities of unification. Here with side effects:

````
?- maplist([X,Y]>>((Y is 2*X),format("~w\n",[(X,Y)])),[0,1,2,3,4,5,6,7,8,9,10],[0,2,A,6,8,10,12|R]).
0,0
1,2
2,4
3,6
4,8
5,10
6,12
7,14
8,16
9,18
10,20
A = 4,
R = [14,16,18,20].
````

Unification, must, of course, succeed. This is not transactional, side-effects will be run before unification failure
hits:

````
?- maplist([X,Y]>>((Y is 2*X),format("~w\n",[(X,Y)])),[0,1,2,3,4,5,6,7,8,9,10],[0,2,A]).
0,0
1,2
2,4
false.
````

If have a complex function that you want to call on the pairs of list members and need to have it "partly specified"
at call time by `maplist/3`, you can "partly-construct" the term of the predicate call that will be issued by `maplist/3`: 

Here we want to call `verify/3` on 

- 6
- the i-th member of the first list
- the i-th member of the second list

```
verify(A,B,C) :- format("Received A = ~w, B = ~w, C = ~w\n",[A,B,C]).

?- maplist(verify(6), [a,b,c,d,e,f], [X,Y,Z,x,y,z]).
Received A = 6, B = a, C = _8442
Received A = 6, B = b, C = _8448
Received A = 6, B = c, C = _8454
Received A = 6, B = d, C = x
Received A = 6, B = e, C = y
Received A = 6, B = f, C = z
true.
```

This works for unconstrained variables (here, `K`) as well:

```
 ?- maplist(verify(K), [a,b,c,d,e,f], [X,Y,Z,x,y,z]).
Received A = _8402, B = a, C = _8442
Received A = _8402, B = b, C = _8448
Received A = _8402, B = c, C = _8454
Received A = _8402, B = d, C = x
Received A = _8402, B = e, C = y
Received A = _8402, B = f, C = z
true.
```

Use `library(yall)` to move values around:

```
?- maplist([Arg1,Arg2]>>verify(Arg1,6,Arg2), [a,b,c,d,e,f], [X,Y,Z,x,y,z]).
Received A = a, B = 6, C = _18906
Received A = b, B = 6, C = _18912
Received A = c, B = 6, C = _18918
Received A = d, B = 6, C = x
Received A = e, B = 6, C = y
Received A = f, B = 6, C = z
```

## Software Archeology:

SWI Prolog's source for [apply.pl](https://github.com/SWI-Prolog/swipl-devel/blob/master/library/apply.pl) 
gives the following implementation:

````
%!  maplist(:Goal, ?List1, ?List2)
%
%   As maplist/2, operating on pairs of elements from two lists.

maplist(Goal, List1, List2) :-
    maplist_(List1, List2, Goal).

maplist_([], [], _).
maplist_([Elem1|Tail1], [Elem2|Tail2], Goal) :-
    call(Goal, Elem1, Elem2),
    maplist_(Tail1, Tail2, Goal).
````    
    
Picat and B-Prolog take their `maplist/3` from [Edinburgh DEC-10 Prolog](http://www.picat-lang.org/bprolog/publib/index.html), the code from mid-80s in file [applic.pl](http://www.picat-lang.org/bprolog/publib/applic.html) is:

````
%   maplist(Pred, OldList, NewList)
%   succeeds when Pred(Old,New) succeeds for each corresponding
%   Old in OldList, New in NewList.  In InterLisp, this is MAPCAR. 
%   It is also MAP2C.  Isn't bidirectionality wonderful?

maplist(_, [], []).
maplist(Pred, [Old|Olds], [New|News]) :-
	apply(Pred, [Old,New]),
	maplist(Pred, Olds, News).
```` 
 
For SICStus Prolog 4.3.0, [this page](https://sicstus.sics.se/sicstus/docs/4.3.0/html/sicstus/lib_002dlists.html)
says `maplist/3` "could be defined as":

```` 
maplist(Pred, Xs, Ys) :-
   (   foreach(X,Xs),
       foreach(Y,Ys),
       param(Pred)
      	do  call(Pred, X, Y)
   ).
```` 
