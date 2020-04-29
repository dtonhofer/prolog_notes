# Examples for the Prolog predicate `maplist/3` 

- Here: `maplist/3` (1 goal, 2 list to relate) 
- For examples about `maplist/2` (1 goal, 1 list to verify) see [this page](maplist_2_examples.md)
- For examples about `maplist/4` (1 goal, 3 lists to relate) see [this page](maplist_4_examples.md)

## About

Here we list a few examples for the predicate [`maplist/3`](https://eu.swi-prolog.org/pldoc/doc_for?object=maplist/3) 
from [library(apply)](https://eu.swi-prolog.org/pldoc/man?section=apply) as run with SWI-Prolog.

`library(apply)`: _"This module defines meta-predicates that apply a predicate on all members of a list."_

In order for lists to be printed fully instead of elided at their end with ellipses ("`|...`") you may have
to first call:

````
?- set_prolog_flag(answer_write_options,[max_depth(0)]).
````

The notes regarding lambda-notation provided by [`library(yall)`](https://www.swi-prolog.org/pldoc/man?section=yall) on the page for [`maplist/2`](maplist_2_examples.md) stay relevant.

The description for [`maplist/3`](https://eu.swi-prolog.org/pldoc/doc_for?object=maplist/3) says:

> `maplist(:Goal, ?List1, ?List2)`
>
> _As maplist/2, operating on pairs of elements from two lists._

Thus you have two lists, _List1_ and _List2_, and `maplist/3` will call the _Goal_ for pairwise associations of list items:

```
   List1             [L1A, L1B, L1C, L1D, ...  
   List2             [L2A, L2B, L2C, L2D, ...  
                        
   Goal called for   (L1A,L2A), (L1B,L2B), (L1C,L2C), (L1D,L2D), ...
```

`Goal` must either be;

- a predicate taking two arguments more than indicated on last position: `foo` to 
  be called as `foo(L1,L2)`, `foo(x)` to be called as `foo(x,L1,L2)`, `foo(x,y)` to be called as `foo(x,y,L1,L2)` etc.
- a lambda expression taking two arguments if one uses `library(yall)`: `[L1,L2]>>foo(L1,L2)`

Additionally:

- both lists must be the same length (or one of the list must be unconstrained, i.e. be open-ended or simply a fresh variable)
- `Goal` must succeed at every location.

```logtalk
% all is fine

?- maplist([X,Y]>>(true),[1,2],[1,4]).
true.

% unequal length means trouble

?- maplist([X,Y]>>(true),[1,2],[1]).
false.

% so does a failing goal

?- maplist([X,Y]>>(false),[1,2],[1,4]).
false.
```

Side-effects will occur before failure hits. It's not transactional:

````
?- maplist([X,Y]>>((Y is 2*X),
           format("~w\n",[(X,Y)])),
	   [0,1,2,3,4,5,6,7,8,9,10],
	   [0,2,A]).
	   
0,0
1,2
2,4
false.
````

The standard application is the one corresponding to the [`map`](https://en.wikipedia.org/wiki/Map_(higher-order_function)) operation available in functional or imperative programming languages: Apply a function to the list items in _List1_, and bind them to the list items in _List2_. Or the reverse. Or check the function results against already bound list items. 

## Form of the two passed lists

Let's distinguish a few cases:

### Both lists are of known length

The two list given to `maplist/3` can be of known length:

````
myprint(X,Y) :- format("~w\n",[[X,Y]]).
?- maplist(myprint,[0,1,2,3],[a,b,c,d]).
[0,a]
[1,b]
[2,c]
[3,d]
true.
````

The same as above, but instead of creating a helper predicate `myprint/2`, we use lambda notation to create a one-liner.
A  call to `myprint/2` is replaced by a call to an "anonymous predicate":

````
?- maplist([X,Y]>>format("~w\n",[[X,Y]]),[0,1,2,3],[a,b,c,d]).
[0,a]
[1,b]
[2,c]
[3,d]
true.
````

Check that a relation holds between the pairwise list elements:

````
?- maplist([X,Y]>>(Y is X*X),[0,1,2,3],[0,1,4,9]).
true.
````

The lists may contain fresh variables. We 
use [`#=`](https://eu.swi-prolog.org/pldoc/doc_for?object=%23%3D%20/%202) constraint predicate to make things interesting:

````
?- use_module(library(clpfd)). % load constraint statisfaction over finite domains

?- maplist([X,Y]>>(Y#=X*X),[0,1,2,3],[A,B,C,D]).
A = 0,
B = 1,
C = 4,
D = 9.

?- maplist([X,Y]>>(Y#=X*X),[0,1,C,D],[A,B,4,9]).
A = 0,
B = 1,
C in -2\/2,
D in -3\/3.

% a simpler relation between between the pairs (I1,I2) than (I2 = I1²) is (I1 = I2-1):

?- maplist([X,Y]>>(X#=Y-1), [0,A,2], [B,1,3]).
A = 0,
B = 1.

% alternatively

?- maplist([X,Y]>>succ(X,Y), [0,A,2], [B,1,3]).
A = 0,
B = 1.

% or lambda-less (and hard to read)

?- maplist(succ, [0,A,2], [B,1,3]).
A = 0,
B = 1.
````

Nothing surprising here.

### One of the lists is of unknown length

Create a list of 11 integers 0..10, then use `maplist/3` to apply a function to this `List1`. If the other list was a fresh variable (so not even known to be a list), it is set to a list of length equal to the length of `List1`:

```logtalk
?- use_module(library(clpfd)). % load constraint statisfaction over finite domains

?- bagof(X,between(0,5,X),In), maplist([X,Y]>>(Y #= X*X*X),In,Out).
In = [0, 1, 2, 3, 4, 5],
Out = [0, 1, 8, 27, 64, 125].

?- AntiIn=[0, 1, 8, 27, 64, 125], maplist([X,Y]>>(Y #= X*X*X),AntiOut,AntiIn).
AntiIn = [0, 1, 8, 27, 64, 125],
AntiOut = [0, 1, 2, 3, 4, 5].
```

### Both of the lists are of unknown length

Without CLP(FD), the predicate called by `maplist/3` errors out rapidly:

```logtalk
?- maplist([X,Y]>>(Y is X*X*X),L1,L2).
L1 = L2, L2 = [] ;
ERROR: Arguments are not sufficiently instantiated
ERROR: In:
ERROR:   [14] _3238 is _3250*_3252*_3246
```

With CLP(FD), `maplist/3` succeeds with longer and longer lists, where constraints exist between variables.

```logtalk
?- use_module(library(clpfd)). % load constraint statisfaction over finite domains

?- maplist([X,Y]>>(Y #= X*X*X),L1,L2).
L1 = L2, L2 = [] ;
L1 = [_11220],
L2 = [_11238],
_11220^3#=_11238 ;
L1 = [_13490, _13496],
L2 = [_13514, _13520],
_13490^3#=_13514,
_13496^3#=_13520 ;
L1 = [_16044, _16050, _16056],
L2 = [_16074, _16080, _16086],
_16044^3#=_16074,
_16050^3#=_16080,
_16056^3#=_16086 
...
```

This is of some interest. We can tell `maplist/3` to stop by misusing `maplist/4`, giving a fixed length list as
"dummy argument":

```logtalk
?- use_module(library(clpfd)). % load constraint statisfaction over finite domains

?- maplist([X,Y,_]>>(Y #= X*X*X),L1,L2,[_,_,_,_]).  % give me 4
L1 = [_22422, _22428, _22434, _22440],
L2 = [_22458, _22464, _22470, _22476],
_22422^3#=_22458,
_22428^3#=_22464,
_22434^3#=_22470,
_22440^3#=_22476.

% Now use those constraints: setting item 1 of L1 to 2 immediately reflects as a 2^3 in L2:

?- maplist([X,Y,_]>>(Y #= X*X*X),L1,L2,[_,_,_,_]),nth0(1,L1,2).
L1 = [_26834, 2, _26846, _26852],
L2 = [_26870, 8, _26882, _26888],
_26834^3#=_26870,
_26846^3#=_26882,
_26852^3#=_26888.
```
 
### One of the lists is a "difference list" prefix, and the other of known length

A difference list prefix is a structure which starts off like a list, but whose tail ends in a fresh variable.
`maplist/3` will adjust the prefix' length and close the list with a `[]`, behaving like a [Caterpillar D7](https://en.wikipedia.org/wiki/Caterpillar_D7):

```logtalk
?- use_module(library(clpfd)). % load constraint statisfaction over finite domains

% The difference list with an unconstrained tail end:

?- DiffList=[1,8,27|T]-T, DiffList=Prefix-_.
DiffList = [1, 8, 27|T]-T,
Prefix = [1, 8, 27|T].

% Using its prefix in maplist/3 constrains the prefix to be a real list, and thus constrains the
% tail likewise

?- DiffList=[1,8,27|T]-T, DiffList=Prefix-_, maplist([X,Y]>>(Y #= X*X*X),[1,2,3,4,5],Prefix).
DiffList = [1, 8, 27, 64, 125]-[64, 125],
T = [64, 125],
Prefix = [1, 8, 27, 64, 125].

% Compute the same as above, but with List1 taking up the x³ values, which is done by reversing
% the argument order to the lambda expression

?- DiffList=[1,8,27|T]-T, DiffList=Prefix-_, maplist([Y,X]>>(Y #= X*X*X),Prefix,[1,2,3,4,5]).
DiffList = [1, 8, 27, 64, 125]-[64, 125],
T = [64, 125],
Prefix = [1, 8, 27, 64, 125].
```

### Both lists are "difference list" prefixes

Maplist generates possible solutions with longer and longer lists on backtracking:

```logtalk
?- use_module(library(clpfd)). % load constraint statisfaction over finite domains

?- DiffList1=[1,2,3|T1]-T1, DiffList1=Prefix1-_,
   DiffList2=[1,8,27|T2]-T2, DiffList2=Prefix2-_,
   maplist([X,Y]>>(Y #= X*X*X),Prefix1,Prefix2).
   
DiffList1 = [1, 2, 3]-[], 
T1 = T2, T2 = [], 
Prefix1 = [1, 2, 3], 
DiffList2 = [1, 8, 27]-[], Prefix2 = [1, 8, 27] ;

DiffList1 = [1, 2, 3, _3136]-[_3136], 
T1 = [_3136], Prefix1 = [1, 2, 3, _3136], 
DiffList2 = [1, 8, 27, _3202]-[_3202],
T2 = [_3202], Prefix2 = [1, 8, 27, _3202], _3136^3#=_3202 ;
...
```

### How about cyclic lists?

(TODO if I know more)

### How about lazy lists?

(TODO if I know more)

## Applications of `maplist/3`

### Computing a function of 1 variable

This is rather straightforward. Just constrain the elements of the second list to the results of a 
function applied to items of the first list.

For example, apply the successor predicate [`succ/2`](https://eu.swi-prolog.org/pldoc/doc_for?object=succ/2)
to relate the pairwise items of two lists. Unlike `X is Y+1`, `succ/2` can be run "forwards" (succwards?) 
or "backwards" (antisuccwards?)

```logtalk
% Helper: Use foldl/4 to create a list of integers

create_list_of_integers(Length,Offset,Lout) :- 
   length(Lout,Length),                % "make sure" that Lout is a list of length "Length"
   foldl([Item,XCur,XNext]>>(succ(XCur,XNext),Item=XCur),Lout,Offset,_XFinal). 
```

And so

```logtalk
% Give only predicate name to maplist

?- create_list_of_integers(6,0,Lin),maplist(succ,Lin,Lout).  % succwards
Lin = [0, 1, 2, 3, 4, 5],
Lout = [1, 2, 3, 4, 5, 6].

?- create_list_of_integers(6,1,Lin),maplist(succ,Lout,Lin).  % antisuccwards by reversing list argument order
Lin = [1, 2, 3, 4, 5, 6],
Lout = [0, 1, 2, 3, 4, 5].

% Alternatively, use lambda notation from yall; you can then rearrange arguments

?- create_list_of_integers(6,0,Lin),maplist([I,O]>>succ(I,O),Lin,Lout).  % succwards
Lin = [0, 1, 2, 3, 4, 5],
Lout = [1, 2, 3, 4, 5, 6].

?- create_list_of_integers(6,1,Lin),maplist([I,O]>>succ(O,I),Lin,Lout). % antisuccwards
Lin = [1, 2, 3, 4, 5, 6],
Lout = [0, 1, 2, 3, 4, 5].
```

Of course, testing of fully grounded lists can be done too:

```logtalk
?- create_list_of_integers(6,0,Lin),maplist([I,O]>>succ(I,O),Lin,Lout),
   format("Now I have two fully grounded lists ~q and ~q\n",[Lin,Lout]),
   maplist([I,O]>>succ(I,O),Lin,Lout),
   format("Pairwise items of those lists are indeed related by the succ predicate!").

Now I have two fully grounded lists [0,1,2,3,4,5] and [1,2,3,4,5,6]
Pairwise items of those lists are indeed related by the succ predicate!
Lin = [0, 1, 2, 3, 4, 5],
Lout = [1, 2, 3, 4, 5, 6].
```

### Tagging of list items

A special case of "applying a function to list items" is _tagging_: adoring terms with functions symbols,
or removing the function symbols. This is done to allow better pattern matching during execution, or
carry additional information about a given term, e.g. whether it's indeed an integer.

Here is a silly example:

```logtalk
% This is a "defaulty" representation. There is no way to distinguish the cases by
% looking at the head if the second argument is fresh. A test must be made in the body.
% That's slow! Note the `!` to commit to the selected branch. 

tag_it(X,int(X))    :- integer(X),!.
tag_it(X,string(X)) :- string(X),!.
tag_it(X,fresh(X))  :- var(X),!.
tag_it(X,atom(X))   :- atom(X),!.
tag_it(X,alien(X)).

% Once a term has been tagged, the correct clause for `process/1` can be chosen at once.
% That's fast!

process(int(X),Y)    :- format("[integer ~q]",[X]), Y is 2*X.
process(string(X),Y) :- format("[string ~q]",[X]), string_concat(X,X,Y).
process(fresh(X),Y)  :- format("[fresh variable ~q]",[X]), Y = [X,X].
process(atom(X),Y)   :- format("[atom ~q]",[X]), atom_concat(X,X,Y).
process(alien(X),Y)  :- format("[alien ~q]",[X]), with_output_to(string(Y),format("~q~q",[X,X])).
```

And so:

```logtalk
% Use maplist to tag them and untag them again

?- maplist(tag_it,[1,2,a,V,f(b,g)],Tagged),maplist(process,Tagged,Untagged).
[integer 1][integer 2][atom a][fresh variable _6590][alien f(b,g)]
Tagged = [int(1), int(2), atom(a), fresh(V), alien(f(b, g))],
Untagged = [2, 4, aa, [V, V], "f(b,g)f(b,g)"].
```

### Verifying the contract of predicates

As for `maplist/2`, `maplist/3` can be used to check that the caller respects the predicate's
[contract](https://en.wikipedia.org/wiki/Design_by_contract) . Here is a simple check to make sure
two lists have the same length. If one of the lists is a fresh variable, it 
is constrained to a new list with as many fresh variables as are in the other:

```logtalk
some_predicate(L1, L2) :-
   ((var(L1),var(L2)) -> fail; true),
   maplist([_,_]>>true,L1,L2),
   format("Contract fulfilled on entry!").
```

And so:

```logtalk
?- some_predicate(L1,L2).
false.

?- some_predicate(L1,[1,2,3]).
Contract fulfilled on entry!
L1 = [_17306, _20144, _20222].

?- some_predicate([a,b,c],[1,2,3]).
Contract fulfilled on entry!
true.
```

## TODO: maplist_relax/3

TODO: Write a maplist that can deal with lists of differing length, and stops with success after as many list pairs as possible have been processed. 

Similarly, maplist_relax/3 should not fail in it entirety if the goal fails, but just stop processing. 

(In that case, what happens on backtracking?)

## Some Software Archeology:

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
