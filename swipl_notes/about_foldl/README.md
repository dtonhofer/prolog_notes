# Fun with _foldl_

This is a companion README for the SWI-Prolog manual page [`fold/4`](https://eu.swi-prolog.org/pldoc/doc_for?object=foldl/4).

- [Intro](#intro)
- [Implementation caveats](#implementation_caveats)
- [Overbearing mode indicators in the manual](#overbearing)
- [Where is SWI-Prolog's _foldr_?](#foldr)
   - [Implementing one-list _foldr/4_ with _reverse/2_ and the one-list _foldl/4_](#implement_foldr)
- [Examples](#examples)
   - [Example 1: Simple atom concatenation](#simple_atom_concatenation)
   - [Example 2: Creating a list of monotonically increasing integers](#monotonically_increasing_integers)
   - [Example 3: Logical operations over a list](#logical_operations_over_a_list)
   - [Example 4: Filtering by occurence count](#filtering_by_occurence_count)
   - [Example 5: A pipeline of operations (edgy usage)](#pipeline_of_operations)
   - [Example 6: maplist emulation](#maplist_emulation)
   - [Example 7: An infinite list of random numbers](#infinite_list_of_random_numbers)
- [Compare _maplist_, _foldl_, _scan_ for mapping a list](#compare)


## Intro<a name="intro" />

`foldl(Goal,List,Starter,Out)`

The _Goal_ in a `foldl` call is the stem of a predicate call. This can be either:

- an atom, giving a predicate name, as in `foo`, in which case `foldl` will look for
  a predicate with indicator `foo/3`, or
- a Prolog-style closure, which is a predicate call with some of the leftmost arguments already specified,
  as in `foo(x,y)`, in which case `foldl` will look with a predicate with indicator `foo/5` 
  and always call it with the first arguments set to `x` and `y`.

The three arguments tacked onto the predicate call to which _Goal_ is the stem are:

   - The current "list element" in argument `List`
   - The "folded value" up to that point
   - The variable to be instantiated to the next "folded value" 

Diagrammed as a data flow, `foldl(Goal,[a,b,c,d],Starter,Final)` performs the following:

```none
Starter -->--Goal-->--Goal-->--Goal-->--Goal-->--Final
              ^        ^        ^        ^
              |        |        |        | 
          [   a   ,    b   ,    c   ,    d  ]
```

At each node, `Goal(Element,FromLeft,ToRight)` computes the next value "going rightwards"
from the value coming in "from the left" and the list element at the current position.

In the definition of the manual

   - `Starter` = `V0`
   - `Final` = `V`

Note that `FromLeft,ToRight` form the pair of an "accumulator weave". The initial 
accumulator value is `Starter`. At each weave in and out of `Goal` the current
accumulator values is "upticked" to its next generation, eventually ending in
`Final`.

A more general case would be a "multi-accumulator waeve", for example, if
one wanted to weave three accumulators:

```none
foldl(:Goal,                  % Goal(Element,InA,OutA,InB,OutB,InC,OutC)
      +List, 
      +StarterA, -FinalA,     % Accumulator weave A
      +StarterB, -FinalB,     % Accumulator weave B 
      +StarterC, -FinalC)     % Accumulator weave C
```

One can of course pack/unpack the multiple accumulators into a single one.

## Implementation caveats<a name="implementation_caveats" />

`foldl`, when applied to multiple lists, does *not* check first whether the lengths of the lists are all equal. It will fail at 
the earliest end-of-list encountered in case list lengths differ. The caller has to take precautions accordingly if this is 
considered costly.

`foldl` can also handle open lists (lists with an unbound variable at the final position (the Fin position))
as argument `List`, for example `[a,b,c|_]`. In that case `foldl` will generate (and process through Goal) longer and longer
lists on redo. This includes the case where the `List` is just an unbound variable.
Indeed, an unbound variable is considered to belong to the set of "open lists" even thought it could be _anything_,
even a non-list. On the other hand, that is arguably borderline, as the predicate indicator for `List` is `+` in
`foldl(:Goal, +List, +V0, -V)`, so unbound variables are in principle not allowed here. But it works:

```
?- foldl([Element,FromLeft,Element]>>succ(FromLeft,Element),L,0,Final).
L = [], Final = 0 ;
L = [1], Final = 1 ;
L = [1,2], Final = 2 ;
L = [1,2,3], Final = 3 ;
L = [1,2,3,4], Final = 4 ;
L = [1,2,3,4,5], Final = 5 ;
L = [1,2,3,4,5,6], Final = 6 ;
...
```

```
?- foldl([Element,FromLeft,Element]>>succ(FromLeft,Element),[4,5,6|L],3,Final).
L = [], Final = 6 ;
L = [7], Final = 7 ;
L = [7,8], Final = 8 ;
L = [7,8,9], Final = 9 
```

## Overbearing mode indicators in the manual<a name="overbearing" />

The [mode indicators](https://eu.swi-prolog.org/pldoc/man?section=preddesc) are (currently, 2020-12):

```
foldl(:Goal, +List, +V0, -V)
foldl(:Goal, +List1, +List2, +V0, -V)
foldl(:Goal, +List1, +List2, +List3, +V0, -V)
foldl(:Goal, +List1, +List2, +List3, +List4, +V0, -V)
```
   
- The `+` mode indicator on `List` arguments may be too much: 
  as said above, open lists and and empty open list, i.e. an unbound variable, are also allowed here.
- The `+V0` and `-V` are definitely too much: `foldl/N` cannot specify those as they depend
  on `Goal`. `foldl/N` just passes them along. I guess those indictors can be disregarded.

## Where is SWI-Prolog's _foldr_?<a name="foldr" /> 

No implementation for a corresponding `foldr` is given. A `foldr` implementation would consist in first calling `reverse/2`
on each of the _m_ input lists, then applying the appropriate `foldl`. This is actually **more efficient** than using a properly 
programmed-out recursive algorithm that cannot be tail-call optimized. 

Be aware, however, that this approach is fragile if open lists are involved: a failing `foldl` will create an infinite 
failure-driven loop with any preceding reverse/2 working on an open list!

Here is **properly recursive code**, which mirrors the `foldl` code. If you need it, you can grab it:

   - [`foldr_recursive.pl`](/other_notes/about_foldl_and_foldr/foldr_for_library_apply/foldr_recursive.pl)
   - [Unit tests](/other_notes/about_foldl_and_foldr/foldr_for_library_apply/tests)

### Implementing one-list _foldr/4_ with _reverse/2_ and the one-list _foldl/4_<a name="implement_foldr"></a>

Naturally one would say:

```
foldr(Goal,List,Start,Final) :-
   reverse(List,Lrev),
   foldl(Goal,Lrev,Start,Final).
```

But suppose we have this Goal:

```
% printox(?Element,+FromLeft,+ToRight)

printox(E,FL,TR) :-
   (E == x)
   -> fail
   ; 
   (
     format(string(S),"~q",[E]),
     atomic_list_concat([FL,'(',S,')'],TR)
   ).
```

With no `x` in the input and a closed list, things work well:

```
?- foldr(printox,[a,b,c],'s',F).
F = 's(c)(b)(a)'.
```

With `x` in the input and a closed list, things work well:

```
?- foldr(printox,[a,x,c],'s',F).
false.
```

With no `x` in the input and an open list, things work well:

```
?- foldr(printox,[a,b,c|_],'s',F).
F = 's(c)(b)(a)' ;
F = 's(_12194)(c)(b)(a)' ;
F = 's(_12194)(_12206)(c)(b)(a)' ;
F = 's(_12194)(_12206)(_12218)(c)(b)(a)'  
etc..
```

But with an `x` in the input and an open list, `foldl/4` fails, and you have an
infinite failure-driven loop: `reverse/2` generates longer and longer lists to be
folded ... but `foldl` never succeeds:

```
?- foldr(printox,[a,x,c|_],'s',F).
*adios*
```

You want to:

   - Succeed the `foldr/4` predicate with the possibility of backtracking if `foldl/4` succeeds.
   - Fail the `foldr/4` predicate with no possibility of backtracking if `foldl/4` fails.

Thus:

```
foldr(Goal,List,Start,Final) :-
   reverse(List,Lrev),
   (foldl(Goal,Lrev,Start,Final)
    ->
    true
    ;
    (!,fail)). % looks weird but it works
```

Now the predicate is safe:

```
?- foldr(printox,[a,x,c|_],'s',F).
false.

?- foldr(printox,[a,b,c|_],'s',F).
F = 's(c)(b)(a)' ;
F = 's(_8310)(c)(b)(a)' ;
F = 's(_8310)(_8322)(c)(b)(a)' ;
F = 's(_8310)(_8322)(_8334)(c)(b)(a)' .
```


## Long-ish _foldl_ and _foldr_ explainers

I had some fun explaining and writing a `foldl/4` and `foldr/4`, complete with test cases.

A markdown page with lots of text, implementations and unit tests is here: 

[**Linear `foldl` and `foldr` in Prolog**](/other_notes/about_foldl_and_foldr)

Directly from that page, the implementation of `foldr/4` on a list (a `foldr/4` is missing in library(apply)):

```none
foo_foldr(Foldy,[Item|Items],Starter,AccUp) :-    % case of nonempty list
   !,                                             % GREEN CUT for determinism
   foo_foldr(Foldy,Items,Starter,AccUpPrev),
   call(Foldy,Item,AccUpPrev,AccUp).

foo_foldr(_,[],Starter,AccUp) :-                  % empty list: bounce Starter "upwards" into AccUp
   AccUp=Starter.                                 % unification not in head for clarity
```

The indicated page also includes a (completely pointless) implementation of `foldl/4` based on `maplist/5`. 

Also good to peruse: [The Wikipedia entry on "linear folds"](https://en.wikipedia.org/wiki/Fold_(higher-order_function)#Linear_folds)

## Naming


## Comparing _maplist_, _foldl_, _scan_ for mapping a list<a name="compare"/>

These all do the same thing:

```
% ---
% Map a list using maplist/3. 
% ---

map_using_maplist(Mapper,ListIn,ListOut) :-
   maplist(in_maplist(Mapper),ListIn,ListOut).

in_maplist(Mapper,In,Out) :- get_dict(In,Mapper,Out).

% ---
% Map a list using foldl/4. 
% ---
    
map_using_foldl(Mapper,ListIn,ListOut) :-
   assertion(var(ListOut)),                     % ListOut must be an unbound variable
   Tip=Fin,                                     % initialize open list consisting of a single variable (named by both "Tip" and "Fin")
   foldl(in_foldl(Mapper),ListIn,Fin,FinOut),   % uptick "Fin" to "FinOut" (in_foldl/4 repeatedly appends to the open list)
   FinOut=[],                                   % close the open list by unifying its Fin with []
   assertion(is_list(Tip)),                     % this is now the case
   ListOut=Tip.                                 % the list we seek *is* the list at "Tip"

% "In" is the "Element",
% "FinIn" is what comes "from the left",
% "FinOut" is what "goes to the right"

in_foldl(Mapper,In,FinIn,FinOut) :- get_dict(In,Mapper,Out),FinIn=[Out|FinOut].
   
% ---
% Map a list using foldl/4, more compactly (but exactly as above)
% ---
    
map_using_foldl_compact(Mapper,ListIn,ListOut) :-
   assertion(var(ListOut)),
   foldl(in_foldl_compact(Mapper),ListIn,ListOut,[]).

in_foldl_compact(Mapper,In,[Out|FinOut],FinOut) :- get_dict(In,Mapper,Out).  

% ---
% Map using scanl/4
% ---   

map_using_scanl(Mapper,ListIn,ListOut) :-
   scanl(in_scanl(Mapper),ListIn,dontcare,ListMid),
   ListMid=[dontcare|ListOut].

in_scanl(Mapper,In,_,Out) :- get_dict(In,Mapper,Out).
   
% ---
% Run tests
% ---

:- begin_tests(various_ways_to_map).

const_mapper(mapper{ a:1, b:x, c:y, d:4, e:5, f:z }).
const_list_in(List) :- atom_chars(abcdabfa,List).
const_list_out([1,x,y,4,1,x,z,1]).

test(maplist,true(ListOut == Expected)) :- 
   const_mapper(Mapper),
   const_list_in(ListIn),
   const_list_out(Expected),
   map_using_maplist(Mapper,ListIn,ListOut).
   
test(foldl,true(ListOut == Expected)) :- 
   const_mapper(Mapper),
   const_list_in(ListIn),
   const_list_out(Expected),
   map_using_foldl(Mapper,ListIn,ListOut).

test(foldl_compact,true(ListOut == Expected)) :- 
   const_mapper(Mapper),
   const_list_in(ListIn),
   const_list_out(Expected),
   map_using_foldl_compact(Mapper,ListIn,ListOut).
   
test(scanl,true(ListOut == Expected)) :- 
   const_mapper(Mapper),
   const_list_in(ListIn),
   const_list_out(Expected),
   map_using_scanl(Mapper,ListIn,ListOut).
   
:- end_tests(various_ways_to_map).   
```

## Examples<a name="examples" />

### Example 1: Simple atom concatenation<a name="simple_atom_concatenation" />

Suppose we want to create an atom that starts with `Begin:`, then contains
the concatenation of atoms found in the in input list. 

First define the `Goal` (note that in this case, the `Goal` performs
a permutation of the parameters, then calls
[`atom_concat/3`](https://eu.swi-prolog.org/pldoc/doc_for?object=atom_concat/3):

```none
mygoal(Element,FromLeft,ToRight) :- atom_concat(FromLeft,Element,ToRight).
```

Using just `atom_concat/3`, elements are prepended

```none
?- 
foldl(atom_concat,[a,b,c,d],'Begin:',Out).
Out = 'dcbaBegin:'.
```

But with rearranged arguments via `mygoal/3`, elements are appended:

```none
?-
foldl(mygoal,[a,b,c,d],'Begin:',Out).
Out = 'Begin:abcd'.
```

If you use [`library(yall)`](https://eu.swi-prolog.org/pldoc/man?section=yall) you can
inline the _Goal_:

```none
?-
foldl([E,FL,TR]>>atom_concat(FL,E,TR),
      [a,b,c,d],
      'Begin:',
      Out).
Out = 'Begin:abcd'.
```

In particular with the empty input list as input:

```none
?- 
foldl([E,FL,TR]>>atom_concat(FL,E,TR),[],'Begin:',Out).
Out = 'Begin:'.
```

### Example 2: Creating a list of monotonically increasing integers<a name="monotonically_increasing_integers" />

Creating a list of integers 0..5:

```none
?- 
length(L,5), % create a list of 6 fresh variables
foldl([E,FL,TR]>>(succ(FL,TR),E=FL), L, 0, Final).

L = [0, 1, 2, 3, 4],
Final = 5.
```

In this case it's still easier to just do

```
?- 
bagof(X,between(0,5,X),List).
List = [0, 1, 2, 3, 4, 5].
```

### Example 3: Logical operations over a list<a name="logical_operations_over_a_list" />

If you have a list containing atoms `true` and `false`, check whether it contains only `true`:

```none
andify_rightwards(true,true,true).
andify_rightwards(true,false,false).
andify_rightwards(false,true,false).
andify_rightwards(false,false,false).

all_true(List) :-
    foldl([E,FromLeft,ToRight]>>once(andify_rightwards(FromLeft,E,ToRight)),
       List,
       true,
       Out),
   Out == true.
```

Check whether it contains only `false`:

```none
andify_negation_rightwards(true,false,true).
andify_negation_rightwards(true,true,false).
andify_negation_rightwards(false,true,false).
andify_negation_rightwards(false,false,false).

all_false(List) :-
   foldl([E,FromLeft,ToRight]>>once(andify_negation_rightwards(FromLeft,E,ToRight)),
      List,
      true,
      Out),
   Out == true.
```

And so:

```none
?- 
all_false([true,true,false,false]).
false.

?- 
all_false([false,false,false,false]).
true.

?- 
all_true([false,false,false,false]).
false.

?- 
all_true([true,true,true,true]).
true.

?- 
all_true([]).  % a "vacuous truth": https://en.wikipedia.org/wiki/Vacuous_truth
true.

?- 
all_false([]). % another "vacuous truth"
true.
```

Simple!

### Example 4: Filtering by occurence count<a name="filtering_by_occurence_count" />

Filtering the elements in a list that occur more than _limit_ times. This is effortlessly done using SWI-Prolog _dicts_.

```
% The program

filter_occurrences(List,Limit,Reacheds,Finals) :-
   foldl(inc_for_key,List,_{},Finals),
   findall(Key,(Finals.Key >= Limit),Reacheds).

inc_for_key(Key,DictIn,DictOut) :-
   (get_dict(Key,DictIn,X) -> succ(X,XP) ; XP=1),
   put_dict(Key,DictIn,XP,DictOut).

% The unit tests

:- begin_tests(filter_occurrences_less_than_n).

test("filter empty list",true(R == [])) :-
   filter_occurrences([],3,R,_).

test("filter nonempty list #1 (limit 3)",true([R,Finals] == [[a,c],foo{a:4,b:2,c:3,d:1,e:1,f:1}])) :-
   filter_occurrences([a,b,c,d,c,e,b,a,a,f,a,c],3,R,Finals),
   dict_pairs(Finals,foo,_). % Sets the tag of the Finals dict to "foo"

test("filter nonempty list #2 (limit 4)",true([R,Finals] == [[a],foo{a:4,b:2,c:3,d:1,e:1,f:1}])) :-
   filter_occurrences([a,b,c,d,c,e,b,a,a,f,a,c],4,R,Finals),
   dict_pairs(Finals,foo,_). % Sets the tag of the Finals dict to "foo"

test("filter nonempty list #3 (limit 5)",true([R,Finals] == [[],foo{a:4,b:2,c:3,d:1,e:1,f:1}])) :-
   filter_occurrences([a,b,c,d,c,e,b,a,a,f,a,c],5,R,Finals),
   dict_pairs(Finals,foo,_). % Sets the tag of the Finals dict to "foo"

:- end_tests(filter_occurrences_less_than_n).
```

And so:

```
?- run_tests.
% PL-Unit: filter_occurrences_less_than_n .... done
% All 4 tests passed
true.
```

### Example 5: A pipeline of operations (edgy usage)<a name="pipeline_of_operations" />

First, a common base:

```none
express(Starter,Operations,Out) :-
   must_be(list(compound),Operations),
   foldl(
      [Op,FromLeft,ToRight]>>(
         call(Op,FromLeft,ToRight),
         debug(express,"Operation is ~q; Got ~q from left, sending ~q to right",[Op,FromLeft,ToRight])),
      Operations,
      Starter,
      Out).
```

Create a predicate setting up a pipeline with specific operations:

```none
main(In,Out) :-
   % This actually works (!) although X and Y are indeed
   % shared in between the yall lambda expressions.
   % I suppose any binding must be removed by backtracking after
   % the lambda call finishes.
   Ops = [
      [X,Y]>>(Y is X*12),
      [X,Y]>>(Y is X-4),
      [X,Y]>>(Y is sqrt(X))
   ],
   express(In,Ops,Out).
```

Running it:

```none
?- 
debug(express).

?- 
main(1,X).
% Operation is [_12168,_12174]>>(_12174 is _12168*12); Got 1 from left, sending 12 to right
% Operation is [_12168,_12174]>>(_12174 is _12168-4); Got 12 from left, sending 8 to right
% Operation is [_12168,_12174]>>(_12174 is sqrt(_12168)); Got 8 from left, sending 2.8284271247461903 to right
X = 2.8284271247461903.

?- main(-1,X).
% Operation is [_22342,_22348]>>(_22348 is _22342*12); Got -1 from left, sending -12 to right
% Operation is [_22342,_22348]>>(_22348 is _22342-4); Got -12 from left, sending -16 to right
ERROR: Arithmetic: evaluation error: `undefined'
```

With the `espress/3` of above, "lazify" the operations, setting up just the symbolic arithmetic expression:

```none
main_lazy(In,Out) :-
   % This actually works (!) although X and Y are indeed
   % shared in between the yall lambda expressions.
   % I suppose any binding must be removed by backtracking after
   % the lambda call finishes.
   Ops = [
      [X,Y]>>(Y = X*12),
      [X,Y]>>(Y = X-4),
      [X,Y]>>(Y = sqrt(X))
   ],
   express(In,Ops,Out).
```

Then:

```
?- 
main_lazy(I,Out).
Out = sqrt(I*12-4).

?- 
main_lazy(I,Out), I=14, Sought is Out.
I = 14,
Out = sqrt(14*12-4),
Sought = 12.806248474865697.
```

### Example 6: maplist emulation<a name="maplist_emulation" />

As nothing holds us back from passing "rightwards" complex values, we can emulate `maplist/3`, passing 
an ever-growing result list to append to.

In fact, `foldl/4` can be used to emulate `maplist/3` but not the reverse.

We do not make this fully general. Suppose we want to emulate "compute X^2 for each X of the input list".

With `maplist/3`, one would do this:

```
maplist_pow2(Lin,Lout) :- maplist([X,Xpow2]>>(Xpow2 is X**2),Lin,Lout).
```

And so:

```
?- 
maplist_pow2([],Lout).
Lout = [].

?- 
maplist_pow2([0,1,2,3,4,5],Lout).
Lout = [0, 1, 4, 9, 16, 25].
```

Let's base the same on `foldl/5` and a "difference list of an open list"
so that growing a list at its end can be done efficiently. Then:

```
subgoal_pow2(X,Tip-Fin,Tip-FreshFin) :-
   Xpow2 is X**2,         % Compute element value
   Fin=[Xpow2|FreshFin].  % Wxtend the open list rooted at Tip at its Fin,
                          % making Fin reference a new listcell with the 
                          % newly computed value and a fresh Fin.
   
foldl_pow2(Lin,Lout) :-   
   foldl(subgoal_pow2,Lin,T-T,Lout-[]).   
%                          ^    ^  
%                          |    |
%                          |    +-- Final iteration of the Tip-Fin pair:
%                          |        The Tip is unified with the result, the Fin is unified with [], closing the
%                          |        open list rooted at Tip.
%                          |
%                          +-- Starter value: a pair of unbound variables.
%                              The first pair element will be bound to the whole open list being constructed on first call.
%                              (i.e. the first listcell of the list backbone, what I call Tip): Tip = [1,2,3|Fin]
%                              The second pair element will always be an unbound variable and denote the empty cell                            
%                              referenced by the final listcell of the list backbone, what I call Fin)
```

Then:

```
?- foldl_pow2([],Lout).
Lout = [].

?- foldl_pow2([0,1,2,3,4,5],Lout).
Lout = [0, 1, 4, 9, 16, 25].
```

### Example 7: An infinite list of random numbers<a name="infinite_list_of_random_numbers">

`foldl/4` can work with an open list. 

So we can for example just ignore `FromLeft` and `ToRight` and instantiate the unbound `Element` we are given to a random integer:

```
?- foldl([Element,FromLeft,ToRight]>>random_between(1,6,Element),L,_,_).
L = [] ;
L = [4] ;
L = [4, 3] ;
L = [4, 3, 1] ;
L = [4, 3, 1, 5] ;
L = [4, 3, 1, 5, 4] ;
L = [4, 3, 1, 5, 4, 5] 
```

It will never stop!

One can evidently handle more than one list, too:

```
?- foldl([E1,E2,E3,_,_]>>(random_between(1,6,E1),random_between(1,6,E2),random_between(1,6,E3)),L1,L2,L3,_,_).
L1 = L2, L2 = L3, L3 = [] ;
L1 = [1],
L2 = L3, L3 = [5] ;
L1 = [1, 3],
L2 = [5, 6],
L3 = [5, 1] ;
L1 = [1, 3, 4],
L2 = [5, 6, 4],
L3 = [5, 1, 3] ;
L1 = [1, 3, 4, 2],
L2 = [5, 6, 4, 4],
L3 = [5, 1, 3, 5] ;
```

