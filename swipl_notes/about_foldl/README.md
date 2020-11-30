# Fun with _foldl_

The _Goal_ in a `foldl` call is a predicate, which is either given by a predicate name,
like `foo` (in which case `foldl` will look for a predicate with indicator `foo/3`), 
or a Prolog-style closure, which is a predicate call with some of the leftmost arguments
already defined, like `foo(x,y)` (in which case `foldl` will look with a predicate with indicator `foo/5` 
and always call it with the first arguments set to `x` and `y`).

The three arguments taked on to the _Goal_ are:

   - The current "list element" 
   - The "folded value" up to that point
   - The variable to be instantiated to the next "folded value" 

Diagrammed as a data flow, `foldl(Goal,[a,b,c,d],Starter,Out)` performs the following:

```none
Starter -->--Goal-->--Goal-->--Goal-->--Goal-->--Out
              ^        ^        ^        ^
              |        |        |        | 
          [   a   ,    b   ,    c   ,    d  ]
```

At each node, `Goal` computes the next value "going rightwards" from the value coming
in "from the left" and the list element at the current position.

In the definition of the manual

   - `Starter` = `V0`
   - `Out` = `V`

with the above image, if you name the arguments of the `Goal`

```none
Goal(Element,FromLeft,ToRight)
```

it becomes rather clear what's going on.

Suppose we want to create an atom that starts with `Begin:`, then contains
the concatenation of atoms found in the in input list. 

First define the `Goal` (note that in this case, the `Goal` performs
a permutation of the parameters, then call [`atom_concat/3`](https://eu.swi-prolog.org/pldoc/doc_for?object=atom_concat/3):

```none
mygoal(Element,FromLeft,ToRight) :- atom_concat(FromLeft,Element,ToRight).
```

With the above:

```none
?-
foldl(mygoal,[a,b,c,d],'Begin:',Out).
Out = 'Begin:abcd'.
```

If you use [`library(yall)`](https://eu.swi-prolog.org/pldoc/man?section=yall) you can
inline the _Goal_:

```none
?-
foldl([Element,FromLeft,ToRight]>>atom_concat(FromLeft,Element,ToRight),
      [a,b,c,d],
      'Begin:',
      Out).
Out = 'Begin:abcd'.
```

In particular with the empty input list as input:

```none
?- 
foldl([Element,FromLeft,ToRight]>>atom_concat(FromLeft,Element,ToRight),
      [],
      'Begin:',
      Out).
Out = 'Begin:'.
```

or, exactly the same:

## Another example

Creating a list of integers 0..5:

```none
?- 
length(List,6),  % create a list of 6 fresh variables
foldl(
   [Element,FromLeft,ToRight]>>(succ(FromLeft,ToRight),Element=FromLeft),
   List,
   0,
   _Out).

List = [0, 1, 2, 3, 4, 5],
_Out = 6.
```

In this case it's still easier to just do

```
?- 
bagof(X,between(0,5,X),List).
List = [0, 1, 2, 3, 4, 5].
```

## Another example

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
?- all_false([true,true,false,false]).
false.

?- all_false([false,false,false,false]).
true.

?- all_true([false,false,false,false]).
false.

?- all_true([true,true,true,true]).
true.

?- all_true([]).  % a "vacuous truth": https://en.wikipedia.org/wiki/Vacuous_truth
true.

?- all_false([]). % another "vacuous truth"
true.
```

Simple!

## Another example

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

## An interesting example: A pipeline of operations

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

## Long-ish Explainer

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

Note that `V0` is an "accumulator", which is transformed as the iteration over the list proceeds, eventually appearing in its final form.

A more general case would be "multi-accumulator":

```none
foldl(:Goal, 
      +List, 
      +V0, -V,
      +W0, -W,
      +K0, -K)
```

One can of course pack the multiple accumulators into a single one.
