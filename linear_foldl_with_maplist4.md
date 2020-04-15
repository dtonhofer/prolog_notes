# Write a linear `foldl` with `maplist/4`

A [linear `foldl`](https://en.wikipedia.org/wiki/Fold_(higher-order_function)) can be easily built
with [`maplist/4`](https://www.swi-prolog.org/pldoc/doc_for?object=maplist/4) if one doesn't want to use the one which comes with [library(apply)](https://www.swi-prolog.org/pldoc/man?predicate=foldl/4). 

Here is how.

What does `linear foldl` do?

Take the following "list backbone", which is the structure underlying the list representation `[a,b,c,d]`:

```
+---+---+---+---[]    <-- list backbone, terminating in the empty list
|   |   |   |
a   b   c   d         <-- list items
```

(The termination as empty list is SWI-Prolog specific, other implementation may have NULL/nil instead)

Folding this list (aka. reducing, accumulating) according to "foldl" means:

- Recurse down the list backbone
- ... computing a running value according to a two-place function `f` 
- ... when you hit the end
- ... come back up from recursion (or output the value; in any case, no further computation needed).

It could be called "Shallowest First Evaluation.".

`foldl` is subject to call call optimization. The "come up from recursion" can be replaced by a "jump up" and
stack frames may even be collapsed into a single stack frame, transforming recursion into iteration. In Prolog,
there must be no choice-points for that to work, which should be the case here.

`f` is iteratively computing the final output `Out` from two values at each list backbone node, starting with a 
specifiable starter value:

- the previous running value
- the current list item

like this:

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

```logtalk
% ===
% linear foldl via maplist/4
% ===

foldl_maplist(_,[],StarterIsNowOut,StarterIsNowOut) :- !.

foldl_maplist(Predicate,List,Starter,Out) :-
   length(List,Len),                    % Len >= 1
   succ(OtherLen,Len),                  % OtherLen <- Len-1
   length(OtherList,OtherLen),          % create a list of fresh variables
   List1 = [Starter|OtherList],         % List of length Len, fresh variables expect Starter item
   append(OtherList,[Out],List2),       % List of length Len, fresh variables, last item is Out
   maplist(Predicate,List,List1,List2). % Call maplist/4 which constructs goals like Predicate(i1,i2,i3) and calls them

% ===
% Implementations for some folding-f's of interest
% - Adding all item in the list.
% - Multiplying all item in the list.
% - Building a new list from the items (foldl causes the list to be reversed in that case)
% - Building a string joined with ',' from the items
% ===

foldy_add(Item,PrevLine,NextLine)   :- NextLine is Item+PrevLine.
foldy_mult(Item,PrevLine,NextLine)  :- NextLine is Item*PrevLine.
foldy_build(Item,PrevLine,NextLine) :- NextLine = '[|]'(Item,PrevLine).  % '[|]' is SWI-Prolog specific
foldy_join(Item,PrevLine,NextLine)  :- (PrevLine \= "")
                                       -> with_output_to(string(NextLine),format("~w,~w",[Item,PrevLine]))
                                       ;  with_output_to(string(NextLine),format("~w",[Item])).

% ===
% Unit tests
% ===

:- begin_tests(foldl).

% Testing our fold_maplist/4

test(maplist_foldl_add)   :- foldl_maplist(foldy_add   , [1,2,3,4,5],  0 , Out), Out=15.
test(maplist_foldl_mult)  :- foldl_maplist(foldy_mult  , [1,2,3,4,5],  1 , Out), Out=120.
test(maplist_foldl_build) :- foldl_maplist(foldy_build , [1,2,3,4,5], [] , Out), Out=[5,4,3,2,1].
test(maplist_foldl_join)  :- foldl_maplist(foldy_join  , [1,2,3,4,5], "" , Out), Out="5,4,3,2,1".

test(maplist_foldl_add_empty_list)   :- foldl_maplist(foldy_add   , [],  0 , Out), Out=0.
test(maplist_foldl_mult_empty_list)  :- foldl_maplist(foldy_mult  , [],  1 , Out), Out=1.
test(maplist_foldl_build_empty_list) :- foldl_maplist(foldy_build , [], [] , Out), Out=[].
test(maplist_foldl_join_empty_list)  :- foldl_maplist(foldy_join  , [], "" , Out), Out="".

% Compare with the results of foldl/4 from "library(apply)"

test(lib_foldl_add)   :- foldl(foldy_add   , [1,2,3,4,5] ,  0 , Out), Out=15.
test(lib_foldl_mult)  :- foldl(foldy_mult  , [1,2,3,4,5] ,  1 , Out), Out=120.
test(lib_foldl_build) :- foldl(foldy_build , [1,2,3,4,5] , [] , Out), Out=[5,4,3,2,1].
test(lib_foldl_join)  :- foldl(foldy_join  , [1,2,3,4,5] , "" , Out), Out="5,4,3,2,1".

test(lib_foldl_add_empty_list)   :- foldl(foldy_add   , [],  0 , Out), Out=0.
test(lib_foldl_mult_empty_list)  :- foldl(foldy_mult  , [],  1 , Out), Out=1.
test(lib_foldl_build_empty_list) :- foldl(foldy_build , [], [] , Out), Out=[].
test(lib_foldl_join_empty_list)  :- foldl(foldy_join  , [], "" , Out), Out="".

:- end_tests(foldl).

rt :- run_tests(foldl).
```

Running the test:

```
?- rt.
% PL-Unit: foldl ................ done
% All 16 tests passed
true.
```
