# Write a linear `foldl` with `maplist/4`

A [linear `foldl`](https://en.wikipedia.org/wiki/Fold_(higher-order_function)) can be easily built
with [`maplist/4`](https://www.swi-prolog.org/pldoc/doc_for?object=maplist/4). Here is how.

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

```logtalk
% ===
% linear foldl via maplist/4
% ===

foldl_maplist([],Starter,_,Starter) :- !.

foldl_maplist(List,Starter,Predicate,Out) :-
   length(List,Len),                    % Len >= 1
   succ(OtherLen,Len),                  % OtherLen <- Len-1
   length(OtherList,OtherLen),          % create a list of fresh variables
   List1 = [Starter|OtherList],         % List of length Len, fresh variables expect Starter item
   append(OtherList,[Out],List2),       % List of length Len, fresh variables, last item is Out
   maplist(Predicate,List,List1,List2).

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
foldy_join(Item,PrevLine,NextLine)  :- (PrevLine \= [])
                                       -> with_output_to(string(NextLine),format("~w,~w",[Item,PrevLine]))
                                       ;  with_output_to(string(NextLine),format("~w",[Item])).

% ===
% Unit tests
% ===

:- begin_tests(foldl_maplist).

test(foldl_add)   :- foldl_maplist([1,2,3,4,5],  0, foldy_add   ,Out), Out=15.
test(foldl_mult)  :- foldl_maplist([1,2,3,4,5],  1, foldy_mult  ,Out), Out=120.
test(foldl_build) :- foldl_maplist([1,2,3,4,5], [], foldy_build ,Out), Out=[5,4,3,2,1].
test(foldl_join)  :- foldl_maplist([1,2,3,4,5], [], foldy_join  ,Out), Out="5,4,3,2,1".

:- end_tests(foldl_maplist).

rt :- run_tests(foldl_maplist).
```

Running the test:

```
?- rt.
% PL-Unit: foldl_maplist .... done
% All 4 tests passed
true.
```
