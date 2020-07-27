# List processing idioms

Suppose you have an "input list" and an "output list", both of the same length, and with the ouput list to be constructed from the input list
according to some algorithm. The relation between the elements need not be pairwise.

It is also possible that both lists are given and that you want to check that "output list" is indeed the result of constructing input list.

## Pairwise processing

If the processing is pairwise, then you do not need to write your own predicate. Using the
metapredicate [maplist/3](https://eu.swi-prolog.org/pldoc/doc_for?object=maplist/3) is a good choice.

For example, the output list is the powers of two of the input list:

```logtalk
power2(In,Out) :- maplist([X,Y]>>(Y is X*X),In,Out).
```

then

```
?- power2([0,1,2,3],Result).
Result = [0, 1, 4, 9].

?- power2([0,1,2,3],[0,1,1,1]).
false.
```

or you can build a result of the actual pairs:

```logtalk
power2pairs(In,OutPairs) :- maplist([X,Y]>>(Y is X*X),In,Powers),maplist([X,Y,X-Y]>>true,In,Powers,OutPairs).
```

then

```
?- power2pairs([0,1,2,3],OutPairs).
OutPairs = [0-0, 1-1, 2-4, 3-9].
```

For more on this, see [Examples for the Prolog predicate `maplist/3]`(../about_maplist/maplist_3_examples.md).

## Non-Pairwise processing

The elements of the output list may have a more complicated relationship to the elements of the input list. For example,
at the ouput value at pos _i_ may be the sum of input values at position _j < i_. Or the output value at posiition _i_ may
be a pair of the input value at position _i_ and _i_ itself.

### In correct order

This is the usual case. Create a predicate that performs a recursive call at last position and which, at each activation:

- takes an input list and an output list (generally a freshvar), plus any extra arguments ; 
- at each activation
   - removes the head of the input list, with the rest given to the recursive call (a monotonically decreasing variant) ;
   - constructs the output list by prepending the ouput value at this position to the output list constructed by the recursive call ;
- the base case just relates the empty input list to the empty output list.

This is subject to tail-call optimization (i.e. transformation by the compiler into a loop) because the last action taken is
the recursive call. The construction of the ouptut list is performed (here, `[I-D|Os]` is done when the present call is made!

```
index_elements(In,Out) :- correct_order_processing(In,Out,0).

correct_order_processing([I|Is],[I-D|Os],D) :-
   succ(D,Dp),
   correct_order_processing(Is,Os,Dp).
   
correct_order_processing([],[],_).
```

then

```
?- index_elements([a,b,c,d,e,f,g],Out).
Out = [a-0, b-1, c-2, d-3, e-4, f-5, g-6].
```

This one is not subject to tail-call optimization although it does exactly the same. BAD!

```
index_elements_bad(In,Out) :- correct_order_processing_non_tco(In,Out,0).

correct_order_processing_non_tco([I|Is],Out,D) :-
   succ(D,Dp),
   correct_order_processing_non_tco(Is,Mid,Dp),
   Out = [I-D|Mid]. % perform some operation after return from the recursive call
   
correct_order_processing_non_tco([],[],_).
```

With `index_elements_bad/2` you can exhaust the stack:

```
?- bagof(N,between(1,10000000,N),Bag), index_elements_bad(Bag,Out).
ERROR: Stack limit (1.0Gb) exceeded
ERROR:   Stack sizes: local: 0.5Gb, global: 0.2Gb, trail: 32.3Mb
```

...with `index_elements/2` things look much better:

```
?- bagof(N,between(1,10000000,N),Bag), index_elements(Bag,Out).
Bag = [1, 2, 3, 4, 5, 6, 7, 8, 9|...],
Out = [1-0, 2-1, 3-2, 4-3, 5-4, 6-5, 7-6, 8-7, ... - ...|...].
```

### In reverse order

Sometimes you want to generate the ouput list in reverse order. Or maybe you are forced to due to the structure of the problem?

Create a predicate that performs a recursive call at last position and which, at each activation:

- takes an input list, an _accumulator_ (initially `[]`) and an output list (generally a freshvar) which passed unmodified to the base case (it allows the caller to "fish" the result out of the depth of the recursion), plus any extra arguments ;
- at each activation
   - removes the head of the input list, with the rest given to the recursive call (a monotonically decreasing variant) ;
   - passes a new accumulator to the recursive call, which has the ouput value at this position prepended ;
- the base case just sets the output list to the received accumulator (shunts the accumulator to the output)

```
index_elements_reverse(In,Out) :- reverse_order_processing(In,[],Out,0).

reverse_order_processing([I|Is],Acc,Out,D) :-
   succ(D,Dp),
   reverse_order_processing(Is,[I-D|Acc],Out,Dp).
   
reverse_order_processing([],Shunt,Shunt,_).
```

And thus:

```text
?- index_elements_reverse([a,b,c,d,e,f,g],Out).
Out = [g-6, f-5, e-4, d-3, c-2, b-1, a-0].
```

Note that this is (probably) subject to tail-optimization, but uses a lot of structure on the global stack:

```
?- bagof(N,between(1,10000000,N),Bag), index_elements_reverse(Bag,Out).
ERROR: Stack limit (1.0Gb) exceeded
ERROR:   Stack sizes: local: 2Kb, global: 0.7Gb, trail: 1Kb
```



