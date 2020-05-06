# Code grabbag

## `vector_nth0/3`

Based on [`nth0/3`](https://eu.swi-prolog.org/pldoc/doc_for?object=nth0/3), this is a "vectorized" version
of `nth0/3`, which retrieves several entries in one call: 

`vector_nth0(Indexes,List,Items)`

- Code: [`vector_nth0.pl`](code/vector_nth0.pl)
- Short link: https://bit.ly/2wz9Nrf_prolog

Example, working forwards:

```
vector_nth0([],[a,b,c,d,e,f],Items)      --> Items=[]
vector_nth0([0,1,2],[a,b,c,d,e,f],Items) --> Items=[a,b,c]
vector_nth0([2,0,1],[a,b,c,d,e,f],Items) --> Items=[c,a,b]
vector_nth0([3,3,3],[a,b,c,d,e,f],Items) --> Items=[d,d,d]
```

Example, finding a matching list:

```
vector_nth0([0,1,2],[_,_,_,_,_,_],[a,b,c]) --> [a,b,c,_,_,_]
```

Example, finding matching indexes:

```
vector_nth0(_9584,[a,b,c,b,e,c,g],[b,c,a]) --> [1,2,0],[a,b,c,b,e,c,g],[b,c,a];
                                               [1,5,0],[a,b,c,b,e,c,g],[b,c,a];
                                               [3,2,0],[a,b,c,b,e,c,g],[b,c,a];
                                               [3,5,0],[a,b,c,b,e,c,g],[b,c,a]
```


## `splinter0/5` and `replace0/5`

- Code: [`splinter0.pl`](splinter0.pl)
- Short link: https://bit.ly/3ekBSU3_prolog

Based on [`append/2`](https://eu.swi-prolog.org/pldoc/doc_for?object=append/2), predicate `splinter0/5`, 
which "splinters" a list into a prefix, the element at position N and a suffix:

```
splinter0(+List, +N, ?Prefix, ?Element, ?Suffix)
splinter0(?List, ?N, +Prefix, +Element, +Suffix)
```

```
[a,b,c,d,e,f,g,h] N=0 ==> [] a [b,c,d,e,f,g,h]
[a,b,c,d,e,f,g,h] N=1 ==> [a] b [c,d,e,f,g,h]
[a,b,c,d,e,f,g,h] N=2 ==> [a,b] c [d,e,f,g,h]
[a,b,c,d,e,f,g,h] N=3 ==> [a,b,c] d [e,f,g,h]
...
```
In principle, this is not more than

```
splinter0(List, N, Front, Element, Back) :-
   length(Front,N),
   append(Front,[Element|Back],List).
```

but once starts to consider all the cases depending on arguments being `var(X)`/`nonvar(X)`, it becomes a bit more involved.

One can immediately build a replace-by-index from `splinter0/5`:

```
replace0(+List, +N, ?NewElement, ?OldElement, ?NewList)
```

It's in the same file.

## `rotate_list/3`

Rotate a list "leftwards" or "rightwards" by N positions.

Initially based on [`append/2`](https://eu.swi-prolog.org/pldoc/doc_for?object=append/2) (but no longer).

```
rotate_list(+List,+N,?Rotated).
```

- N=0 : no rotation
- N>0 : Rotate "leftwards": move a prefix of (N mod Length) list items to the back of the list to form the result
- N<0 : Rotate "rightwards": move a suffix of (abs(N) mod Length) list items to the back of the list to form the result (however, this is done completely the same way as for the case N>0) 

- Code: [`rotate_list.pl`](rotate_list.pl)
- Short link: https://bit.ly/2SbgQh9_prolog

## `vector_replace0/4`

A "vectorized" replace-by-index using `maplist/4`, `foldl/4` and
`library(assoc)` (association lists via AVL trees).

```
vector_replace0(ListIn,ReplacePairs,ListOut,ReplacedPairs)
```

- Code: [`vector_replace0.pl`](vector_replace0.pl)
- Short link: https://bit.ly/3aGYhIk_prolog
