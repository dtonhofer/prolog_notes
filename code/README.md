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

One can immediately build a replace-by-index from `splinter0/5`:

```
replace0(+List, +N, ?NewElement, ?OldElement, ?NewList)
```

- Code: [`splinter0.pl`](splinter0.pl)
- Short link: https://bit.ly/3ekBSU3_prolog

## `rotate_list/3`

Rotate a list "leftwards" or "rightwards".   

```
rotate_list(+List,+N,?Rotated).
```

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
