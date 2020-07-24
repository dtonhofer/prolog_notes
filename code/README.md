# Code grabbag

## Working with strings

- Generate/Test/Enumerate strings of N spaces (and application of [`string_concat/3`](https://eu.swi-prolog.org/pldoc/doc_for?object=string_concat/3))
   - Code: [`string_of_spaces.pl`](strings/string_of_spaces.pl)
   - Unit tests: [`string_of_spaces.plt`](strings/string_of_spaces.plt)

- Convert "stuff" to string or atom (an application of [`atom_string/2`](https://eu.swi-prolog.org/pldoc/doc_for?object=atom_string/2))
   - Code: [`conversion.pl`](strings/conversion.pl)
   - Unit tests: [`conversion.plt`](strings/conversion.plt)

## `vector_nth0/3`

- Code: [`vector_nth0.pl`](vector_nth0.pl)
- Short link: https://bit.ly/2wz9Nrf_prolog

Based on [`nth0/3`](https://eu.swi-prolog.org/pldoc/doc_for?object=nth0/3), this is a "vectorized" version
of `nth0/3`, which retrieves several entries in one call: 

```
vector_nth0(?Indexes,?List,?Items)
```

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

## `splinter0/5`

- Code: [`splinter0.pl`](splinter0.pl)
- Short link: https://bit.ly/3ekBSU3_prolog

Based on [`append/2`](https://eu.swi-prolog.org/pldoc/doc_for?object=append/2), predicate `splinter0/5`, 
"splinters" a list into a prefix, the element at position N and a suffix:

```
splinter0(+List, +N, ?Prefix, ?Element, ?Suffix)
splinter0(?List, ?N, +Prefix, +Element, +Suffix)
```

Examples: 

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

but once starts to consider all the cases depending on arguments being `var(X)`/`nonvar(X)`, it becomes a
bit more involved.

 ## `replace0/5`

One can immediately build a replace-by-index from `splinter0/5`:

```
replace0(List, N, NewItem, OldItem, NewList) :-
   splinter0(List, N, Prefix, OldItem, Suffix),
   append([Prefix, [NewItem], Suffix], NewList).
```

The predicate 

```
replace0(+List, +N, ?NewItem, ?OldItem, ?NewList)
```

is in the same file [`splinter0.pl`](splinter0.pl). 

## `rotate_list/3`

- Code: [`rotate_list.pl`](rotate_list.pl)
- Short link: https://bit.ly/2SbgQh9_prolog

Rotate a list "leftwards" or "rightwards" by N positions.

```
rotate_list(+List,+N,?Rotated).
```

- N=0 : no rotation
- N>0 : Rotate "leftwards": move a prefix of (N mod Length) list items to the back of the list to form the result
- N<0 : Rotate "rightwards": move a suffix of (abs(N) mod Length) list items to the back of the list to form the result (however, this is done completely the same way as for the case N>0) 

Examples:

```
rotate_list([1,2,3],-1,R) ---> R=[3,1,2].
rotate_list([1,2,3],+0,R) ---> R=[1,2,3].
rotate_list([1,2,3],+1,R) ---> R=[2,3,1].
```

## `vector_replace0/4`

- Code: [`vector_replace0.pl`](vector_replace0.pl)
- Short link: https://bit.ly/35F3zTE_prolog

A "vectorized" replace-by-index using `maplist/4`, `foldl/4` and
`library(assoc)` (association lists using AVL trees).

```
vector_replace0(ListIn,ReplacePairs,ListOut,ReplacedPairs)
```

Examples:

```
vector_replace0([a,b,c,d],[3-y,1-x],LO,RPs)         ---> LO=[a,x,c,y], RPs=[1-b,3-d].
vector_replace0([a,b,c,d],[0-e,1-f,2-g,3-h],LO,RPs) ---> LO=[e,f,g,h], RPs=[0-a,1-b,2-c,3-d].
```

## `between_with_step/4`

- Code: [`between_with_step.pl`](between_with_step.pl)
- Short link: https://bit.ly/2WFunjY_prolog

An extended [`between/3`](https://eu.swi-prolog.org/pldoc/doc_for?object=between/3) which takes an additional integer step value. It is not based on `between/3`.

```
between(+Start,+End,+Step,?Value)
between(+Start,+End,+Step,?Value,?OptionList)
```

Examples:

```
?- between(10,20,3,L).
L = 10 ;
L = 13 ;
L = 16 ;
L = 19.

?- between(10,-10,-3,L).
L = 10 ;
L = 7 ;
L = 4 ;
L = 1 ;
L = -2 ;
L = -5 ;
L = -8.
```

`between_with_step/5` additionally takes a list of options. If this list contains `throw_if_empty`, then the predicate will throw a "domain error" if the sequence is empty:

```
?- between(0,+inf,-1,_,[throw_if_empty]).
ERROR: Domain error: .... 

?- between(0,+inf,-1,_).
false.
```

## `misc.pl` 

Predicates which may or may not be useful.

## `unit_tests_for_between.pl`

- Code: [`unit_tests_for_between.pl`](unit_tests_for_between.pl)
- Short link: https://bit.ly/3bD7yBC_prolog

Unit testing [`between/3`](https://eu.swi-prolog.org/pldoc/doc_for?object=between/3).

## `unit_tests_for_must_be.pl`

- Code: [`unit_tests_for_must_be.pl`](unit_tests_for_must_be.pl)
- Short link: https://bit.ly/2Zha7Xy_prolog

Unit testing [`must_be/2`](https://eu.swi-prolog.org/pldoc/doc_for?object=must_be/2).







