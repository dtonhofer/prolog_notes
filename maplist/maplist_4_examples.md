# Examples for the Prolog predicate `maplist/4` (as run with SWI-Prolog)

- This page can also be accessed as: http://bit.ly/2In2NQj_prolog
- For examples about `maplist/2`, see [this page](maplist_2_examples.md)
- For examples about `maplist/3`, see [this page](maplist_3_examples.md)

## About

A few examples for the predicate [`maplist/4`](https://eu.swi-prolog.org/pldoc/doc_for?object=maplist/4) 
from [library(apply)](https://eu.swi-prolog.org/pldoc/man?section=apply) as run with SWI-Prolog.

`library(apply)`: _"This module defines meta-predicates that apply a predicate on all members of a list."_

In order for lists to be printed fully instead of elided at their end with ellipses ("`|...`") you may have
to first call:

````
?- set_prolog_flag(answer_write_options,[max_depth(0)]).
````

## Intro

The description for [`maplist/4`](https://eu.swi-prolog.org/pldoc/doc_for?object=maplist/4) says:

> `maplist(:Goal, ?List1, ?List2, ?List3)`
>
>     As maplist/2, operating on triples of elements from three lists.

## Examples

### Implementing `foldl/4` with `maplist/4`.

This is rather pointless, but why not:

- [Explainer](https://github.com/dtonhofer/prolog_notes/blob/master/foldl_foldr/linear_foldl_with_maplist4.md)
- [Code](https://github.com/dtonhofer/prolog_notes/blob/master/foldl_foldr/maplist_foldl.pl)

### Assembling/Disassembling a key-value list for sorting

```logtalk
% The little predicate that assembles and disassembles a single Key-Value pair.

ziptwo(K,V,K-V).

% The sorter. It runs maplist/4 twice, around the call to keysort/2.

sort_them(LK,LV,LO) :-
   maplist(ziptwo,LK,LV,LKV),   % Assemble Key-Value list
   keysort(LKV,LKVSo),
   maplist(ziptwo,_,LO,LKVSo).  % Disassemble Key-Value list

% Test data

keylist([b,c,c,a,b,d,b]).
vallist([2,3,2,1,1,4,2]).
sollist([1,2,1,2,3,2,4]).

% One-liner test

go :- keylist(LK),vallist(LV),sort_them(LK,LV,LO),sollist(LO).
```


