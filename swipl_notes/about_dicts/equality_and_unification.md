# Dict equality and unification

When comparing two dicts, you can use:

   - `==`, then the tags must pass `==`, the key sets must be equal and all the key-value pairs must pass `==`
   - `=`,  then the tags must unify, the key sets must be equal and the values must unify for each key

There should be a way to perform `==` while disregarding the tag. There is not, but you can use this:

```none
dict_equality_sans_tag(D1,D2) :-
   ((var(D1);is_dict(D1)) -> true ; type_error("dict or var",D1)),
   ((var(D2);is_dict(D2)) -> true ; type_error("dict or var",D2)),   
   (nonvar(D1),nonvar(D2)) 
   -> 
   (assertion((is_dict(D1),is_dict(D2))),
    dict_pairs(D1,_Tag1,Pairs1), % Pairs1 will be ordered by natural order of keys
    dict_pairs(D2,_Tag2,Pairs2), % Pairs2 will be ordered by natural order of keys
    Pairs1 == Pairs2).
```

Some [`plunit`](https://eu.swi-prolog.org/pldoc/doc_for?object=section(%27packages/plunit.html%27)) unit tests:

- [`dict_equality_testing.pl`](code/dict_equality_testing.pl) (uses `dict_equality_sans_tag/2`)
- [`dict_unification_testing.pl`](code/dict_equality_testing.pl)

 
==

==

Ugly disassembly using the basic "compound_name_arguments/3" predicate.
Don't do this, use dict_pairs/3!
Plus, this is not guaranteed to work if a new implementation comes out-

==
:- begin_tests(cna_dict).

dict_functor_name(FN) :- compound_name_arity(_{},FN,_).

test("compound_name_arguments/3 on empty dict", true([Name,Args] == [FN,[alpha]])) :-
   dict_functor_name(FN),
   compound_name_arguments(alpha{},Name,Args).

test("compound_name_arguments/3 on nonempty dict", true([Name,Args] == [FN,[alpha, 1, x, 2, y]])) :-
   dict_functor_name(FN),
   compound_name_arguments(alpha{x:1,y:2},Name,Args).

test("compound_name_arguments/3 on nonempty anonymous dict", true([Name,Args] == [FN,[X, 1, x, 2, y]])) :-
   dict_functor_name(FN),
   compound_name_arguments(_{x:1,y:2},Name,Args),
   Args = [X|_Rest].

:- end_tests(cna_dict).
==
