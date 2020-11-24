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

With the above, some tests:

[`dict_testing.p`](code/dict_testing.pl)


==

==
 
==
:- begin_tests(dict_pairs).

% Obtain the special blob which is the dict functor name (it cannot be written down)

dict_functor_name(FN) :- compound_name_arity(_{},FN,_).

% This is used to process the special list yielded by compound_name_arguments/3

swappypairzip([V,K|Ms],[K-V|Ps]) :- !,swappypairzip(Ms,Ps).
swappypairzip([],[]).

test("analyze empty dict", true([Name,Pairs] == [foo,[]])) :-
   dict_pairs(foo{},Name,Pairs).

% Pairs list is assumed "ordered" in the next test

test("analyze nonempty dict", true([Name,Pairs] == [foo,[a-x,b-y]])) :-
   dict_pairs(foo{a:x,b:y},Name,Pairs).

% Analyze dict with "compound_name_arguments/3":
% VERY different, much nastier than using dict_pairs

test("analyze empty dict with compound_name_arguments/3",true([Name,DictTag] == [FN,foo])) :-
   dict_functor_name(FN),
   compound_name_arguments(foo{},Name,[DictTag]).

test("analyze nonempty dict with compound_name_arguments/3",true([Name,DictTag,DictContentZippedSorted] == [FN,foo,[a-x,b-y]])) :-
   dict_functor_name(FN),
   compound_name_arguments(foo{a:x,b:y},Name,[DictTag|DictContent]),
   swappypairzip(DictContent,DictContentZipped),       % DictContent has the order of Key, Value reversed!
   keysort(DictContentZipped,DictContentZippedSorted). % and there is no guarantee on order

% Assemble a dict

test("assemble an empty dict",true(Dict == foo{})) :-
   dict_pairs(Dict,foo,[]).

test("assemble a nonempty dict",true(Dict == foo{a:x,b:y,c:z})) :-
   dict_pairs(Dict,foo,[a-x,b-y,c-z]).

test("assemble nonempty dict from differently ordered pairs",true(Dict == foo{a:x,b:y,c:z})) :-
   dict_pairs(Dict,foo,[b-y,a-x,c-z]).

% This throws the non-ISO exception with formal "duplicate_key(Key)"

test("assemble nonempty dict with ambivalent input",error(duplicate_key(a))) :-
   dict_pairs(_,foo,[b-y,a-x,c-z,a-666]).

% Pairs cannot be specified with ":"

test("assemble nonempty dict with pairs that use ':'",error(type_error(_,_))) :-
   dict_pairs(_,foo,[b:y,a:x,c:z]).

:- end_tests(dict_pairs).
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
