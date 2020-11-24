:- begin_tests(dict_assembly_disassembly).

% Obtain the special blob which is the dict functor name in FN
% (it cannot be written down and is unforgeable)

dict_functor_name(FN) :- 
   compound_name_arity(_{},FN,_).

% This is used to process the special list yielded by compound_name_arguments/3
% applied to a dict

swappypairzip([V,K|Ms],[K-V|Ps]) :- !,swappypairzip(Ms,Ps).
swappypairzip([],[]).

test("analyze empty dict", true([Name,Pairs] == [foo,[]])) :-
   dict_pairs(foo{},Name,Pairs).

% Pairs list is assumed "ordered" in the next test

test("analyze nonempty dict", true([Name,Pairs] == [foo,[a-x,b-y]])) :-
   dict_pairs(foo{a:x,b:y},Name,Pairs).

% Analyze dict with "compound_name_arguments/3":
% VERY different, much nastier than using dict_pairs!
% In fact, it relies on the fact that the dict is implemented as a
% compound term, which may change. **AVOID**

test("analyze empty dict with compound_name_arguments/3",
     true([Name,DictTag] == [FN,foo])) :-
   dict_functor_name(FN),
   compound_name_arguments(foo{},Name,[DictTag]).

test("analyze nonempty dict with compound_name_arguments/3",
     true([Name,DictTag,DictContentZippedSorted] == [FN,foo,[a-x,b-y]])) :-
   dict_functor_name(FN),
   compound_name_arguments(foo{a:x,b:y},Name,[DictTag|DictContent]),
   swappypairzip(DictContent,DictContentZipped),       % DictContent has the order of Key, Value reversed!
   keysort(DictContentZipped,DictContentZippedSorted). % and there is no guarantee on order

% Assemble a dict using dict_pairs/3

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

:- end_tests(dict_assembly_disassembly).
