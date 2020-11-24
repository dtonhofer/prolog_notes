% ===
% Nice assembly & analyis using "dict_pairs/3".
% ===

:- begin_tests(assembly_analysis_using_dict_pairs).

test("analyze empty dict", true([Name,Pairs] == [foo,[]])) :-
   dict_pairs(foo{},Name,Pairs).

% Pairs list is assumed "ordered" in the next test

test("analyze nonempty dict", true([Name,Pairs] == [foo,[a-x,b-y]])) :-
   dict_pairs(foo{a:x,b:y},Name,Pairs).

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

:- end_tests(assembly_analysis_using_dict_pairs).

% ===
% Ugly assembly & analyis using "compound_name_arguments/3".
& Don't do this, use dict_pairs/3!
% Plus, this is not guaranteed to work if a new implementation comes out
% that doesn't implement the dict as a compound term.
% ===

:- begin_tests(assembly_analysis_using_compound_name_arity).

% Obtain the special blob which is the dict functor name in FN
% (it cannot be written down and is unforgeable)

dict_functor_name(FN) :- 
   compound_name_arity(_{},FN,_).

% This is used to process the special list yielded by compound_name_arguments/3
% applied to a dict

swappypairzip([V,K|Ms],[K-V|Ps]) :- !,swappypairzip(Ms,Ps).
swappypairzip([],[]).

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

:- end_tests(assembly_analysis_using_compound_name_arity).
