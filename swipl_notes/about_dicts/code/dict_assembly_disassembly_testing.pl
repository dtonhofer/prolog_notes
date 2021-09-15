% ==========
% Demonstrating/Testing SWI-Prolog dicts as described at 
% https://eu.swi-prolog.org/pldoc/man?section=bidicts
% ==========



% ===
% Demonstrating/Testing the "nice way" of assembling and disassembling a dict
% using the dict_pairs/3 predicate.
% ===

:- begin_tests(assembly_disassembly_using_dict_pairs).

test("disassemble empty dict", true([Tag,Pairs] == [foo,[]])) :-
   dict_pairs(foo{},Tag,Pairs).

% Pairs list is assumed "ordered" in the next test

test("disassemble nonempty dict", true([Tag,Pairs] == [foo,[a-x,b-y]])) :-
   dict_pairs(foo{a:x,b:y},Tag,Pairs).

test("assemble an empty dict",true(Dict == foo{})) :-
   dict_pairs(Dict,foo,[]).

test("assemble a nonempty dict",true(Dict == foo{a:x,b:y,c:z})) :-
   dict_pairs(Dict,foo,[a-x,b-y,c-z]).

test("assemble nonempty dict from differently ordered pairs: same result",true(Dict == foo{a:x,b:y,c:z})) :-
   dict_pairs(Dict,foo,[b-y,a-x,c-z]).

% This throws the non-ISO exception with formal "duplicate_key(Key)"

test("assemble nonempty dict with ambivalent input",error(duplicate_key(a))) :-
   dict_pairs(_,foo,[b-y,a-x,c-z,a-666]).

% Pairs cannot be specified with ":"

test("assemble nonempty dict with pairs that use ':'",error(type_error(_,_))) :-
   dict_pairs(_,foo,[b:y,a:x,c:z]).

:- end_tests(assembly_disassembly_using_dict_pairs).



% ===
% Demonstrating/Testing the "ugly way" of assembling and disassembling a dict
% using compound_name_arguments/3.
% 
% ******************************************
% **** Don't do this, use dict_pairs/3! ****
% ******************************************
%
% This is not guaranteed to work if a new implementation comes
% out that doesn't base the dict implementation based on compound terms.
% ===

:- begin_tests(assembly_disassembly_using_compound_name_arity).

% Obtain the special blob which is the dict functor name in FN
% (it cannot be written down and is 'unforgeable')

dict_functor_name(FN) :- 
   compound_name_arity(_{},FN,_).

% The following is used to process the special list yielded by 
% compound_name_arguments/3 applied to a dict

swappypairzip([V,K|Ms],[K-V|Ps]) :- 
   !,swappypairzip(Ms,Ps).
swappypairzip([],[]).

test("disassemble empty dict with compound_name_arguments/3", true([Name,Tag] == [FN,foo])) :-
   dict_functor_name(FN),
   compound_name_arguments(foo{},Name,[Tag]).

test("disassemble nonempty dict with compound_name_arguments/3", true([Name,Tag,DictContentZippedSorted] == [FN,foo,[a-x,b-y]])) :-
   dict_functor_name(FN),
   compound_name_arguments(foo{a:x,b:y},Name,[Tag|DictContent]),
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

:- end_tests(assembly_disassembly_using_compound_name_arity).

