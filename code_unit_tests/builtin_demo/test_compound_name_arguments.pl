% A bunch of unit tests which explains the behaviour of compound_name_arguments/3
% https://eu.swi-prolog.org/pldoc/doc_for?object=compound_name_arguments/3

:- begin_tests(compound_name_arguments).

dict_functor(F) :- compound_name_arguments(_{},F,_).

test("Analyze an empty list", error(type_error(compound,[]))) :-
   compound_name_arguments([],_,_).

test("Analyze a one-element list", true([Name,Args] == ['[|]',[a,[]])) :-
   compound_name_arguments([a],Name,Args).

test("Analyze a three-element list", true([Name,Args] == ['[|]',[a,[b,c]]])) :-
   compound_name_arguments([a,b,c],Name,Args).

test("Analyze SWI-Prolog specific compound term of arity 0", true([Name,Args] == [a,[]])) :-
   compound_name_arguments(a(),Name,Args).

test("Analyze compound term of arity 3", true([Name,Args] == [a,[x,y,z]])) :-
   compound_name_arguments(a(x,y,z),Name,Args).

test("Assemble SWI-Prolog specific compound term of arity 0", true(C == a()])) :-
   compound_name_arguments(C,a,[]).

test("Assemble compound term of arity 3", true(C == a(x,y,z))) :-
   compound_name_arguments(C,a,[x,y,z]).

% ---
% Use dict_pairs/3 instead of compound_name_arguments/2 as below!!
% ---

test("Analyze SWI-Prolog specific (named) empty dict", true([Name,Args] == [F,[a]])) :-
   compound_name_arguments(a{},Name,Args),
   dict_functor(F).

test("Analyze SWI-Prolog specific (named) dict with two pairs", true([Name,Args] == [F,[a,1,x,2,y]])) :-
   compound_name_arguments(a{x:1,y:2},Name,Args),
   dict_functor(F).

test("Assemble SWI-Prolog specific (named) empty dict", true(D == a{})) :-
   dict_functor(F),
   compound_name_arguments(D,F,[a]),

test("Assemble SWI-Prolog specific (named) dict with two pairs", true(D == a{x:1, y:2})) :-
   dict_functor(F),
   compound_name_arguments(D,F,[a,x,1,y,2),

:- end_tests(compound_name_arguments).
