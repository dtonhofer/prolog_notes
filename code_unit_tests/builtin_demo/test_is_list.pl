% A bunch of unit tests which explains the behaviour of is_list/1
% https://eu.swi-prolog.org/pldoc/doc_for?object=is_list/1

:- begin_tests(is_list).

give_me_cyclic(L) :- L = [1,2,3|L].

test("An open list is not a list",fail) :-
   is_list([a,b,c|_]).

test("An atom is not a list",fail) :-
   is_list(x).

test("A dict is not a list",fail) :-
   is_list(_{}).

test("A freshvar is not a list", fail) :-
   is_list(_).

test("Cyclic term is cyclic") :- 
   give_me_cyclic(L),
   cyclic_term(L).

test("A cyclic list is not a list") :-
   give_me_cyclic(L),
   \+ is_list(L).

:- end_tests(is_list).
 
