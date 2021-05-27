% A bunch of unit tests which explains the behaviour of is_list/1
% https://eu.swi-prolog.org/pldoc/doc_for?object=is_list/1

:- begin_tests(is_list).

give_me_cyclic(L) :- L = [1,2,3|L].

test("An open list is not a list",fail) :-
   is_list([a,b,c|_]).

test("An atom is not a list",fail) :-
   is_list(x).

test("A dict is not a list (actually it's a compound term)",fail) :-
   is_list(_{}).

test("An unbound variable is not a list", fail) :-
   is_list(_List).

test("A cyclic term is cyclic") :- 
   give_me_cyclic(L),
   cyclic_term(L).

test("A cyclic term is not a list") :-
   give_me_cyclic(L),
   \+ is_list(L).

:- end_tests(is_list).
 
