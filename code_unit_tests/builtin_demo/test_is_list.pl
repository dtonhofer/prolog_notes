:- begin_tests(is_list).

test("An open list is not a list",fail) :-
   is_list([a,b,c|_]).

test("An atom is not a list",fail) :-
   is_list(x).

test("A dict is not a list",fail) :-
   is_list(_{}).

test("A freshvar is not a list", fail) :-
   is_list(_).

:- end_tests(is_list).
 
