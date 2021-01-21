:- use_module(library('heavycarbon/utils/randomly_insert.pl')).

% :- debug(randomly_insert).

:- begin_tests(randomly_insert).

the_test(L) :-
   length(L,5),
   bagof(true,member("XXXXXXX",L),Bag),
   length(Bag,1).

test("randomly inserting into nonempty list, multiple times") :-
   L=[one,two,three,four],
   length(R,20),
   % Fill list R with examples of having randomly inserted "XXXXXXX" into L
   maplist(([LN]>>randomly_insert("XXXXXXX",L,LN)),R),
   % Perform some slight verification
   debug(randomly_insert,"We obtained ~q",[R]),
   maplist(the_test,R).

test("randomly inserting into empty list", true(R == ["XXXXXXX"])) :-
   randomly_insert("XXXXXXX",[],R).

:- end_tests(randomly_insert).


