:- use_module('digits_dcg.pl').

recognize_atom_to_atom(In,Rest,DcgGoal,Out) :- 
   assertion(nonvar(In)),
   var(Out), % Out not yet set; transfer it back to caller in the end
   !,
   atom_codes(In,InCodes),
   compound_name_arguments(DcgGoalCompleted,DcgGoal,[Out]), % will fill Out with atom
   phrase(DcgGoalCompleted,InCodes,RestCodes),
   atom_codes(Rest,RestCodes), % this instantiates or unifies/compares
   assertion(atom(Out)).
 
recognize_atom_to_atom(In,Rest,DcgGoal,Out) :- 
   assertion(nonvar(In)),
   nonvar(Out), % Out already set; pass it to recognizer goal for checking 
   assertion(atom(Out)),
   !,
   atom_codes(In,InCodes),
   compound_name_arguments(DcgGoalCompleted,DcgGoal,[Out]), % will consult Out
   phrase(DcgGoalCompleted,InCodes,RestCodes),   
   atom_codes(Rest,RestCodes). % this instantiates or unifies/compares

% ---------

:- begin_tests(digits).

test("digits 12345",true(Out == '12345')) :-
   recognize_atom_to_atom('12345','',nonempty_atom_of_digits,Out).

test("digits 0",true(Out == '0')) :-
   recognize_atom_to_atom('0','',nonempty_atom_of_digits,Out).

test("no digits",fail) :-
   recognize_atom_to_atom('',_,nonempty_atom_of_digits,_).

test("not digits",fail) :-
   recognize_atom_to_atom('foo',_,nonempty_atom_of_digits,_).

test("digits with rest",true([Out,Rest] == ['0123','foo'])) :-
   recognize_atom_to_atom('0123foo',Rest,nonempty_atom_of_digits,Out).

:- end_tests(digits).


