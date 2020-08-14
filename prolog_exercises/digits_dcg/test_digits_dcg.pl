:- use_module('digits_dcg.pl').

% load with ?- [test_digits_dcg].
% run with  ?- run_tests.

recognize(InAtom,DigitsAtom,RestAtom,DcgGoal) :- 
   assertion(atom(InAtom)),
   atom_codes(InAtom,InCodes),
   % dynamic construction of phrase-able goal
   compound_name_arguments(FullGoal,DcgGoal,[DigitsAtomVar]),
   phrase(FullGoal,InCodes,RestCodes),
   atom_codes(RestAtom,RestCodes),
   % unfiy to return value in DigitsAtom or to compare with value in DigitsAtom
   DigitsAtom = DigitsAtomVar.

% ---------

:- begin_tests(digits).

test("digits 12345",true([Digits,Rest] == ['12345',''])) :-
   recognize('12345',Digits,Rest,nonempty_atom_of_digits).

test("digits 0",true([Digits,Rest] == ['0',''])) :-
   recognize('0',Digits,Rest,nonempty_atom_of_digits).

test("no digits",fail) :-
   recognize('',_Digits,_Rest,nonempty_atom_of_digits).

test("not digits",fail) :-
   recognize('foo',_Digits,_Rest,nonempty_atom_of_digits).

test("digits with rest",true([Digits,Rest] == ['0123','foo'])) :-
   recognize('0123foo',Digits,Rest,nonempty_atom_of_digits).

:- end_tests(digits).


