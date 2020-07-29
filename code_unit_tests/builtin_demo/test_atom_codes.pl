% Some unit tests for atom_codes/2
% https://eu.swi-prolog.org/pldoc/doc_for?object=atom_codes/2

:- begin_tests(atom_codes).

% ---
% listwards tests
% ---

test("listwards atom",true(Cs == [97, 98, 99])) :-
   atom_codes(abc,Cs).

test("listwards char",true(Cs == [97])) :-
   atom_codes(a,Cs).

test("listwards string",true(Cs == [97, 98, 99])) :-
   atom_codes("abc",Cs).

test("listwards integer",true(Cs == [49, 50, 51])) :-
   atom_codes(123,Cs).

test("listwards float",true(Cs == [49, 50, 51, 46, 53])) :-
   atom_codes(123.5,Cs).

test("listwards rational",true(Cs == [51, 114, 53])) :-
   atom_codes(3r5,Cs).

test("listwards empty atom",true(Cs == [])) :-
   atom_codes('',Cs).

test("listwards empty string",true(T = (Cs == []))) :-
   atom_codes("",Cs).

test("listwards katakana",true(Cs == [12450, 12452, 12454])) :-
   atom_codes('アイウ',Cs).

% ---
% tests that result in exceptions
% ---

test("listwards compound throws",error(type_error(atom,x(1)))) :-
   :- atom_codes(x(1),_).

test("atomwards compound throws",error(type_error(list,x(1)))) :-
   :- atom_codes(_,x(1)).

test("nowards throws",error(instantiation_error)) :-
   :- atom_codes(_,_).

test("atomwards strings throw",error(type_error(character_code,"a"))) :-
   :- atom_codes(_,["a"]).

% ---
% atomwards tests
% ---

test(atomwards_codes,true(A == abc)) :-
   atom_codes(A,[97, 98, 99]).

test(atomwards_chars,true(A == abc)) :-
   atom_codes(A,[a,  b,  c]).

test(atomwards_code,true(A == a)) :-
   atom_codes(A,[97]).

test(atomwards_char,true(A == a)) :-
   atom_codes(A,[a]).

test(atomwards_empty,true(A == '')) :-
   atom_codes(A,[]).

test(atomwards_codes_n1,true(A == '123')) :-
   atom_codes(A,[49, 50, 51]).

test(atomwards_codes_n2,true(A == '123.5')) :-
   atom_codes(A,[49, 50, 51, 46, 53]).

test(atomwards_codes_n3,true(A == '3r5')) :-
   atom_codes(A,[51, 114, 53]).

% ---
% It's not not quite relational: "abc" and 'abc' are deterministically
% related to the same codelist ... but "abc" and 'abc' are not the same!
% (What's missing is the hidden degree of freedom that is the direction of
%  information flow.)
% ---

test("not quite relational") :- 
   String = "abc", 
   atom_codes(String,Cs), 
   atom_codes(Atom,Cs), 
   Atom = 'abc', 
   Atom \= String.

:- end_tests(atom_codes).

