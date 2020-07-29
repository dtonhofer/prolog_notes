% Some test code for atom_chars/2
% https://www.swi-prolog.org/pldoc/doc_for?object=atom_chars/2

:- begin_tests(atom_chars).

test("listwards atom",true(Cs == [a, b, c])) :-
   atom_chars(abc,Cs).

test("listwards char",true(Cs == [a])) :-
   atom_chars(a,Cs).

test("listwards string",true(Cs == [a, b, c])) :-
   atom_chars("abc",Cs).

test("listwards integer",true(Cs == ['1', '2', '3'])) :-
   atom_chars(123,Cs).

test("listwards float",true(Cs == ['1', '2', '3', '.', '5'])) :-
   atom_chars(123.5,Cs).

test("listwards rational",true(Cs == ['3', 'r', '5'])) :-
   atom_chars(3r5,Cs).

test("listwards empty_atom",true(Cs == [])) :-
   atom_chars('',Cs).

test("listwards empty_string",true(Cs == [])) :-
   atom_chars("",Cs).

test("listwards katakana",true(Cs == ['ア', 'イ', 'ウ'])) :-
   atom_chars('アイウ',Cs).

test("listwards compound throws",error(type_error(atom,x(1)))) :-
   atom_chars(x(1),_).

test("atomwards compound throws",error(type_error(list,x(1)))) :-
   atom_chars(_,x(1)).

test("nowards throws",error(instantiation_error)) :-
   atom_chars(_,_).

test("atomwards strings throws",error(type_error(character_code,"a"))) :-
   atom_chars(_,["a"]).

test("atomwards codes",true(A == abc)) :-
   atom_chars(A,[97, 98, 99]).

test("atomwards chars",true(A == abc)) :-
   atom_chars(A,[a,  b,  c]).

test("atomwards unique code",true(A == a)) :-
   atom_chars(A,[97]).

test("atomwards unique char",true(A == a)) :-
   atom_chars(A,[a]).

test("atomwards empty list",true(A == '')) :-
   atom_chars(A,[]).

test("atomwards codes forming integer",true(A == '123')) :-
   atom_chars(A,[49, 50, 51]).

test("atomwards codes forming float",true(A == '123.5')) :-
   atom_chars(A,[49, 50, 51, 46, 53]).

test("atomwards codes forming non-integer rational",true(A == '3r5')) :-
   atom_chars(A,[51, 114, 53])).

% It's not not quite relational: "abc" and 'abc' are deterministically
% related to the same codelist, but "abc" and 'abc' are not the same!
% (What's missing is the hidden degree of freedom that is the direction of
%  information flow.)

test("not quite relational") :- 
   String = "abc", 
   atom_chars(String,Cs), 
   atom_chars(Atom,Cs), 
   Atom = 'abc', 
   Atom \= String.

:- end_tests(atom_chars).


