% Posted to the comment section at https://eu.swi-prolog.org/pldoc/doc_for?object=atom_chars/2

:- begin_tests(atom_chars).

test(listwards_atom        ,[true(T)]) :- atom_chars(abc  ,Cs)  , T = (Cs == [a, b, c]).
test(listwards_char        ,[true(T)]) :- atom_chars(a    ,Cs)  , T = (Cs == [a]).
test(listwards_string      ,[true(T)]) :- atom_chars("abc",Cs)  , T = (Cs == [a, b, c]).
test(listwards_integer     ,[true(T)]) :- atom_chars(123  ,Cs)  , T = (Cs == ['1', '2', '3']).
test(listwards_float       ,[true(T)]) :- atom_chars(123.5,Cs)  , T = (Cs == ['1', '2', '3', '.', '5']).
test(listwards_rational    ,[true(T)]) :- atom_chars(3r5  ,Cs)  , T = (Cs == ['3', 'r', '5']).
test(listwards_empty_atom  ,[true(T)]) :- atom_chars(''   ,Cs)  , T = (Cs == []).
test(listwards_empty_string,[true(T)]) :- atom_chars(""   ,Cs)  , T = (Cs == []).
test(listwards_katakana    ,[true(T)]) :- atom_chars('アイウ',Cs), T = (Cs == ['ア', 'イ', 'ウ']).

test(listwards_compound ,[error(type_error(atom,x(1)))])          :- atom_chars(x(1),_).
test(atomwards_compound ,[error(type_error(list,x(1)))])          :- atom_chars(_,x(1)).
test(nowards            ,[error(instantiation_error)])            :- atom_chars(_,_).
test(atomwards_strings  ,[error(type_error(character_code,"a"))]) :- atom_chars(_,["a"]).

test(atomwards_codes   ,[true(T)]) :- atom_chars(A,[97, 98, 99]),         T = (A == abc).
test(atomwards_chars   ,[true(T)]) :- atom_chars(A,[a,  b,  c]),          T = (A == abc).
test(atomwards_code    ,[true(T)]) :- atom_chars(A,[97]),                 T = (A == a).
test(atomwards_char    ,[true(T)]) :- atom_chars(A,[a]),                  T = (A == a).
test(atomwards_empty   ,[true(T)]) :- atom_chars(A,[]),                   T = (A == '').
test(atomwards_codes_n1,[true(T)]) :- atom_chars(A,[49, 50, 51]),         T = (A == '123').
test(atomwards_codes_n2,[true(T)]) :- atom_chars(A,[49, 50, 51, 46, 53]), T = (A == '123.5').
test(atomwards_codes_n3,[true(T)]) :- atom_chars(A,[51, 114, 53]),        T = (A == '3r5').

% It's not not quite relational: "abc" and 'abc' are deterministically
% related to the same codelist, but "abc" and 'abc' are not the same!
% (What's missing is the hidden degree of freedom that is the direction of
%  information flow.)

test(not_quite_relational) :- String = "abc", atom_chars(String,Cs), atom_chars(Atom,Cs), Atom = 'abc', Atom \= String.

:- end_tests(atom_chars).

rt(atom_chars) :- run_tests(atom_chars).
