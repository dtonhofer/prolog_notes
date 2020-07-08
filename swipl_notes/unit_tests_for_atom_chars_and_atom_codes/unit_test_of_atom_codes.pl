% Posted to the comment section at https://eu.swi-prolog.org/pldoc/doc_for?object=atom_codes/2

:- begin_tests(atom_codes).

test(listwards_atom        ,[true(T)]) :- atom_codes(abc  ,Cs)  , T = (Cs == [97, 98, 99]).
test(listwards_char        ,[true(T)]) :- atom_codes(a    ,Cs)  , T = (Cs == [97]).
test(listwards_string      ,[true(T)]) :- atom_codes("abc",Cs)  , T = (Cs == [97, 98, 99]).
test(listwards_integer     ,[true(T)]) :- atom_codes(123  ,Cs)  , T = (Cs == [49, 50, 51]).
test(listwards_float       ,[true(T)]) :- atom_codes(123.5,Cs)  , T = (Cs == [49, 50, 51, 46, 53]).
test(listwards_rational    ,[true(T)]) :- atom_codes(3r5  ,Cs)  , T = (Cs == [51, 114, 53]).
test(listwards_empty_atom  ,[true(T)]) :- atom_codes(''   ,Cs)  , T = (Cs == []).
test(listwards_empty_string,[true(T)]) :- atom_codes(""   ,Cs)  , T = (Cs == []).
test(listwards_katakana    ,[true(T)]) :- atom_codes('アイウ',Cs), T = (Cs == [12450, 12452, 12454]).

test(listwards_compound ,[error(type_error(atom,x(1)))])          :- atom_codes(x(1),_).
test(atomwards_compound ,[error(type_error(list,x(1)))])          :- atom_codes(_,x(1)).
test(nowards            ,[error(instantiation_error)])            :- atom_codes(_,_).
test(atomwards_strings  ,[error(type_error(character_code,"a"))]) :- atom_codes(_,["a"]).

test(atomwards_codes   ,[true(T)]) :- atom_codes(A,[97, 98, 99]),         T = (A == abc).
test(atomwards_chars   ,[true(T)]) :- atom_codes(A,[a,  b,  c]),          T = (A == abc).
test(atomwards_code    ,[true(T)]) :- atom_codes(A,[97]),                 T = (A == a).
test(atomwards_char    ,[true(T)]) :- atom_codes(A,[a]),                  T = (A == a).
test(atomwards_empty   ,[true(T)]) :- atom_codes(A,[]),                   T = (A == '').
test(atomwards_codes_n1,[true(T)]) :- atom_codes(A,[49, 50, 51]),         T = (A == '123').
test(atomwards_codes_n2,[true(T)]) :- atom_codes(A,[49, 50, 51, 46, 53]), T = (A == '123.5').
test(atomwards_codes_n3,[true(T)]) :- atom_codes(A,[51, 114, 53]),        T = (A == '3r5').

% It's not not quite relational: "abc" and 'abc' are deterministically
% related to the same codelist, but "abc" and 'abc' are not the same!
% (What's missing is the hidden degree of freedom that is the direction of
%  information flow.)

test(not_quite_relational) :- String = "abc", atom_codes(String,Cs), atom_codes(Atom,Cs), Atom = 'abc', Atom \= String.

:- end_tests(atom_codes).

rt(atom_codes) :- run_tests(atom_codes).
