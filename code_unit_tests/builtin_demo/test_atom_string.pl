% https://eu.swi-prolog.org/pldoc/doc_for?object=atom_string/2
%
% atom_string(?Atom, ?String)
%
% Note that atom_string/2 does not really behave "predicate-ly" but
% more like a two-sided pipeline.
%
% Send in anything "stringy" ------>+      +---------> String equivalent of the input 
%                                   |      |
%                    atom_string(?Atom, ?String)
%
% Atom equivalent of the input <----+      +<------ Send in anything "stringy" 
%                                   |      |
%                    atom_string(?Atom, ?String)
%
% Send in anything "stringy" ------>+      +<------ Send in anything "stringy" 
%                                   |      |
%                    atom_string(?Atom, ?String)
%                                   |      |
%                         "true if stringily equivalent"
%
% Maybe these predicates which accepts several representations of the "same thing"
% should be thought of as operating on equivalence classes but returning a preferred
% representative of the equivalence class depending on the argument, whereas
% verification can work with any representative of the equivalence class!

:- begin_tests(atom_string_transform).

test("Output on pos 2 is string (input on pos 1 is atom)"  ,[true(S == "foo")]) :- atom_string( foo ,S).
test("Output on pos 2 is string (input on pos 1 is string)",[true(S == "foo")]) :- atom_string("foo",S).
test("Output on pos 1 is atom (input on pos 2 is string)"  ,[true(A == foo)])   :- atom_string(A,"foo").
test("Output on pos 1 is atom (input on pos 2 is atom)"    ,[true(A == foo)])   :- atom_string(A,'foo').

:- end_tests(atom_string_transform).

:- begin_tests(atom_string_verify).

test("Verification with atom on pos 1, string on pos 2")   :- atom_string( foo  , "foo").
test("Verification with atom on pos 1, atom on pos 2")     :- atom_string( foo  ,  foo).
test("Verification with string on pos 1, string on pos 2") :- atom_string("foo" , "foo").
test("Verification with string on pos 1, atom on pos 2")   :- atom_string("foo" ,  foo).

:- end_tests(atom_string_verify).

