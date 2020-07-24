% ===
% Perform certain type conversion in an easy-on-the-programmer's mind matter
% ===

% ---
% convert_to_string(+In,?Out)
% ---

:- consult([conversion]).

% Shoudl not consult here, but use user should use load_test_files([])
% to load .plt file associated to the file "conversion.pl" instead.
% Doesn't work though.

:- begin_tests(string_conversion).

test("convert string to string", true(Str = "duh")) :-
   convert_to_string("duh",Str).

test("convert atom to string", true(Str == "foo")) :-
   convert_to_string(foo,Str).

% But what floating-point printing style is really used???
test("convert float to string", true(Str == "1.2345")) :- 
   convert_to_string(1.2345,Str).

test("convert int to string", true(Str == "12345")) :-
   convert_to_string(12345,Str).

test("convert non-integer rational to string", true(Str == "1r3")) :-
   convert_to_string(1r3,Str).

test("convert list of characters to string", true(Str == "foo")) :-
   convert_to_string([f,o,o],Str).

test("convert list of integers to string", true(Str == "foo")) :-
   convert_to_string([0x66,0x6F,0x6F],Str).

% ---
% verifying
% ---

test("verify string to string") :-
   convert_to_string("duh","duh").

test("verify atom to string") :-
   convert_to_string(foo,"foo").

% ---
% failures
% ---

test("fail verifying string to string",fail) :-
   convert_to_string("foo","oof").

test("fail verifying atom to string",fail) :-
   convert_to_string(foo,"oof").

% If Prolog were more hardcore, this would work with a type constraint on _In & _Out
test("fail converting freshvar to string",error(instantiation_error)) :- 
   convert_to_string(_In,_Out).
 
test("fail converting list of stuff to string",error(type_error(_,_))) :- 
   convert_to_string([aaa,bbb],_).

test("fail converting dict to string",error(type_error(_,_))) :- 
   convert_to_string(foo{},_).

:- end_tests(string_conversion).

% ---
% convert_to_atom(+In,?Out)
% ---

:- begin_tests(atom_conversion).

test("convert atom to atom", true(A = duh)) :-
   convert_to_atom(duh,A).

test("convert string to atom", true(A == foo)) :-
   convert_to_atom("foo",A).

% But what floating-point printing style is really used???
test("convert float to atom", true(A == '1.2345')) :- 
   convert_to_atom(1.2345,A).

test("convert int to atom", true(A == '12345')) :-
   convert_to_atom(12345,A).

test("convert non-integer rational to atom", true(A == '1r3')) :-
   convert_to_atom(1r3,A).

test("convert list of characters to atom", true(A == foo)) :-
   convert_to_atom([f,o,o],A).

test("convert list of integers to atom", true(A == foo)) :-
   convert_to_atom([0x66,0x6F,0x6F],A).

% ---
% verifying
% ---

test("verify atom to atom") :-
   convert_to_atom(duh,duh).

test("verify string to atom") :-
   convert_to_atom("foo",foo).

% ---
% failures
% ---

test("fail verifying string to atom",fail) :-
   convert_to_atom("foo",oof).

test("fail verifying atom to atom",fail) :-
   convert_to_atom(foo,oof).

% If Prolog were more hardcore, this would work with a type constraint on _In & _Out
test("fail converting freshvar to atom",error(instantiation_error)) :- 
   convert_to_atom(_In,_Out).
 
test("fail converting list of stuff to atom",error(type_error(_,_))) :- 
   convert_to_atom([aaa,bbb],_).

test("fail converting dict to atom",error(type_error(_,_))) :- 
   convert_to_atom(foo{},_).

:- end_tests(atom_conversion).



