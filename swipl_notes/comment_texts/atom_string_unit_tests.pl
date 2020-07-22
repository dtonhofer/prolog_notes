% https://eu.swi-prolog.org/pldoc/doc_for?object=atom_string/2

:- begin_tests(atom_string_transforming).

   test("Output on pos 2 is string (input on pos 1 is atom)"  ,[true(S == "foo")]) :- atom_string( foo ,S).
   test("Output on pos 2 is string (input on pos 1 is string)",[true(S == "foo")]) :- atom_string("foo",S).

   test("Output on pos 1 is atom (input on pos 2 is string)"  ,[true(A == foo)])   :- atom_string(A,"foo").
   test("Output on pos 1 is atom (input on pos 2 is atom)"    ,[true(A == foo)])   :- atom_string(A,'foo').

:- end_tests(atom_string_transforming).

:- begin_tests(atom_string_verification).

   test("Verification with atom on pos 1, string on pos 2")   :- atom_string( foo  , "foo").
   test("Verification with atom on pos 1, atom on pos 2")     :- atom_string( foo  ,  foo).
   test("Verification with string on pos 1, string on pos 2") :- atom_string("foo" , "foo").
   test("Verification with string on pos 1, atom on pos 2")   :- atom_string("foo" ,  foo).

:- end_tests(atom_string_verification).

