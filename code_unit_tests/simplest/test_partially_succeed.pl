:- begin_tests(partially_succeed).

   test("just succeed") :- true.
   test("just fail")    :- fail.

:- end_tests(partially_succeed).

