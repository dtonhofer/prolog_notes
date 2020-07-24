:- use_module(library(leveling)).

:- begin_tests(leveling).

test("Second argument may be fresh, and necessarily takes up string", true(Out == "hello")) :-
   leveling_string("hello",Out).

test("Second argument may be fresh, and takes up stringified int", true(Out == "123")) :-
   leveling_string(123,Out).

test("Second argument may be fresh, and takes up stringified atom", true(Out == "foo")) :-
   leveling_string(foo,Out).

test("Second argument can be stated as string") :- 
   leveling_string("hello","hello").

test("Second argument can be stringified, must match first string argument: case of atom") :-
   leveling_string("hello",hello).

test("Second argument can be stringified, must match first string argument: case of integer") :-
   leveling_string("123",123).

test("Second argument can be stringified, must match: case of failure",fail) :-
   leveling_string("hello",123).

:- end_tests(leveling).
