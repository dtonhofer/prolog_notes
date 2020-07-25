:- use_module(library('heavycarbon/strings/string_of_spaces.pl')).

:- begin_tests(string_of_spaces).

test("generate a short string", true(Str == "     ")) :- 
   string_of_spaces(5,Str).

test("generate a long string", true(Str == "               ")) :-
   string_of_spaces(15,Str).

test("verify a string") :-
   string_of_spaces(5,"     ").

test("length of string",true(L == 5)) :-
   string_of_spaces(L,"     ").

test("generate pairs", true(Bag == [0-"",1-" ",2-"  ",3-"   ",4-"    "])) :-
   length(Bag,5),
   bagof(L-S,limit(5,string_of_spaces(L,S)),Bag).

test("generate multiple strings") :-
   forall(between(0,2000,Len),
          (string_of_spaces(Len,Spaces),
           string_length(Spaces,Len),
           debug(test_string_of_spaces,"Ok for length ~d\n",[Len]))).

test("fail verifiying a non-space string",fail) :-
   string_of_spaces(5,"12345").

test("fail verifying length of string",fail) :-
   string_of_spaces(50,"     ").

test("fail obtaining length of non-space string",fail) :-
   string_of_spaces(_,"XXX").

:- end_tests(string_of_spaces).




