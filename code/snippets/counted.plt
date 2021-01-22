:- use_module(library('snippets/counted.pl')).

:- begin_tests(counted).

test("count empty list",true(Result == counts{})) :-
   counted([],Result,counts).

test("#1",true(Result == foo{a:4,b:2,c:3,d:1,e:1,f:1})) :-
   counted([a,b,c,d,c,e,b,a,a,f,a,c],Result,foo).

test("#2",true(Result == foo{' ':1,!:1,',':1,'H':1,'W':1,d:1,e:1,l:3,o:2,r:1})) :-
   atom_chars("Hello, World!",Chars),
   counted(Chars,Result,foo).

:- end_tests(counted).
 
