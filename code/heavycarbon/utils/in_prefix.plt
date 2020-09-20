:- use_module(library('heavycarbon/utils/in_prefix.pl')).

:- begin_tests(in_prefix).

test(one) :-
   in_prefix(X,[X,1,2]).

test(two,[fail]) :-
   in_prefix(_X,[1,2|_U]).

test(three,[fail]) :-
   in_prefix(_X,[1,2,_U]).

test(four) :-
   in_prefix(1,[1,_X,_U]).

test(five,[fail]) :-
   in_prefix(3,[1,2,_U]).

test(six,[fail]) :-
   in_prefix(3,[]).

test(seven,[fail]) :-
   in_prefix(3,foo).

test(eight,[fail]) :-
   in_prefix(3,_).

test(nine,[fail]) :-
   in_prefix_with_stoplist(x,[x,y],[y,x]).

test(ten) :-
   in_prefix_with_stoplist(x,[x,y],[x,y]).

test(eleven,[fail]) :-
   in_prefix_with_stoplist(X,[X,Y],[a,b,c,Y,X]).

test(twelve) :-
   in_prefix_with_stoplist(X,[X,Y],[a,b,c,X,Y]).

test(thirteen) :-
   in_prefix_with_stoplist(X,[Y],[a,b,c,X,Y]).

test(fourteen,[fail]) :-
   in_prefix_with_stoplist(X,[Y],[a,b,c,Y,X]).

:- end_tests(in_prefix).


