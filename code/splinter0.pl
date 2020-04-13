% ===
% Mon 13 Apr 11:08:46 CEST 2020
% ---
% This is free and unencumbered software released into the public domain.
% 
% Anyone is free to copy, modify, publish, use, compile, sell, or
% distribute this software, either in source code form or as a compiled
% binary, for any purpose, commercial or non-commercial, and by any
% means.
% 
% For more information, please refer to <http://unlicense.org/>
% ===

% ===
% "Splinter, 0-based"
% ===
% Given a List and an index N into the list (N from 0 to N-1),
% splinter a list at position N into:
%
% - a front-list
% - the element at position N (which is why the index must be 0..N-1)
% - the back-list
%
% for example:
%
% [a,b,c,d,e,f,g,h] N=0 ==> [] a [b,c,d,e,f,g,h]
% [a,b,c,d,e,f,g,h] N=1 ==> [a] b [c,d,e,f,g,h]
% [a,b,c,d,e,f,g,h] N=2 ==> [a,b] c [d,e,f,g,h]
% [a,b,c,d,e,f,g,h] N=3 ==> [a,b,c] d [e,f,g,h]
% [a,b,c,d,e,f,g,h] N=4 ==> [a,b,c,d] e [f,g,h]
% [a,b,c,d,e,f,g,h] N=5 ==> [a,b,c,d,e] f [g,h]
% [a,b,c,d,e,f,g,h] N=6 ==> [a,b,c,d,e,f] g [h]
% [a,b,c,d,e,f,g,h] N=7 ==> [a,b,c,d,e,f,g] h []
%
% This is done judicious application of append/2.
%
% splinter0(+List, +N, ?Front, ?Element, ?Back)
% splinter0(?List, ?N, +Front, +Element, +Back)
% ===


splinter0(List, N, Front, Element, Back) :-
    length(Front, N),
    append(Front, [Element|Back], List).


% ===
% Unit tests
% ===

:- begin_tests(splinter0).

% negative index

test(oob,[throws(error(_,_))]) :- splinter0([a,b],-1,_,_,_).

% out of bounds on list

test(oob0,[fail]) :- splinter0([],0,_,_,_).
test(oob1,[fail]) :- splinter0([],1,_,_,_).
test(oob2,[fail]) :- splinter0([a,b],2,_,_,_).

% "global variable" llonglist

longlist([a,b,c,d,e,f,g,h]).

% working forwards: generating "front,element,back" from "list,N"

test(forwards) :-
   longlist(List),
   length(List,Len),
   succ(Nmax,Len),
   foreach(between(0,Nmax,N),fwd(List,N)).

fwd(List,N) :-
   splinter0(List,N,Fl,Elem,Bl),
   format("~w N=~w ==> ~w ~w ~w\n",[List,N,Fl,Elem,Bl]),
   % test consist in ... checking the spec!
   append([Fl,[Elem],Bl],List), % format("append ok!\n"),
   length(Fl,N).                % format("length ok!\n"). 

% working backwards: recomposing "list,N" from "front,element,back"

test(backwards) :-
   longlist(List),
   length(List,Len),
   succ(Nmax,Len),
   foreach(between(0,Nmax,N),bwd(List,N)).

bwd(List,N) :-
   splinter0(List,N,Fl,Elem,Bl),              % splinter a known list (work forward, assumed correct)
   splinter0(RecontList,RecontN,Fl,Elem,Bl),  % reconstitute List and N from the result of splintering
   format("~w ~w ~w ==> ~w N=~w\n",[Fl,Elem,Bl,RecontList,RecontN]),
   % test consist in comparing!
   RecontList = List, % format("list comparison ok!\n"),
   RecontN = N.       % format("index comparison ok!\n").         

:- end_tests(splinter0).

rt :- run_tests(splinter0).

