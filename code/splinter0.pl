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
% This is done by judicious application of append/2.
%
% splinter0(+List, +N, ?Front, ?Element, ?Back)
% splinter0(?List, ?N, +Front, +Element, +Back)
% ===

% ===
% TODO: Test more modes!!!
%
% Consider the following possibilites:
% 1 = nonvar at that argument place
% 0 = var at at that argument place
% . = don't care
%
% Deterministic, anything additionally set allows consistency checks (conchecks)
%  
% [1 1 . . .] - List, N     --> can compute/concheck Front, Element, Back ("work forwards")
% [1 . 1 . .] - List, Front --> can compute/concheck N via length/2, then work forward
% [1 . . . 1] - List, Back  --> can compute N, and thus the rest
% [. . 1 . 1] - Front, Back --> can compute N, generate List as fresh variables ("work backwards")
%
% Patch up with fresh variables:
%
% [. 1 . . 1] - N,Back      --> can generate fresh Front and List
%
% Nondeterministic:
%
% [1 . . . .] - List        --> generate all N possibilities to splinter the list
%
% List of arbitrary length implied (all the cases that have not been caught above)
% In this case, throwing an error seems appropriate.
%
% [0 . . . 0] - Not enough info to limit length of List
% [0 . 0 . 1] - Not enough info to limit length of list
% ==

splinter0(List, N, Front, Element, Back) :-
    ground_flags([List, N, Front, Element, Back], GroundFlags),
    splinter0_dispatch(GroundFlags, List, N, Front, Element, Back).

% ---
% Handle cases depending on what variables are ground/nonground
% ---

splinter0_dispatch([1,1|_], List, N, Front, Element, Back) :-
   !,
   splinter0_forward(List, N, Front, Element, Back).
 
splinter0_dispatch([1,_,1|_], List, N, Front, Element, Back) :-
   !,
   splinter0_forward(List, N, Front, Element, Back).
 
splinter0_dispatch([1,_,_,_,1], List, N, Front, Element, Back) :-
   !,
   length(Back,B),
   length(List,L),
   N is L-B-1,
   splinter0_forward(List,N,Front,Element,Back).

splinter0_dispatch([_,_,1,_,1], List, N, Front, Element, Back) :-
   !,
   length(Back,B),
   length(Front,F),
   L is B+F+1,
   length(List,L), % generate fresh variables if var(List)
   N is F,   
   splinter0_forward(List,N,Front,Element,Back).
 
splinter0_dispatch([_,1,_,_,1], List, N, Front, Element, Back) :-
   !,
   length(Back,B),
   F is N,
   L is F+B+1,
   length(List,L), % generate fresh variables if var(List)
   splinter0_forward(List,N,Front,Element,Back).
 
splinter0_dispatch([1|_], List, N, Front, Element, Back) :-
   !,
   length(List,L),   % known
   length(Front,N),  % generate
   succ(N,L),        % test on length
   append(Front,[Element|Back],List).

splinter0_dispatch(GFs,_,_,_,_,_) :-
   with_output_to(string(Buffer),format("Not enough info to limit the length of the list! ground_flags = ~q",[GFs])),
   throw(error(Buffer)).

% ---
% Workhorse
% ---

splinter0_forward(List, N, Front, Element, Back) :-
   length(Front,N),
   append(Front,[Element|Back],List).

% ---
% Helper: compute "ground flags"
% ---

ground_flags([A|As],[1|Flags]) :- nonvar(A),!,ground_flags(As,Flags).
ground_flags([A|As],[0|Flags]) :- var(A),!,ground_flags(As,Flags).
ground_flags([],[]).

% ===
% Unit tests
% ===

% TODO: Add test cases for the other possibilites of var/nonvar vectors

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

test(list_and_N_known) :-
   longlist(List),
   length(List,Len),
   succ(Nmax,Len),
   foreach(between(0,Nmax,N),fwd_list_and_N_known(List,N)).

fwd_list_and_N_known(List,N) :-
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

% 
:- end_tests(splinter0).

rt :- run_tests(splinter0).

