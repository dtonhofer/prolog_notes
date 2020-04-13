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

splinter0(List, N, FrontList, Element, BackList) :-
    ground_flags([List, N, FrontList, Element, BackList], GroundFlags),
    splinter0_dispatch(GroundFlags, List, N, FrontList, Element, BackList).

% ---
% Handle cases depending on what variables are ground/nonground
% ---

splinter0_dispatch([1,1|_], List, N, FrontList, Element, BackList) :-
   !,
   splinter0_forward(List,N,FrontList,Element,BackList).
 
splinter0_dispatch([1,_,1|_], List, N, FrontList, Element, BackList) :-
   !,
   splinter0_forward(List,N,FrontList,Element,BackList).
 
splinter0_dispatch([1,_,_,_,1], List, N, FrontList, Element, BackList) :-
   !,
   length(BackList,B),
   length(List,L),
   N is L-B-1,
   splinter0_forward(List,N,FrontList,Element,BackList).

splinter0_dispatch([_,_,1,_,1], List, N, FrontList, Element, BackList) :-
   !,
   length(BackList,B),
   length(FrontList,F),
   L is B+F+1,
   length(List,L), % generate fresh variables if var(List)
   N is F,   
   splinter0_forward(List,N,FrontList,Element,BackList).
 
splinter0_dispatch([_,1,_,_,1], List, N, FrontList, Element, BackList) :-
   !,
   length(BackList,B),
   F is N,
   L is F+B+1,
   length(List,L), % generate fresh variables if var(List)
   splinter0_forward(List,N,FrontList,Element,BackList).
 
splinter0_dispatch([1|_], List, N, FrontList, Element, BackList) :-
   !,
   length(List,L),     % known list, known length
   succ(N_max,L),
   between(0,N_max,N), % generate Ns because "length(Front,N)" alone won't stop
   length(FrontList,N),    % know length, known list of fresh variables
   append([FrontList,[Element],BackList],List).

splinter0_dispatch(GFs,_,_,_,_,_) :-
   with_output_to(string(Buffer),format("Not enough info to limit the length of the list! ground_flags = ~q",[GFs])),
   throw(error(Buffer)).

% ---
% The Workhorse. There are only two lines!
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

longlist([a,b,c,d,e,f,g,h]). % "global variable" longlist

test(negative_index,[throws(error(_,_))]) :- splinter0([a,b],-1,_,_,_).
test(empty_list_1,[fail]) :- splinter0([],0,_,_,_).
test(empty_list_2,[fail]) :- splinter0([],1,_,_,_).
test(out_of_bounds,[fail]) :- splinter0([a,b],2,_,_,_).
test(too_much_freedom,[throws(_)]) :- splinter0(_List,_N,_FrontList,_Element,_BackList).

% ---

test(list_and_N_known) :-
   longlist(List),
   length(List,ListLen),
   succ(N_max,ListLen),
   foreach(between(0,N_max,N),list_and_N_known(List,N)).

list_and_N_known(List,N) :-
   splinter0(List,N,FrontList_out,Element_out,BackList_out),
   format("List=~w N=~w ==> ~w ~w ~w\n",[List,N,FrontList_out,Element_out,BackList_out]),
   % test consists in checking the spec!
   append([FrontList_out,[Element_out],BackList_out],List),
   length(FrontList_out,N).

% ---

test(list_and_frontlist_known) :- 
   longlist(List),
   FrontList=[a,b,c,d],
   splinter0(List,N,FrontList,Element,BackList),
   append([FrontList,[Element],BackList],List),
   length(FrontList,N).

% ---

test(list_and_backlist_known) :-
   longlist(List),
   length(List,ListLen),
   succ(BackListLen_max,ListLen),
   foreach(between(0,BackListLen_max,BackListLen),list_and_backlist_known(List,BackListLen)).

list_and_backlist_known(List,BackListLen) :-
   extract_a_back_list(List,BackListLen,BackList),
   splinter0(List,N_out,FrontList_out,Element_out,BackList),
   format("List=~w BackList=~w ==> ~w ~w ~w\n",[List,BackList,FrontList_out,Element_out,N_out]),
   % test consists in checking the spec!
   append([FrontList_out,[Element_out],BackList],List),
   length(FrontList_out,N_out).

extract_a_back_list(List,BackListLen,BackList_out) :-
   length(BackList_out,BackListLen),
   append([_,BackList_out],List).

% ---

test(frontlist_and_backlist_known) :-
   longlist(List),
   length(List,ListLen),
   succ(N_max,ListLen),
   foreach(between(0,N_max,N),frontlist_and_backlist_known(List,N)).

frontlist_and_backlist_known(List,N) :-
   splinter0(List,N,FrontList_mid,Element_mid,BackList_mid), % splinter a known list, assumed to work correctly
   splinter0(List_rebuild,N_rebuild,FrontList_mid,Element_mid,BackList_mid), 
   format("FrontList=~w Element=~w BackList=~w ==> ~w N=~w\n",[FrontList_mid,Element_mid,BackList_mid,List_rebuild,N_rebuild]),
   % test consist in comparing!
   List = List_rebuild,
   N = N_rebuild.

% ---

test(backlist_and_N_known) :-
   splinter0(L,3,FL,E,[f,g,h]),
   E  = e,
   FL = [x,y,z],
   L  = [x,y,z,e,f,g,h].

% ---

test(only_list_known) :-
   bagof([N,FrontList,Element,BackList],splinter0([a,b,c,d],N,FrontList,Element,BackList),Bag),
   Bag = [[0, [], a, [b, c, d]], 
          [1, [a], b, [c, d]], 
          [2, [a, b], c, [d]], 
          [3, [a, b, c], d, []]].

:- end_tests(splinter0).

rt :- run_tests(splinter0).
