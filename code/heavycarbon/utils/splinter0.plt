:- use_module(library('heavycarbon/utils/splinter0.pl')).

% ============================================================================
% Unit tests for splinter0
% ============================================================================

:- begin_tests(splinter0).

% ---
% "global variable" longlist
% ---

longlist([a,b,c,d,e,f,g,h]).

% ---
% Testing error conditions
% ---

% Negative index throws.
% Positive but out of bounds index causes failure.
% Arguments too unconstrained throws.

test(negative_index,[throws(error(_,_))]) :- splinter0([a,b],-1,_,_,_).
test(empty_list_oob1,[fail])              :- splinter0([],0,_,_,_).
test(empty_list_oob2,[fail])              :- splinter0([],1,_,_,_).
test(nonempty_list_oob,[fail])            :- splinter0([a,b],2,_,_,_).
test(too_much_freedom,[throws(_)])        :- splinter0(_List,_N,_Prefix,_Element,_Suffix).

% ---
% "List and N known"
% ---

test(list_and_N_known) :-
   longlist(List),
   length(List,ListLen),
   succ(N_max,ListLen),
   foreach(between(0,N_max,N),list_and_N_known(List,N)).

list_and_N_known(List,N) :-
   splinter0(List,N,Prefix_out,Element_out,Suffix_out),
   debug(splinter_test,"List=~w N=~w ==> ~w ~w ~w\n",[List,N,Prefix_out,Element_out,Suffix_out]),
   % test consists in checking the spec!
   append([Prefix_out,[Element_out],Suffix_out],List),
   length(Prefix_out,N).

% ---
% "List and prefix known"
% ---

test(list_and_prefix_known) :-
   longlist(List),
   Prefix=[a,b,c,d],
   splinter0(List,N,Prefix,Element,Suffix),
   append([Prefix,[Element],Suffix],List),
   length(Prefix,N).

% ---
% "List and suffix known"
% ---

test(list_and_suffix_known) :-
   longlist(List),
   length(List,ListLen),
   succ(SuffixLen_max,ListLen),
   foreach(between(0,SuffixLen_max,SuffixLen),list_and_suffix_known(List,SuffixLen)).

list_and_suffix_known(List,SuffixLen) :-
   extract_a_suffix(List,SuffixLen,Suffix),
   splinter0(List,N_out,Prefix_out,Element_out,Suffix),
   debug(splinter_test,"List=~w Suffix=~w ==> ~w ~w ~w\n",[List,Suffix,Prefix_out,Element_out,N_out]),
   % test consists in checking the spec!
   append([Prefix_out,[Element_out],Suffix],List),
   length(Prefix_out,N_out).

extract_a_suffix(List,SuffixLen,Suffix_out) :-
   length(Suffix_out,SuffixLen),
   append([_,Suffix_out],List).

% ---
% "prefix and suffix known"
% ---

test(prefix_and_suffix_known) :-
   longlist(List),
   length(List,ListLen),
   succ(N_max,ListLen),
   foreach(between(0,N_max,N),prefix_and_suffix_known(List,N)).

prefix_and_suffix_known(List,N) :-
   splinter0(List,N,Prefix_mid,Element_mid,Suffix_mid), % splinter a known list, assumed to work correctly
   splinter0(List_rebuild,N_rebuild,Prefix_mid,Element_mid,Suffix_mid),
   debug(splinter_test,"Prefix=~w Element=~w Suffix=~w ==> ~w N=~w\n",[Prefix_mid,Element_mid,Suffix_mid,List_rebuild,N_rebuild]),
   % test consist in comparing!
   List = List_rebuild,
   N = N_rebuild.

% ---
% "suffix and N known"
% ---

test(suffix_and_N_known) :-
   splinter0(L,3,FL,E,[f,g,h]),
   E  = e,
   FL = [x,y,z],
   L  = [x,y,z,e,f,g,h].

% ---
% only list known
% ---

test(only_list_known) :-
   bagof([N,Prefix,Element,Suffix],splinter0([a,b,c,d],N,Prefix,Element,Suffix),Bag),
   Bag = [[0, [], a, [b, c, d]],
          [1, [a], b, [c, d]],
          [2, [a, b], c, [d]],
          [3, [a, b, c], d, []]].

:- end_tests(splinter0).

% ============================================================================
% Unit tests for replace0
% ============================================================================

:- begin_tests(replace0).

% ---
% normal replace
% ---

test(replace0_0) :- replace0([a,b,c],0,rep(A),A,NewList), A=a, NewList = [rep(a), b, c].
test(replace0_1) :- replace0([a,b,c],1,rep(B),B,NewList), B=b, NewList = [a, rep(b), c].
test(replace0_2) :- replace0([a,b,c],2,rep(C),C,NewList), C=c, NewList = [a, b, rep(c)].

% ---
% testing error conditions
% ---

% Negative index throws.
% Positive but out of bounds index causes failure.

test(replace0_fail_0,[fail])               :- replace0([a,b,c],3,_,_,_).
test(replace0_fail_1,[fail])               :- replace0([],0,_,_,_).
test(replace0_fail_2,[throws(error(_,_))]) :- replace0([a,b,c],-1,_,_,_).

:- end_tests(replace0).


