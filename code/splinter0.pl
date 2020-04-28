% 2345678901234567890123456789012345678901234567890123456789012345678901234567
% ============================================================================
% 2020-04-XX
% https://github.com/dtonhofer/prolog_notes
% ----------------------------------------------------------------------------
% This is free and unencumbered software released into the public domain.
%
% Anyone is free to copy, modify, publish, use, compile, sell, or
% distribute this software, either in source code form or as a compiled
% binary, for any purpose, commercial or non-commercial, and by any
% means.
%
% For more information, please refer to <http://unlicense.org/>
% ============================================================================

% ============================================================================
% "Splinter, 0-based"
%
% splinter0(+List, +N, ?Prefix, ?Element, ?Suffix)
% splinter0(?List, ?N, +Prefix, +Element, +Suffix)
% ============================================================================
% Given a List and an index N into the list (N from 0 to N-1),
% splinter a list at position N into:
%
% - a prefix-list
% - the element at position N (which is why the index must be 0..N-1)
% - a suffix-list
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
% Error responses:
%
% - a negative index causes a throw
% - an out-of-bounds positive or 0 index causes failure
% ============================================================================
% HOWTO
%
% Put this code into file a "splinter0.pl"
% Load on Prolog REPL: [splinter0].
% Run unit tests: ?- rt.
% ============================================================================

% ============================================================================
% Consider the following possibilites:
% 1 = nonvar at that argument place
% 0 = var at at that argument place
% . = don't care
%
% Deterministic, anything additionally set allows consistency checks (conchecks)
%
% [1 1 . . .] - List, N        --> can compute/concheck Prefix, Element, Suffix ("work forwards")
% [1 . 1 . .] - List, Prefix   --> can compute/concheck N via length/2, then work forward
% [1 . . . 1] - List, Suffix   --> can compute N, and thus the rest
% [. . 1 . 1] - Prefix, Suffix --> can compute N, generate List as fresh variables ("work backwards")
%
% Patch up with fresh variables:
%
% [. 1 . . 1] - N,Suffix       --> can generate fresh Prefix and List
%
% Nondeterministic:
%
% [1 . . . .] - List           --> generate all N possibilities to splinter the list
%
% List of arbitrary length implied (all the cases that have not been caught above)
% In this case, throwing an error seems appropriate.
%
% [0 . . . 0] - Not enough info to limit length of List
% [0 . 0 . 1] - Not enough info to limit length of list
% ============================================================================

splinter0(List, N, Prefix, Element, Suffix) :-
    ground_flags([List, N, Prefix, Element, Suffix], GroundFlags),
    splinter0_dispatch(GroundFlags, List, N, Prefix, Element, Suffix).

% ---
% Handle cases depending on what variables are ground/nonground
% ---

splinter0_dispatch([1,1|_], List, N, Prefix, Element, Suffix) :-
   !,
   splinter0_forward(List,N,Prefix,Element,Suffix).

splinter0_dispatch([1,_,1|_], List, N, Prefix, Element, Suffix) :-
   !,
   splinter0_forward(List,N,Prefix,Element,Suffix).

splinter0_dispatch([1,_,_,_,1], List, N, Prefix, Element, Suffix) :-
   !,
   length(Suffix,B),
   length(List,L),
   N is L-B-1,
   splinter0_forward(List,N,Prefix,Element,Suffix).

splinter0_dispatch([_,_,1,_,1], List, N, Prefix, Element, Suffix) :-
   !,
   length(Suffix,B),
   length(Prefix,F),
   L is B+F+1,
   length(List,L), % generate fresh variables if var(List)
   N is F,
   splinter0_forward(List,N,Prefix,Element,Suffix).

splinter0_dispatch([_,1,_,_,1], List, N, Prefix, Element, Suffix) :-
   !,
   length(Suffix,B),
   F is N,
   L is F+B+1,
   length(List,L), % generate fresh variables if var(List)
   splinter0_forward(List,N,Prefix,Element,Suffix).

splinter0_dispatch([1|_], List, N, Prefix, Element, Suffix) :-
   !,
   length(List,L),     % known list, known length
   succ(N_max,L),
   between(0,N_max,N), % generate Ns because "length(Prefix,N)" alone won't stop
   length(Prefix,N),    % know length, known list of fresh variables
   append([Prefix,[Element],Suffix],List).

splinter0_dispatch(GFs,_,_,_,_,_) :-
   with_output_to(string(Buffer),format("Not enough info to limit the length of the list! ground_flags = ~q",[GFs])),
   throw(error(Buffer)).

% ---
% The Workhorse. There are only two lines!
% ---

splinter0_forward(List, N, Prefix, Element, Suffix) :-
   length(Prefix,N),
   append(Prefix,[Element|Suffix],List).

% ---
% Helper: compute "ground flags"
% ---

ground_flags([A|As],[1|Flags]) :- nonvar(A),!,ground_flags(As,Flags).
ground_flags([A|As],[0|Flags]) :- var(A),!,ground_flags(As,Flags).
ground_flags([],[]).

% ============================================================================
% Unit tests
% ============================================================================

:- begin_tests(splinter0).

longlist([a,b,c,d,e,f,g,h]). % "global variable" longlist

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

test(list_and_N_known) :-
   longlist(List),
   length(List,ListLen),
   succ(N_max,ListLen),
   foreach(between(0,N_max,N),list_and_N_known(List,N)).

list_and_N_known(List,N) :-
   splinter0(List,N,Prefix_out,Element_out,Suffix_out),
   format("List=~w N=~w ==> ~w ~w ~w\n",[List,N,Prefix_out,Element_out,Suffix_out]),
   % test consists in checking the spec!
   append([Prefix_out,[Element_out],Suffix_out],List),
   length(Prefix_out,N).

% ---

test(list_and_prefixlist_known) :-
   longlist(List),
   Prefix=[a,b,c,d],
   splinter0(List,N,Prefix,Element,Suffix),
   append([Prefix,[Element],Suffix],List),
   length(Prefix,N).

% ---

test(list_and_suffix_known) :-
   longlist(List),
   length(List,ListLen),
   succ(SuffixLen_max,ListLen),
   foreach(between(0,SuffixLen_max,SuffixLen),list_and_suffix_known(List,SuffixLen)).

list_and_suffix_known(List,SuffixLen) :-
   extract_a_suffix(List,SuffixLen,Suffix),
   splinter0(List,N_out,Prefix_out,Element_out,Suffix),
   format("List=~w Suffix=~w ==> ~w ~w ~w\n",[List,Suffix,Prefix_out,Element_out,N_out]),
   % test consists in checking the spec!
   append([Prefix_out,[Element_out],Suffix],List),
   length(Prefix_out,N_out).

extract_a_suffix(List,SuffixLen,Suffix_out) :-
   length(Suffix_out,SuffixLen),
   append([_,Suffix_out],List).

% ---

test(prefixlist_and_suffix_known) :-
   longlist(List),
   length(List,ListLen),
   succ(N_max,ListLen),
   foreach(between(0,N_max,N),prefixlist_and_suffix_known(List,N)).

prefixlist_and_suffix_known(List,N) :-
   splinter0(List,N,Prefix_mid,Element_mid,Suffix_mid), % splinter a known list, assumed to work correctly
   splinter0(List_rebuild,N_rebuild,Prefix_mid,Element_mid,Suffix_mid),
   format("Prefix=~w Element=~w Suffix=~w ==> ~w N=~w\n",[Prefix_mid,Element_mid,Suffix_mid,List_rebuild,N_rebuild]),
   % test consist in comparing!
   List = List_rebuild,
   N = N_rebuild.

% ---

test(suffix_and_N_known) :-
   splinter0(L,3,FL,E,[f,g,h]),
   E  = e,
   FL = [x,y,z],
   L  = [x,y,z,e,f,g,h].

% ---

test(only_list_known) :-
   bagof([N,Prefix,Element,Suffix],splinter0([a,b,c,d],N,Prefix,Element,Suffix),Bag),
   Bag = [[0, [], a, [b, c, d]],
          [1, [a], b, [c, d]],
          [2, [a, b], c, [d]],
          [3, [a, b, c], d, []]].

:- end_tests(splinter0).

rt :- run_tests(splinter0).

