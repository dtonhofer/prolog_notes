:- use_module(library('heavycarbon/utils/splinter0.pl')).

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

test("negative index",[throws(error(_,_))]) :- splinter0([a,b],-1,_,_,_).
test("empty list oob1",[fail])              :- splinter0([],0,_,_,_).
test("empty list oob2",[fail])              :- splinter0([],1,_,_,_).
test("nonempty list oob",[fail])            :- splinter0([a,b],2,_,_,_).
test("too much_freedom",[throws(_)])        :- splinter0(_List,_N,_Prefix,_Element,_Suffix).

% ---
% "List and N known"
% ---

test("case of: list and N known") :-
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

test("case of: list and prefix known") :-
   longlist(List),
   Prefix=[a,b,c,d],
   splinter0(List,N,Prefix,Element,Suffix),
   append([Prefix,[Element],Suffix],List),
   length(Prefix,N).

% ---
% "List and suffix known" (foreach with added cut)
% ---

% Mystery: 
% In 8.3.4, there is no open choicepoint
% But in SWI Prolog 8.3.5, this code leaves a final
% choicepoint open apparently at the "append/2" in 
% extract_a_suffix (hence a cut after that append/2), but
% running that append at the toplevel does nothing of the sort.

test("case of: list and suffix known (foreach, added cut)") :-
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

extract_a_suffix(List,SuffixLen,Suffix) :-
   length(Suffix,SuffixLen),
   append([_,Suffix],List),
   !. % The cut of mystery!

% ---
% "List and suffix known" (forall, no cut)
% ---

test("case of: list and suffix known (forall, no cut)") :-
   longlist(List),
   length(List,ListLen),
   succ(SuffixLen_max,ListLen),
   forall(between(0,SuffixLen_max,SuffixLen),list_and_suffix_known_cutless(List,SuffixLen)).

list_and_suffix_known_cutless(List,SuffixLen) :-
   extract_a_suffix_cutless(List,SuffixLen,Suffix),
   splinter0(List,N_out,Prefix_out,Element_out,Suffix),
   debug(splinter_test,"List=~w Suffix=~w ==> ~w ~w ~w\n",[List,Suffix,Prefix_out,Element_out,N_out]),
   % test consists in checking the spec!
   append([Prefix_out,[Element_out],Suffix],List),
   length(Prefix_out,N_out).

extract_a_suffix_cutless(List,SuffixLen,Suffix) :-
   length(Suffix,SuffixLen),
   append([_,Suffix],List).

% ---
% "prefix and suffix known"
% ---

test("case of: prefix and suffix known") :-
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

test("case of: suffix and N known") :-
   splinter0(L,3,FL,E,[f,g,h]),
   E  = e,
   FL = [x,y,z],
   L  = [x,y,z,e,f,g,h].

% ---
% only list known
% ---

test("case of: only list known") :-
   bagof([N,Prefix,Element,Suffix],splinter0([a,b,c,d],N,Prefix,Element,Suffix),Bag),
   Bag = [[0, [], a, [b, c, d]],
          [1, [a], b, [c, d]],
          [2, [a, b], c, [d]],
          [3, [a, b, c], d, []]].

:- end_tests(splinter0).

