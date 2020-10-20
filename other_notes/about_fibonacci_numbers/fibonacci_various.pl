:- use_module(library(clpfd)).

% =============================================================================
% Compute the Fibonacci Series, F = fib(N), for N >= 0, using various
% approaches
%
% More on this series:
% https://en.wikipedia.org/wiki/Fibonacci_number
%
% The idea for the lazy list comes from
% http://rosettacode.org/wiki/Fibonacci_sequence#Prolog
% =============================================================================

% -----------------------------------------------------------------------------
% Testing using the excellent "plunit"
% https://eu.swi-prolog.org/pldoc/doc_for?object=section(%27packages/plunit.html%27)
% -----------------------------------------------------------------------------

:- debug(fib). % Debug printing on!

:- begin_tests(fib).

test("naive, tabled") :-
   Algo = naive_tabled,
   test_first_few_values(Algo),
   test_single_value(Algo).

test("top-down, cache is a list containing fib(x) ascending") :-
   Algo = topdown_cache_using_list_ascending,
   test_first_few_values(Algo),
   test_single_value(Algo).

test("top-down, cache is a list containing fib(x) descending") :-
   Algo = topdown_cache_using_list_descending,
   test_first_few_values(Algo),
   test_single_value(Algo).

test("top-down, cache is of unbound variables linked by CLPFD contraints") :-
   Algo = topdown_cache_with_clpfd_constraints,
   test_first_few_values(Algo),
   test_single_value(Algo).

test("top-down, cache is a dict using in accumulator style") :-
   Algo = topdown_cache_using_dict,
   test_first_few_values(Algo),
   test_single_value(Algo).

test("bottom-up, direct") :-
   Algo = bottomup_direct,
   test_first_few_values(Algo),
   test_single_value(Algo).

test("bottom-up, cache is a lazy list") :-
   Algo = bottomup_cache_using_lazy_list,
   test_first_few_values(Algo),
   test_single_value(Algo).

test("bottom-up, cache is a dict") :-
   Algo = bottomup_cache_using_dict,
   test_first_few_values(Algo),
   test_single_value(Algo).

test("Some performance printout") :-
   %%%%  NOT WORKING untable(fib/2),table(fib/2), % clear the retained tabled data
   Algos=[ naive_tabled
          ,topdown_cache_using_list_ascending
          ,topdown_cache_using_list_descending
          ,topdown_cache_with_clpfd_constraints
          ,topdown_cache_using_dict
          ,bottomup_direct
          ,bottomup_cache_using_lazy_list
          ,bottomup_cache_using_dict],
   maplist([A]>>subyall(A),Algos).

:- end_tests(fib).

% ---
% Helper to run time calls
% ---

subyall(Algo) :-
   bagof(_,
      F^N^(debug(fib,"######## Running ~q",[Algo]),
           member(N,[2000,3000,4000]),
           debug(fib,"...to compute fib(~q)",[N]),
           time(fib_algo(N,F,Algo))),_).

% ---
% What we expect
% ---

the_first_twenty( [0-0,1-1,2-1,3-2,4-3,5-5,6-8,7-13,8-21,9-34,10-55,11-89,12-144,13-233,14-377,15-610,16-987,17-1597,18-2584,19-4181] ).

the_1001st(N) :-
   atomic_list_concat(
      ["43466557686937456435688527675040625802564660517371",
       "78040248172908953655541794905189040387984007925516",
       "92959225930803226347752096896232398733224711616429",
       "96440906533187938298969649928516003704476137795166",
       "849228875"],NumberAsAtom),
   atom_number(NumberAsAtom,N).

% ---
% Common code for a single algorithm
% ---

test_first_few_values(Algo) :-
   the_first_twenty(Expected),
   nf_points(Algo,19,Result),
   assertion(Result == Expected).

test_single_value(Algo) :-
   the_1001st(Expected),
   fib_algo(1000,F,Algo),
   assertion(F == Expected).

% ---
% Collecting (x,fib(x)) points
% ---

nf_points(Algo,Max,Result) :-
   bagof(N-F,bagging_goal(N,F,Algo,Max),Result).

bagging_goal(N,F,Algo,Max) :-
   between(0,Max,N),
   fib_algo(N,F,Algo).

% ---
% Calling an algorithm by name
% ---

fib_algo(N,F,naive_tabled) :-
   !,fib(N,F).

fib_algo(N,F,topdown_cache_using_list_ascending) :-
   !,fib_topdown_cache_using_list_ascending(N,F,_Cache).

fib_algo(N,F,topdown_cache_using_list_descending) :-
   !,fib_topdown_cache_using_list_descending(N,F,_Cache).

fib_algo(N,F,topdown_cache_with_clpfd_constraints) :-
   !,fib_topdown_cache_using_list_descending(N,F,_Cache).

fib_algo(N,F,topdown_cache_using_dict) :-
   !,fib_topdown_cache_using_dict(N,F,_Cache).

fib_algo(N,F,bottomup_direct) :-
   !,fib_bottomup_direct(N,F).

fib_algo(N,F,bottomup_cache_using_lazy_list) :-
   !,fib_bottomup_cache_using_lazy_list(N,F,_Cache).

fib_algo(N,F,bottomup_cache_using_dict) :-
   !,fib_bottomup_cache_using_dict(N,F,_Cache).

% -----------------------------------------------------------------------------
% Carve the constants fib(0) and fib(1) out of the code.
% (Why? Hey, maybe we want to compute other series later?)
% -----------------------------------------------------------------------------

const(fib0,0).
const(fib1,1).

% -----------------------------------------------------------------------------
% Naive implementation, but tabled
% -----------------------------------------------------------------------------

:- table fib/2.

fib(0, F0) :- const(fib0,F0),!.
fib(1, F1) :- const(fib1,F1),!.
fib(N, F) :-
   N>1,!,                     % check and cut not really needed
   NA is N-1, fib(NA, FA),
   NB is N-2, fib(NB, FB),
   F is FA + FB.

% -----------------------------------------------------------------------------
% Proceed top-down, using a list of initially unbound variable as cache.
% The fib(N) for low N come first in the cache, nth0/3 is used to grab a cache
% entry.
% -----------------------------------------------------------------------------

fib_topdown_cache_using_list_ascending(N, F, Cache) :-
   Nplus is N+1,
   CacheSize is max(Nplus,2),
   length(Cache,CacheSize),
   const(fib0,F0),
   const(fib1,F1),
   Cache = [F0,F1|_],                           % cache begins with fib(0),fib(1)
   fib_rec_alfa(N,F,Cache).

fib_rec_alfa(N,F,Cache) :-
   nth0(N,Cache,X),                             % access using nth0/3 (costly, do it once, then use "->")
   (nonvar(X) ->
      (F=X)                                     % already cached, we are done!
      ;
      (NA is N-1, fib_rec_alfa(NA,FA,Cache),    % otherwise recursive calls
       NB is N-2, fib_rec_alfa(NB,FB,Cache),
       F is FA+FB,
       X=F)).                                   % puts new value into cache

% -----------------------------------------------------------------------------
% Proceed top-down, using a list of initially unbound variable as cache.
% The fib(N) for low N come last in the cache, access is immediate, every
% call the cache entry for fib(N) directly at the head of its given cache, a
% sublist.
% The result is obtained by consulting the head of the overall cache.
% -----------------------------------------------------------------------------

fib_topdown_cache_using_list_descending(N, F, Cache) :-
   Nminus is N-1,
   const(fib0,F0),
   const(fib1,F1),
   prepend_freshvar([F1,F0],Cache,Nminus),      % cache ends with fib(1),fib(0)
   fib_rec_bravo(N,Cache),
   Cache=[F|_].

% "fill the cache"

fib_rec_bravo(0,_) :- !.          % the cache is already set up for fib(0) at init time
fib_rec_bravo(1,_) :- !.          % the cache is already set up for fib(1) at init time

fib_rec_bravo(_N,[F|_]) :-
   nonvar(F),!.                   % the cache already contains a value for fib(_N)

fib_rec_bravo(N,[F|DCache1]) :-
   var(F),!,                      % the cache does not yet contain a value for fib(N)
   DCache1 = [FA|DCache2],
   DCache2 = [FB|_],
   NA is N-1, fib_rec_bravo(NA,DCache1),
   NB is N-2, fib_rec_bravo(NB,DCache2),
   F is FA+FB.

% ---
% A helper function
% prepend_freshvar(+Cache,-NewCache,+HowMany)
% To prepend HowMany freshvars to Cache, giving NewCache.
% HowMany can be -1 or 0, too in cases fib(0) or fib(1) is asked.
% ---

prepend_freshvar([_|R],R,-1) :- !.

prepend_freshvar(C,C,0) :- !.

prepend_freshvar(Cin,[_|Cup],Count) :-
   Cminus is Count-1,
   prepend_freshvar(Cin,Cup,Cminus).

% -----------------------------------------------------------------------------
% Proceed top-down, using a list of initially unbound variable as cache.
% The fib(N) for low N come last in the cache, access is immediate, every
% call the cache entry for fib(N) directly at the head of its given cache, a
% sublist.
% However, nothing is computed on the purely linear passage through the cache
% from fib(N) to fib(0) - only CLP(FD) constraints are set up. Once fib(0) and
% fib(1) are reached, or in this case, specified at the very end of the call
% will the result be computed through the constraint network.
% -----------------------------------------------------------------------------

fib_topdown_cache_with_clpfd_constraints(N, F, Cache) :-
   Nminus is N-1,
   prepend_freshvar([X,Y],Cache,Nminus),    % for fun, leave fib(0)=X, fib(1)=Y unspecified for now
   fib_charlie(Cache),
   Cache=[F|_],                             % we got the (as yet uncomputed result) at the head of the cache
   const(fib0,X),                           % specifying X=fib(0) and Y=fib(1) realizes the result F
   const(fib1,Y).

fib_charlie([_]) :- !.                      % nothing to do for fib(0)

fib_charlie([_,_]) :- !.                    % nothing to do for fib(1)

fib_charlie([F,FA,FB|FurtherCache]) :-      % linear loop through cache, just set up contraints
   F #= FA+FB,
   fib_charlie([FA,FB|FurtherCache]).

% -----------------------------------------------------------------------------
% Proceed top-down, using an associative array (dict) as cache.
% This is more costly as the cache is used as an immutable but growing
% accumulator (functional style), so lots of memory operations are involved.
% -----------------------------------------------------------------------------

fib_topdown_cache_using_dict(N, F, Cache) :-
   const(fib0,F0),
   const(fib1,F1),
   fib_rec_delta(cache{0:F0,1:F1},Cache,N),
   get_dict(N,Cache,F).

fib_rec_delta(CacheIn,CacheOut,N) :-
   get_dict(N,CacheIn,_)
   ->
   CacheOut=CacheIn                         % value in cache already, "tread the cache through"
   ;
   (NA is N-1, fib_rec_delta(CacheIn,Cache2,NA),
    NB is N-2, fib_rec_delta(Cache2,Cache3,NB),
    F is Cache3.get(NA) + Cache3.get(NB),
    put_dict(N,Cache3,F,CacheOut)).

% -----------------------------------------------------------------------------
% Proceed bottom-up, without using any cache
% -----------------------------------------------------------------------------

fib_bottomup_direct(0,0) :- !.

fib_bottomup_direct(N, F) :-
   N>0,!,
   Countdown is N-1,
   const(fib0,F0),
   const(fib1,F1),
   fib_bottomup_loop(Countdown,F0,F1,F).

% ---
% Tail recursive call moving "bottom up" towards N
% fib_bottomup_loop(+Countdown,+FA,+FB,-F)
% ---

fib_bottomup_loop(0,_,F,F) :- !.

fib_bottomup_loop(Countdown,FA,FB,F) :-
   Countdown>0,!,
   FC is FA+FB,
   CountdownNext is Countdown-1,
   fib_bottomup_loop(CountdownNext,FB,FC,F).

% -----------------------------------------------------------------------------
% Proceed bottom-up, the cache being a lazy list (an open list with a "frozen"
% goal on the unbound variables that takes the place of the final "[]", which
% I call the open list's FIN).
% The fib(N) for low N come first in the lazy list (necessarily) and the list
% can be automagically extended at its end by asking for values for higher N
% through unification.
% This implementation also works if you pass a closed list of unbound variables
% as "Cache". Then all the Fibonacci values from fib(0) to fib(length(list))
% are computed.
% -----------------------------------------------------------------------------

fib_bottomup_cache_using_lazy_list(N, F, Cache) :-
   fib_lazy_list(Cache),
   nth0(N,Cache,F).

% You always get a lazy list with at least 2 elements computed-out.
% Wire up the rest of the list with a "frozen goal".
% "More" may be:
% - unbound variable (the open list FIN) or
% - the [] (a properlist/closedlist FIN)
% - actually a start of a list if the cache has been pre-set to an open or
%   closed list.

fib_lazy_list([F0,F1|More]) :-
    const(fib0,F0),
    const(fib1,F1),
    traverse_cache_and_wire_up_fin(F0,F1,More).

% If FIN is [] we have reached the end of the closed list and thus
% we are done. Note that the head avoids unifying with [], to correctly
% test equality with '=='.

traverse_cache_and_wire_up_fin(_,_,FIN) :-
   FIN==[],!.

% If FIN is an unbound variable, we hit the end of an open list, so wire
% up a frozen goal there.

traverse_cache_and_wire_up_fin(Fa,Fb,FIN) :-
   var(FIN),
   !,
   freeze(FIN,fib_lazy_list_goal(Fa,Fb,FIN)).

% Otherwise the list continues, and we just compute next fibonacci number.

traverse_cache_and_wire_up_fin(Fa,Fb,[Fc|More]) :-
   just_add(Fa,Fb,Fc),
   traverse_cache_and_wire_up_fin(Fb,Fc,More).

% This predicate will be called the first time the FIN of the
% open list participates in a unification, which happens when
% the next as-yet-unrealized element of the list is demanded.
% The goal FIN=[Fc|NewFIN], appends Fc and creates a new FIN as
% unbound variable.

fib_lazy_list_goal(Fa,Fb,FIN) :-
   just_add(Fa,Fb,Fc),
   FIN=[Fc|NewFIN],
   traverse_cache_and_wire_up_fin(Fb,Fc,NewFIN).

% The formula, moved out to follow the don't-repeat-yourself principle

just_add(V1,V2,VN) :- VN is V1+V2.

% -----------------------------------------------------------------------------
% Proceed bottom-up, using an associative array (dict) as cache.
% The cache is used as an immutable but growing accumulator (functional style),
% so lots of memory operations are involved. But we have a big cache at the
% end!
% -----------------------------------------------------------------------------

fib_bottomup_cache_using_dict(0,0,cache{0:F0}) :-
   const(fib0,F0).

fib_bottomup_cache_using_dict(N,F,Cache) :-
   N>0,!,
   Countdown is N-1,
   const(fib0,F0),
   const(fib1,F1),
   fib_bottomup_loop_with_dict(Countdown,N,cache{0:F0,1:F1},Cache),
   get_dict(N,Cache,F).

% ---
% Tail recursive call moving "bottom up" towards N
% fib_bottomup_loop_with_dict(+Countdown,+N,+CacheIn,-CacheOut)
% ---

fib_bottomup_loop_with_dict(0,_,Cache,Cache) :- !.

fib_bottomup_loop_with_dict(Countdown,N,CacheIn,CacheOut) :-
   Ncur is N-Countdown+1,  % we shall compute fib(Ncur)
   NA is Ncur-2,
   NB is Ncur-1,
   F is CacheIn.NA + CacheIn.NB,    % retrieve from cache using . notation
   put_dict(Ncur,CacheIn,F,Cache2), % build new dict with new "Ncur -> F" mapping
   CountdownNext is Countdown-1,
   fib_bottomup_loop_with_dict(CountdownNext,N,Cache2,CacheOut).

