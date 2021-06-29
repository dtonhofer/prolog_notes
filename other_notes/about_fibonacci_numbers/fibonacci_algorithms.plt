/*  Zero-Clause BSD (0BSD) follows (https://opensource.org/licenses/0BSD)

    Permission to use, copy, modify, and/or distribute this software for
    any purpose with or without fee is hereby granted.

    THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL
    WARRANTIES WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED
    WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE
    AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL
    DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA
    OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER
    TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
    PERFORMANCE OF THIS SOFTWARE.
*/

:- use_module(library(clpfd)).
:- use_module('fibonacci_algorithms.pl').

% =============================================================================
% Tests for the algorithms computing the Fibonacci Series.
%
% The testing framework used here is the excellent "plunit"
% https://eu.swi-prolog.org/pldoc/doc_for?object=section(%27packages/plunit.html%27)
% =============================================================================

:- debug(fib). % Debug printing on!
% :- debug(fib_freeze).

:- begin_tests(fibonacci).

% --- helpers ---

the_first_twenty( 
   [0-0, 1-1, 2-1, 3-2, 4-3, 5-5, 6-8, 7-13, 8-21, 9-34, 10-55,
    11-89, 12-144, 13-233, 14-377, 15-610, 16-987, 17-1597, 
    18-2584, 19-4181]).

the_1001st(N) :-
   atomic_list_concat(
      ["43466557686937456435688527675040625802564660517371",
       "78040248172908953655541794905189040387984007925516",
       "92959225930803226347752096896232398733224711616429",
       "96440906533187938298969649928516003704476137795166",
       "849228875"],NumberAsAtom),
   atom_number(NumberAsAtom,N).

first_few_values(Algo) :-
   the_first_twenty(Expected),
   collect_pairs(Algo,19,Result),
   assertion(Result == Expected).

single_value(Algo) :-
   the_1001st(Expected),
   fib_algo(Algo,1000,F),
   assertion(F == Expected).

collect_pairs(Algo,Max,Result) :-
   bagof(N-F,
      (between(0,Max,N),
       fib_algo(Algo,N,F)),
      Result).

% Calling an algorithm by name. 

fib_algo(naive_tabled                           , N,F) :- !,fib(N,F).
fib_algo(bottomup_direct                        , N,F) :- !,fib_bottomup_direct(N,F).
fib_algo(bottomup_dict_cache                    , N,F) :- !,fib_bottomup_dict_cache(N,F,_Cache).
fib_algo(bottomup_frozen_cache                  , N,F) :- !,fib_bottomup_frozen_cache(N,F,_Cache).
fib_algo(bottomup_lazylist_cache                , N,F) :- !,fib_bottomup_lazylist_cache(N,F,_Cache).
fib_algo(topdown_list_cache_ascending           , N,F) :- !,fib_topdown_list_cache_ascending(N,F,_Cache).
fib_algo(topdown_list_cache_descending          , N,F) :- !,fib_topdown_list_cache_descending(N,F,_Cache).
fib_algo(topdown_list_cache_descending_cautious , N,F) :- !,fib_topdown_list_cache_descending_cautious(N,F,_Cache).
fib_algo(topdown_list_cache_descending_debug    , N,F) :- !,fib_topdown_list_cache_descending_debug(N,F,_Cache).
fib_algo(topdown_list_cache_clpfd               , N,F) :- !,fib_topdown_list_cache_clpfd(N,F,_Cache).
fib_algo(topdown_dict_cache                     , N,F) :- !,fib_topdown_dict_cache(N,F,_Cache).
fib_algo(matrixmult                             , N,F) :- !,fib_matrixmult(N,F).
fib_algo(matrixmult_streamlined                 , N,F) :- !,fib_matrixmult_streamlined(N,F).
fib_algo(fast_doubling                          , N,F) :- !,fib_fast_doubling(N,F).
fib_algo(golden_ratio                           , N,F) :- !,fib_golden_ratio(N,F).
fib_algo(X,_,_)                                        :- existence_error(algorithm_name,X).


% --- actual tests ---

test("naive, tabled") :-
   first_few_values(naive_tabled),
   single_value(naive_tabled).

test("bottom-up, direct") :-
   first_few_values(bottomup_direct),
   single_value(bottomup_direct).

test("bottom-up, cache is a dict") :-
   first_few_values(bottomup_dict_cache),
   single_value(bottomup_dict_cache).

test("bottom-up, cache is a list of var with frozen goals") :-
   first_few_values(bottomup_frozen_cache),
   single_value(bottomup_frozen_cache).

test("bottom-up, cache is a lazy list (which is open) with a frozen goal on fin") :-
   first_few_values(bottomup_lazylist_cache),
   single_value(bottomup_lazylist_cache).

test("top-down, cache is a list with fib(N) ascending") :-
   first_few_values(topdown_list_cache_ascending),
   single_value(topdown_list_cache_ascending).

test("top-down, cache is list with fib(N) descending") :-
   first_few_values(topdown_list_cache_descending),
   single_value(topdown_list_cache_descending).

test("top-down, cache is list with fib(N) descending, cautious") :-
   first_few_values(topdown_list_cache_descending_cautious),
   single_value(topdown_list_cache_descending_cautious).

test("top-down, cache is list with fib(N) descending, using constraints") :-
   first_few_values(topdown_list_cache_clpfd),
   single_value(topdown_list_cache_clpfd).

test("top-down, cache is a dict") :-
   first_few_values(topdown_dict_cache),
   single_value(topdown_dict_cache).

test("matrix multiplication") :-
   first_few_values(matrixmult),
   single_value(matrixmult).

test("matrix multiplication, streamlined") :-
   first_few_values(matrixmult_streamlined),
   single_value(matrixmult_streamlined).

test("fast doubling") :-
   first_few_values(fast_doubling),
   single_value(fast_doubling).

test("golden ratio") :-
   first_few_values(golden_ratio),
   single_value(golden_ratio).

:- end_tests(fibonacci).

% =============================================================================
% Tests where you start not with 0 and 1 but some other values.
% =============================================================================

:- begin_tests(fibonacci_with_unusual_starter_values).

expected_result(Fib0,Fib1,Pairs,Max) :-
   Fib0 = 3,
   Fib1 = 7,
   Pairs = [0-3,1-7,2-10,3-17,4-27,5-44,6-71,7-115,8-186,9-301,10-487,11-788,12-1275,13-2063,14-3338,15-5401],
   length(Pairs,MaxPlusOne),
   Max is MaxPlusOne-1.

test("Unusual starter values, bottomup-direct") :-
   expected_result(Fib0,Fib1,Pairs,Max),
   bagof(
      N-F,
      (between(0,Max,N),fib_bottomup_direct_usv(N,F,Fib0,Fib1)),
      Result),
   assertion(Result == Pairs).

test("Unusual starter values, matrixmult") :-
   expected_result(Fib0,Fib1,Pairs,Max),
   bagof(
      N-F,
      (between(0,Max,N),fib_matrixmult_usv(N,F,Fib0,Fib1)),
      Result),
   assertion(Result == Pairs).

test("Unusual starter values, matrixmult streamlined") :-
   expected_result(Fib0,Fib1,Pairs,Max),
   bagof(N-F,
      (between(0,Max,N),fib_matrixmult_streamlined_usv(N,F,Fib0,Fib1)),
      Result),
   assertion(Result == Pairs).

:- end_tests(fibonacci_with_unusual_starter_values).

% =============================================================================
% Tests of only the "matrixpow" predicates, which also comput Fibonacci numbers
% =============================================================================

:- begin_tests(matrixpow).

test("matrixpow") :-
   bagof(Power-Result,
      (
         between(0,12,Power),
         matrixpow(Power, [[1,1],[1,0]], Result)
      ),
      Bag),
   assertion(Bag == 
      [0-[[1,0],
          [0,1]],
       1-[[1,1],
          [1,0]],
       2-[[2,1],
          [1,1]],
       3-[[3,2],
          [2,1]],
       4-[[5,3],
          [3,2]],
       5-[[8,5],
          [5,3]],
       6-[[13,8],
          [8,5]],
       7-[[21,13],
          [13,8]],
       8-[[34,21],
          [21,13]],
       9-[[55,34],
          [34,21]],
      10-[[89,55],
          [55,34]],
      11-[[144,89],
          [89,55]],
      12-[[233,144],
          [144,89]]]).

test("matrixpow_streamlined") :-
   bagof(Power-Result,
      (
         between(0,12,Power),
         matrixpow_streamlined(Power, v(1,0), Result)
      ),
      Bag),
   assertion(Bag == 
      [0-v(0,1),
       1-v(1,0),
       2-v(1,1),
       3-v(2,1),
       4-v(3,2),
       5-v(5,3),
       6-v(8,5),
       7-v(13,8),
       8-v(21,13),
       9-v(34,21),
      10-v(55,34),
      11-v(89,55),
      12-v(144,89)]).

:- end_tests(matrixpow).


/*
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
*/

% ---
% Simple timing of algorithms computing fib(2000), fib(3000), fib(4000)
% ---
/*
subyall(Algo) :-
   bagof(_,
      F^N^(debug(fib,"######## Running ~q",[Algo]),
           member(N,[2000,3000,4000]),
           debug(fib,"...to compute fib(~q)",[N]),
           time(fib_algo(N,F,Algo))),_).
*/


