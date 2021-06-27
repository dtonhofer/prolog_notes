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

test("naive, tabled") :-
   test_first_few_values(naive_tabled),
   test_single_value(naive_tabled).

test("bottom-up, direct") :-
   test_first_few_values(bottomup_direct),
   test_single_value(bottomup_direct).

test("bottom-up, cache is a dict") :-
   test_first_few_values(bottomup_dict_cache),
   test_single_value(bottomup_dict_cache).

test("bottom-up, cache is a list of var with frozen goals") :-
   test_first_few_values(bottomup_frozen_cache),
   test_single_value(bottomup_frozen_cache).

test("bottom-up, cache is a lazy list (which is open) with a frozen goal on fin") :-
   test_first_few_values(bottomup_lazylist_cache),
   test_single_value(bottomup_lazylist_cache).

test("top-down, cache is a list with fib(N) ascending") :-
   test_first_few_values(topdown_list_cache_ascending),
   test_single_value(topdown_list_cache_ascending).

test("top-down, cache is list with fib(N) descending") :-
   test_first_few_values(topdown_list_cache_descending),
   test_single_value(topdown_list_cache_descending).

test("top-down, cache is list with fib(N) descending, cautious") :-
   test_first_few_values(topdown_list_cache_descending_cautious),
   test_single_value(topdown_list_cache_descending_cautious).

test("top-down, cache is list with fib(N) descending, using constraints") :-
   test_first_few_values(topdown_list_cache_clpfd),
   test_single_value(topdown_list_cache_clpfd).

test("top-down, cache is a dict") :-
   test_first_few_values(topdown_dict_cache),
   test_single_value(topdown_dict_cache).

% ---
% What we expect
% ---

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

% ---
% Common code for a single algorithm
% ---

test_first_few_values(Algo) :-
   the_first_twenty(Expected),
   nf_points(Algo,19,Result),
   assertion(Result == Expected).

test_single_value(Algo) :-
   the_1001st(Expected),
   fib_algo(Algo,1000,F),
   assertion(F == Expected).

% ---
% Collecting pairs x-fib(x), with x going from 0..Max.
% Eeach point is computed individually.
% ---

nf_points(Algo,Max,Result) :-
   bagof(N-F,bagging_goal(Algo,N,F,Max),Result).

bagging_goal(Algo,N,F,Max) :-
   between(0,Max,N),
   fib_algo(Algo,N,F).


:- end_tests(fibonacci).

% ---
% Calling an algorithm by name. 
% Name is the first argument to use Prolog's indexing. This
% means we don't need to cut to achieve determinism.
% ---

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
fib_algo(X,_,_)                                        :- existence_error(algorithm_name,X).

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


