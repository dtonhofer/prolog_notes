% 2345678901234567890123456789012345678901234567890123456789012345678901234567
% ============================================================================
% 2020-04-19
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

% ===
% linear foldl
% ("foldl" is the "Laughably recursive fold": subject to tail call optimization)
% ===
% foo_foldl(+Foldy,+List,+Acc,?Result)
%
% Foldy   : Name of the predicate to call at each node of the list. That predicate takes:
%             - the node's item as first argument (called "Item" in the code);
%             - the value obtained by the series of function application so far
%               (called "AccDown" for "accumulator, downwards" in the code);
%             - and the result of the function application will be unified with the third argument
%               (called "Result" in the code).
% List      : List to traverse.
% Acc       : Accumulator. The value being threaded in and out of "Foldy" and exchanged 
%             with more updated values as we move towards the end of recursion. Initially
%             the "starter value".
% Result    : The final value of the Accumulator will be unified with this argument at the
%             end of recursion.
% ===

foo_foldl(Foldy,[Item|Items],AccDown,Result) :-  % case of nonempty list
   !,                                            % GREEN CUT for determinism
   call(Foldy,Item,AccDown,AccDownNext),
   foo_foldl(Foldy,Items,AccDownNext,Result).

foo_foldl(_,[],AccDown,Result) :-                % empty list: bounce AccDown "upwards" into Result
   AccDown=Result.                               % unification not in head for clarity

% ===
% Unit tests
% The expected value is not given to the predicate. Let it run, compare afterwards.
% ===

:- begin_tests(foo_foldl).

in([1,2,3,4,5]).

ffl(Foldy,List,Starter,Result) :- foo_foldl(Foldy,List,Starter,Result).
ffa(Foldy,List,Starter,Result) :- foldl(Foldy,List,Starter,Result). % from library(apply)

test(foo_foldl_len)    :- in(L),ffl(foldy_len     , L ,  0 , R), R=5.
test(foo_foldl_add)    :- in(L),ffl(foldy_add     , L,   0 , R), R=15.
test(foo_foldl_mult)   :- in(L),ffl(foldy_mult    , L,   1 , R), R=120.
test(foo_foldl_build)  :- in(L),ffl(foldy_build   , L,  [] , R), R=[5,4,3,2,1].
test(foo_foldl_squadd) :- in(L),ffl(foldy_squadd  , L,   0 , R), R=21909.
test(foo_foldl_join)   :- in(L),ffl(foldy_join    , L,  "" , R), R="5,4,3,2,1".
test(foo_foldl_expr)   :- in(L),ffl(foldy_expr(*) , L,   1 , R), R=5*(4*(3*(2*(1*1)))).

test(foo_foldl_len_empty)    :- ffl(foldy_len     , [],  0 , R), R=0.
test(foo_foldl_add_empty)    :- ffl(foldy_add     , [],  0 , R), R=0.
test(foo_foldl_mult_empty)   :- ffl(foldy_mult    , [],  1 , R), R=1.
test(foo_foldl_build_empty)  :- ffl(foldy_build   , [], [] , R), R=[].
test(foo_foldl_squadd_empty) :- ffl(foldy_squadd  , [],  0 , R), R=0.
test(foo_foldl_join_empty)   :- ffl(foldy_join    , [], "" , R), R="".
test(foo_foldl_expr_empty)   :- ffl(foldy_expr(*) , [],  1 , R), R=1.

% Compare with the results of foldl/4 from "library(apply)": SAME!

test(lib_foldl_len)    :- in(L),ffa(foldy_len     , L ,  0 , R), R=5.
test(lib_foldl_add)    :- in(L),ffa(foldy_add     , L ,  0 , R), R=15.
test(lib_foldl_mult)   :- in(L),ffa(foldy_mult    , L ,  1 , R), R=120.
test(lib_foldl_build)  :- in(L),ffa(foldy_build   , L , [] , R), R=[5,4,3,2,1].
test(lib_foldl_squadd) :- in(L),ffa(foldy_squadd  , L ,  0 , R), R=21909.
test(lib_foldl_join)   :- in(L),ffa(foldy_join    , L , "" , R), R="5,4,3,2,1".
test(lib_foldl_expr)   :- in(L),ffa(foldy_expr(*) , L ,  1 , R), R=5*(4*(3*(2*(1*1)))).

test(lib_foldl_len_empty)    :- ffa(foldy_len     , [] ,  0 , R), R=0.
test(lib_foldl_add_empty)    :- ffa(foldy_add     , [] ,  0 , R), R=0.
test(lib_foldl_mult_empty)   :- ffa(foldy_mult    , [] ,  1 , R), R=1.
test(lib_foldl_build_empty)  :- ffa(foldy_build   , [] , [] , R), R=[].
test(lib_foldl_squadd_empty) :- ffa(foldy_squadd  , [] ,  0 , R), R=0.
test(lib_foldl_join_empty)   :- ffa(foldy_join    , [] , "" , R), R="".
test(lib_foldl_expr_empty)   :- ffa(foldy_expr(*) , [] ,  1 , R), R=1.

:- end_tests(foo_foldl).

rt :- run_tests(foo_foldl).

/* RUN IT

?- [foldy],[foo_foldl].
true.

?- rt.
% PL-Unit: foo_foldl ............................ done
% All 28 tests passed
true.

*/

