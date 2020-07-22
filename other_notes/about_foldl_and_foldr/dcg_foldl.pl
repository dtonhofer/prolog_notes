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
% linear foldl using DCGs
% ("foldl" is the "Laughably recursive fold": subject to tail call optimization)
% ===
% dcg_foldl(+Foldy,+List,+Acc,?Result)
% ---
% Foldy   : Name of the predicate to call at each node of the list. That predicate takes:
%           - the node's item as first argument (called "Item" in the code);
%           - the result from the returning recursion as second argument (called "ThreadIn"
%             in the code as it is "threaded in" (like a thread) to the function/predicate);
%           - the result of the function application will be unified with the third argument
%             (called "ThreadOut" in the code as it is "threaded out").
% List    : List to traverse.
% Starter : The starter value.
% Out     : The result of the "foldr process" will be unified with this argument.
% ===

dcg_foldl(Foldy,List,Starter,Result) :-
   phrase(recognizer(Foldy,Starter,Result),List).

recognizer(Foldy,Acc,Result) --> 
   [Item],                             % case of another Item in the list
   !,                                  % GREEN CUT for determinism
   { call(Foldy,Item,Acc,AccNext) },     
   recognizer(Foldy,AccNext,Result).

recognizer(_Foldy,Acc,Result) --> 
   [],                                 % case of list is empty
   {Acc=Result}.                       % unification not in head for clarity


% ===
% Unit tests
% The expected value is not given to the predicate. Let it run, compare afterwards.
% ===

:- begin_tests(dcg_foldl).

in([1,2,3,4,5]).

dfl(Foldy,List,Starter,Result) :- dcg_foldl(Foldy,List,Starter,Result).

test(dcg_foldl_len)    :- in(L),dfl(foldy_len     , L ,  0 , R), R=5.
test(dcg_foldl_add)    :- in(L),dfl(foldy_add     , L,   0 , R), R=15.
test(dcg_foldl_mult)   :- in(L),dfl(foldy_mult    , L,   1 , R), R=120.
test(dcg_foldl_build)  :- in(L),dfl(foldy_build   , L,  [] , R), R=[5,4,3,2,1].
test(dcg_foldl_squadd) :- in(L),dfl(foldy_squadd  , L,   0 , R), R=21909.
test(dcg_foldl_join)   :- in(L),dfl(foldy_join    , L,  "" , R), R="5,4,3,2,1".
test(dcg_foldl_expr)   :- in(L),dfl(foldy_expr(*) , L,   1 , R), R=5*(4*(3*(2*(1*1)))).

test(dcg_foldl_len_empty)    :- dfl(foldy_len     , [],  0 , R), R=0.
test(dcg_foldl_add_empty)    :- dfl(foldy_add     , [],  0 , R), R=0.
test(dcg_foldl_mult_empty)   :- dfl(foldy_mult    , [],  1 , R), R=1.
test(dcg_foldl_build_empty)  :- dfl(foldy_build   , [], [] , R), R=[].
test(dcg_foldl_squadd_empty) :- dfl(foldy_squadd  , [],  0 , R), R=0.
test(dcg_foldl_join_empty)   :- dfl(foldy_join    , [], "" , R), R="".
test(dcg_foldl_expr_empty)   :- dfl(foldy_expr(*) , [],  1 , R), R=1.

:- end_tests(dcg_foldl).

rt :- run_tests(dcg_foldl).

% Run like this
%
% ?- [foldy],[dcg_foldl].
% true.
%
% ?- rt.
%% PL-Unit: dcg_foldl .............. done
%% All 14 tests passed
% true.

