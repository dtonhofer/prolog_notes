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
% linear foldr
% ("foldr" is the "Really recursive fold": not subject to tail call optimization)
% ===
% foo_foldr(+Foldy,+List,+ThreadIn,?ThreadOut)
% ---
% Foldy     : Name of the predicate to call at each node of the list. That predicate takes:
%             - the node's item as first argument (called "Item" in the code);
%             - the result from the returning recursion as second argument
%               (called "Intermed" in the code);
%             - the result of the function application will be unified with the third argument
%               (called "ThreadOut" in the code).
% List      : List to traverse.
% ThreadIn  : The value being threaded in unmodified to the end of the recursion.
%             Initially the starter value.
% ThreadOut : The result of the "foldr process" will be unified with this argument.
% ===

foo_foldr(_,[],ThreadEnd,ThreadEnd) :- !. % GREEN CUT

foo_foldr(Foldy,[Item|Ls],ThreadIn,ThreadOut) :-
   foo_foldr(Foldy,Ls,ThreadIn,Intermed),
   call(Foldy,Item,Intermed,ThreadOut).

% ===
% Unit tests
% The expected value is not given to the predicate. Let it run, compare afterwards.
% ===

:- begin_tests(foo_foldr).

test(foldr_add)    :- foo_foldr(foldy_add    , [1,2,3,4,5],  0 , Out), Out=15.
test(foldr_mult)   :- foo_foldr(foldy_mult   , [1,2,3,4,5],  1 , Out), Out=120.
test(foldr_build)  :- foo_foldr(foldy_build  , [1,2,3,4,5], [] , Out), Out=[1,2,3,4,5].
test(foldr_squadd) :- foo_foldr(foldy_squadd , [1,2,3,4,5],  0 , Out), Out=507425426245. % need GMP library here
test(foldr_join)   :- foo_foldr(foldy_join   , [1,2,3,4,5], "" , Out), Out="1,2,3,4,5".
test(foldr_expr)   :- foo_foldr(foldy_expr(*) , [1,2,3,4,5], 1 , Out), Out=1*(2*(3*(4*(5*1)))).

test(foldr_add_empty)    :- foo_foldr(foldy_add     , [],  0 , Out), Out=0.
test(foldr_mult_empty)   :- foo_foldr(foldy_mult    , [],  1 , Out), Out=1.
test(foldr_build_empty)  :- foo_foldr(foldy_build   , [], [] , Out), Out=[].
test(foldr_squadd_empty) :- foo_foldr(foldy_squadd  , [],  0 , Out), Out=0.
test(foldr_join_empty)   :- foo_foldr(foldy_join    , [], "" , Out), Out="".
test(foldr_expr_empty)   :- foo_foldr(foldy_expr(*) , [],  1 , Out), Out=1.

% library(apply) has no "foldr" so no comparison tests!

:- end_tests(foo_foldr).

rt :- run_tests(foo_foldr).
