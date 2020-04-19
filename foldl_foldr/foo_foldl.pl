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
% foo_foldl(+Foldy,+List,+ThreadIn,?ThreadOut)
% ---
% Foldy   : Name of the predicate to call at each node of the list. That predicate takes:
%             - the node's item as first argument (called "Item" in the code);
%             - the result of the series of function application so far
%               (called "Intermed" in the code);
%             - the result of the function application will be unified with the third argument
%               (called "ThreadOut" in the code).
% List      : List to traverse.
% ThreadIn  : The value being threaded in and folded through "Foldy" while moving to the
%             end of the recursion and the list. Initially the starter value. Actually an
%             "accumulator".
% ThreadOut : The result of the "foldl process" will be unified with this argument at the
%             end of recursion and the list.
% ===

foo_foldl(_,[],ThreadEnd,ThreadEnd) :- !. % GREEN CUT

foo_foldl(Foldy,[Item|Ls],ThreadIn,ThreadOut) :-
   call(Foldy,Item,ThreadIn,Intermed),
   foo_foldl(Foldy,Ls,Intermed,ThreadOut).

% ===
% Unit tests
% The expected value is not given to the predicate. Let it run, compare afterwards.
% ===

:- begin_tests(foo_foldl).

test(foldl_add)    :- foo_foldl(foldy_add     , [1,2,3,4,5],  0 , Out), Out=15.
test(foldl_mult)   :- foo_foldl(foldy_mult    , [1,2,3,4,5],  1 , Out), Out=120.
test(foldl_build)  :- foo_foldl(foldy_build   , [1,2,3,4,5], [] , Out), Out=[5,4,3,2,1].
test(foldl_squadd) :- foo_foldl(foldy_squadd  , [1,2,3,4,5],  0 , Out), Out=21909.
test(foldl_join)   :- foo_foldl(foldy_join    , [1,2,3,4,5], "" , Out), Out="5,4,3,2,1".
test(foldl_expr)   :- foo_foldl(foldy_expr(*) , [1,2,3,4,5],  1 , Out), Out=5*(4*(3*(2*(1*1)))).

test(foldl_add_empty)    :- foo_foldl(foldy_add     , [],  0 , Out), Out=0.
test(foldl_mult_empty)   :- foo_foldl(foldy_mult    , [],  1 , Out), Out=1.
test(foldl_build_empty)  :- foo_foldl(foldy_build   , [], [] , Out), Out=[].
test(foldl_squadd_empty) :- foo_foldl(foldy_squadd  , [],  0 , Out), Out=0.
test(foldl_join_empty)   :- foo_foldl(foldy_join    , [], "" , Out), Out="".
test(foldl_expr_empty)   :- foo_foldl(foldy_expr(*) , [],  1 , Out), Out=1.

% Compare with the results of foldl/4 from "library(apply)": SAME!

test(lib_foldl_add)    :- foldl(foldy_add     , [1,2,3,4,5] ,  0 , Out), Out=15.
test(lib_foldl_mult)   :- foldl(foldy_mult    , [1,2,3,4,5] ,  1 , Out), Out=120.
test(lib_foldl_build)  :- foldl(foldy_build   , [1,2,3,4,5] , [] , Out), Out=[5,4,3,2,1].
test(lib_foldl_squadd) :- foldl(foldy_squadd  , [1,2,3,4,5] ,  0 , Out), Out=21909.
test(lib_foldl_join)   :- foldl(foldy_join    , [1,2,3,4,5] , "" , Out), Out="5,4,3,2,1".
test(lib_foldl_expr)   :- foldl(foldy_expr(*) , [1,2,3,4,5] ,  1 , Out), Out=5*(4*(3*(2*(1*1)))).

test(lib_foldl_add_empty)    :- foldl(foldy_add     , [] ,  0 , Out), Out=0.
test(lib_foldl_mult_empty)   :- foldl(foldy_mult    , [] ,  1 , Out), Out=1.
test(lib_foldl_build_empty)  :- foldl(foldy_build   , [] , [] , Out), Out=[].
test(lib_foldl_squadd_empty) :- foldl(foldy_squadd  , [] ,  0 , Out), Out=0.
test(lib_foldl_join_empty)   :- foldl(foldy_join    , [] , "" , Out), Out="".
test(lib_foldl_expr_empty)   :- foldl(foldy_expr(*) , [] ,  1 , Out), Out=1.

:- end_tests(foo_foldl).

rt :- run_tests(foo_foldl).
