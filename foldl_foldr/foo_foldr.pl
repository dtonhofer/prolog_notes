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
% foo_foldr(+Foldy,+List,+Acc,?Result)
% ---
% Foldy     : Name of the predicate to call at each node of the list. That predicate takes:
%             - the node's item as first argument (called "Item" in the code);
%             - the result from the returning recursion as second argument
%               (called "AccUpPrev" for "accumulator, upwards" in the code);
%             - and the result of the function application will be unified with the third argument
%               (called "AccUp" in the code; that value will inform the next call of Foldy.
% List      : List to traverse.
% Starter   : The value being threaded in unmodified to the end of the recursion. The initial
%             value of the Accumulator will be unified with this argument at the end of
%             recursion and the start of return-from-recursion.
% AccUp     : Accumulator. The value being threaded in and out of "Foldy" and exchanged 
%             with more updated values as we return from recursion. Initially the "starter value".
% ===

foo_foldr(Foldy,[Item|Items],Starter,AccUp) :-    % case of nonempty list
   !,                                             % GREEN CUT for determinism
   foo_foldr(Foldy,Items,Starter,AccUpPrev),
   call(Foldy,Item,AccUpPrev,AccUp).

foo_foldr(_,[],Starter,AccUp) :-                  % empty list: bounce Starter "upwards" into AccUp
   AccUp=Starter.                                 % unification not in head for clarity

% ===
% Unit tests
% The expected value is not given to the predicate. Let it run, compare afterwards.
% ===

:- begin_tests(foo_foldr).

in([1,2,3,4,5]).

ffr(Foldy,List,Starter,AccUp) :- foo_foldr(Foldy,List,Starter,AccUp).

test(foo_foldr_len)    :- in(L),ffr(foldy_len     , L ,  0 , R), R=5.
test(foo_foldr_add)    :- in(L),ffr(foldy_add     , L ,  0 , R), R=15.
test(foo_foldr_mult)   :- in(L),ffr(foldy_mult    , L ,  1 , R), R=120.
test(foo_foldr_build)  :- in(L),ffr(foldy_build   , L , [] , R), R=[1,2,3,4,5].
test(foo_foldr_squadd) :- in(L),ffr(foldy_squadd  , L ,  0 , R), R=507425426245.
test(foo_foldr_join)   :- in(L),ffr(foldy_join    , L , "" , R), R="1,2,3,4,5".
test(foo_foldr_expr)   :- in(L),ffr(foldy_expr(*) , L ,  1 , R), R=1*(2*(3*(4*(5*1)))).

test(foo_foldr_len_empty)    :- ffr(foldy_len     , [],  0 , R), R=0.
test(foo_foldr_add_empty)    :- ffr(foldy_add     , [],  0 , R), R=0.
test(foo_foldr_mult_empty)   :- ffr(foldy_mult    , [],  1 , R), R=1.
test(foo_foldr_build_empty)  :- ffr(foldy_build   , [], [] , R), R=[].
test(foo_foldr_squadd_empty) :- ffr(foldy_squadd  , [],  0 , R), R=0.
test(foo_foldr_join_empty)   :- ffr(foldy_join    , [], "" , R), R="".
test(foo_foldr_expr_empty)   :- ffr(foldy_expr(*) , [],  1 , R), R=1.

% library(apply) has no "foldr" so no comparison tests!

:- end_tests(foo_foldr).

rt :- run_tests(foo_foldr).

/* RUN IT

?- [foldy],[foo_foldr].
true.

?- rt.
% PL-Unit: foo_foldr .............. done
% All 14 tests passed
true.

*/

