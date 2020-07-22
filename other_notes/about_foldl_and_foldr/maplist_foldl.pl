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
% linear foldl via maplist/4
% ("foldl" is the "Laughably recursive fold": subject to tail call optimization)
% ===
% maplist_foldl(+Foldy,+List,+Acc,?Result)
%
% Foldy   : Name of the predicate to call at each node of the list. That predicate takes:
%             - the node's item as first argument (called "Item" in the code);
%             - the valued obtained by the series of function application so far
%               (called "Acc" in the code);
%             - the result of the function application will be unified with the third argument
%               (called "Result" in the code).
% List      : List to traverse.
% Starter   : The Starter value. Also the initial Accumulator value. The Accumulator value
%             is threaded through two lists by maplist/4.
% Result    : The final value of the Accumulator will be unified with this argument at the
%             end of recursion.
% ===

% The empty list is handled separately

maplist_foldl(_,[],Starter,Result) :- 
   !,               % GREEN CUT. 
   Starter=Result.  % Unification in body instead of head for clarity

maplist_foldl(Foldy,List,Starter,Result) :-
   length(List,Len),
   Len >= 1,
   succ(OtherLen,Len),                  % OtherLen <- Len-1
   length(OtherList,OtherLen),          % Create a list of fresh variables of length OtherLen
   List1 = [Starter|OtherList],         % List1 is list of length Len, contains fresh variables expect Starter item
   append(OtherList,[Result],List2),    % List2 is list of length Len, contains fresh variables, last item is Result
   maplist(Foldy,List,List1,List2).     % Call maplist/4 which constructs goals like Foldy(i1,i2,i3) and calls them

% ===
% Unit tests
% The expected value is not given to the predicate. Let it run, compare afterwards.
% ===

:- begin_tests(maplist_foldl).

in([1,2,3,4,5]).

mfl(Foldy,List,Starter,Result) :- maplist_foldl(Foldy,List,Starter,Result).

test(maplist_foldl_len)    :- in(L),mfl(foldy_len     , L ,  0 , R), R=5.
test(maplist_foldl_add)    :- in(L),mfl(foldy_add     , L,   0 , R), R=15.
test(maplist_foldl_mult)   :- in(L),mfl(foldy_mult    , L,   1 , R), R=120.
test(maplist_foldl_build)  :- in(L),mfl(foldy_build   , L,  [] , R), R=[5,4,3,2,1].
test(maplist_foldl_squadd) :- in(L),mfl(foldy_squadd  , L,   0 , R), R=21909.
test(maplist_foldl_join)   :- in(L),mfl(foldy_join    , L,  "" , R), R="5,4,3,2,1".
test(maplist_foldl_expr)   :- in(L),mfl(foldy_expr(*) , L,   1 , R), R=5*(4*(3*(2*(1*1)))).

test(maplist_foldl_len_empty)    :- mfl(foldy_len     , [],  0 , R), R=0.
test(maplist_foldl_add_empty)    :- mfl(foldy_add     , [],  0 , R), R=0.
test(maplist_foldl_mult_empty)   :- mfl(foldy_mult    , [],  1 , R), R=1.
test(maplist_foldl_build_empty)  :- mfl(foldy_build   , [], [] , R), R=[].
test(maplist_foldl_squadd_empty) :- mfl(foldy_squadd  , [],  0 , R), R=0.
test(maplist_foldl_join_empty)   :- mfl(foldy_join    , [], "" , R), R="".
test(maplist_foldl_expr_empty)   :- mfl(foldy_expr(*) , [],  1 , R), R=1.

:- end_tests(maplist_foldl).

rt :- run_tests(maplist_foldl).

% How to run:
%
% ?- [foldy],[maplist_foldl].
% true.
%
% ?- rt.
% % PL-Unit: maplist_foldl .............. done
% % All 14 tests passed
% true.


