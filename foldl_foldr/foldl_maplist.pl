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
% foldl_maplist(+Foldy,+List,+Starter,?Out)
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

foldl_maplist(_,[],Starter,Starter) :- !. % GREEN CUT

foldl_maplist(Foldy,List,Starter,Out) :-
   length(List,Len),                    % Len >= 1
   succ(OtherLen,Len),                  % OtherLen <- Len-1
   length(OtherList,OtherLen),          % create a list of fresh variables
   List1 = [Starter|OtherList],         % List of length Len, fresh variables expect Starter item
   append(OtherList,[Out],List2),       % List of length Len, fresh variables, last item is Out
   maplist(Foldy,List,List1,List2).     % Call maplist/4 which constructs goals like Foldy(i1,i2,i3) and calls them

% ===
% Unit tests
% The expected value is not given to the predicate. Let it run, compare afterwards.
% ===

:- begin_tests(foldl_maplist).

test(foldl_add)    :- foldl_maplist(foldy_add     , [1,2,3,4,5] ,  0 , Out), Out=15.
test(foldl_mult)   :- foldl_maplist(foldy_mult    , [1,2,3,4,5] ,  1 , Out), Out=120.
test(foldl_build)  :- foldl_maplist(foldy_build   , [1,2,3,4,5] , [] , Out), Out=[5,4,3,2,1].
test(foldl_squadd) :- foldl_maplist(foldy_squadd  , [1,2,3,4,5] ,  0 , Out), Out=21909.
test(foldl_join)   :- foldl_maplist(foldy_join    , [1,2,3,4,5] , "" , Out), Out="5,4,3,2,1".
test(foldl_expr)   :- foldl_maplist(foldy_expr(*) , [1,2,3,4,5] ,  1 , Out), Out=5*(4*(3*(2*(1*1)))).

test(foldl_add_empty)    :- foldl_maplist(foldy_add     , [],  0 , Out), Out=0.
test(foldl_mult_empty)   :- foldl_maplist(foldy_mult    , [],  1 , Out), Out=1.
test(foldl_build_empty)  :- foldl_maplist(foldy_build   , [], [] , Out), Out=[].
test(foldl_squadd_empty) :- foldl_maplist(foldy_squadd  , [],  0 , Out), Out=0.
test(foldl_join_empty)   :- foldl_maplist(foldy_join    , [], "" , Out), Out="".
test(foldl_expr_empty)   :- foldl_maplist(foldy_expr(*) , [],  1 , Out), Out=1.

% Compare with the results of foldl/4 from "library(apply)"

test(lib_foldl_add)    :- foldl(foldy_add     , [1,2,3,4,5] ,  0 , Out), Out=15.
test(lib_foldl_mult)   :- foldl(foldy_mult    , [1,2,3,4,5] ,  1 , Out), Out=120.
test(lib_foldl_build)  :- foldl(foldy_build   , [1,2,3,4,5] , [] , Out), Out=[5,4,3,2,1].
test(lib_foldl_squadd) :- foldl(foldy_squadd  , [1,2,3,4,5] ,  0 , Out), Out=21909.
test(lib_foldl_join)   :- foldl(foldy_join    , [1,2,3,4,5] , "" , Out), Out="5,4,3,2,1".
test(lib_foldl_expr)   :- foldl(foldy_expr(*) , [1,2,3,4,5] ,  1 , Out), Out=5*(4*(3*(2*(1*1)))).

test(lib_foldl_add_empty)    :- foldl_maplist(foldy_add   , [] ,  0 , Out), Out=0.
test(lib_foldl_mult_empty)   :- foldl_maplist(foldy_mult  , [] ,  1 , Out), Out=1.
test(lib_foldl_build_empty)  :- foldl_maplist(foldy_build , [] , [] , Out), Out=[].
test(lib_foldl_squadd_empty) :- foldl_maplist(foldy_build , [] ,  0 , Out), Out=0.
test(lib_foldl_join_empty)   :- foldl_maplist(foldy_join  , [] , "" , Out), Out="".
test(lib_foldl_expr_empty)   :- foldl_maplist(foldy_join  , [] ,  1 , Out), Out=1.

:- end_tests(foldl_maplist).

rt :- run_tests(foldl_maplist).



