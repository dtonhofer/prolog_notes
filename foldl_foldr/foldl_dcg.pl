   
% Wrap the call to DCG processing

foldl_dcg(Foldy,List,Starter,Result) :-
   phrase(recognizer(Foldy,Starter,Result),List).

% Whenever there is an "Item" in the list, first call Foldy, then recurse.

recognizer(Foldy,ThreadIn,ThreadOut) --> 
   [Item],
   !,                                         % GREEN CUT
   { call(Foldy,Item,ThreadIn,Intermed) },     
   recognizer(Foldy,Intermed,ThreadOut).

% When there is nothing in the list, "bounce the accumulator" by unifying
% "ThreadIn" with "ThreadOut", i.e. with "Result"

recognizer(_Foldy,ThreadEnd,ThreadEnd) --> [].

% ===
% Unit tests
% The expected value is not given to the predicate. Let it run, compare afterwards.
% ===

:- begin_tests(foldl_dcg).

test(foldl_dcg_add)    :- foldl_dcg(foldy_add     , [1,2,3,4,5],  0 , Out), Out=15.
test(foldl_dcg_mult)   :- foldl_dcg(foldy_mult    , [1,2,3,4,5],  1 , Out), Out=120.
test(foldl_dcg_build)  :- foldl_dcg(foldy_build   , [1,2,3,4,5], [] , Out), Out=[5,4,3,2,1].
test(foldl_dcg_squadd) :- foldl_dcg(foldy_squadd  , [1,2,3,4,5],  0 , Out), Out=21909.
test(foldl_dcg_join)   :- foldl_dcg(foldy_join    , [1,2,3,4,5], "" , Out), Out="5,4,3,2,1".
test(foldl_dcg_expr)   :- foldl_dcg(foldy_expr(*) , [1,2,3,4,5],  1 , Out), Out=5*(4*(3*(2*(1*1)))).

test(foldl_dcg_add_empty)    :- foldl_dcg(foldy_add     , [],  0 , Out), Out=0.
test(foldl_dcg_mult_empty)   :- foldl_dcg(foldy_mult    , [],  1 , Out), Out=1.
test(foldl_dcg_build_empty)  :- foldl_dcg(foldy_build   , [], [] , Out), Out=[].
test(foldl_dcg_squadd_empty) :- foldl_dcg(foldy_squadd  , [],  0 , Out), Out=0.
test(foldl_dcg_join_empty)   :- foldl_dcg(foldy_join    , [], "" , Out), Out="".
test(foldl_dcg_expr_empty)   :- foldl_dcg(foldy_expr(*) , [],  1 , Out), Out=1.

:- end_tests(foldl_dcg).

rt :- run_tests(foldl_dcg).
