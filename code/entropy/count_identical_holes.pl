% ===
% Count the number of "holes" ("uninstantiated variables" or rather "fresh terms") in a
% list and set up a list of pairs:
% (X-Count) where "Count" expresses how many times variable "X" appears, which, when
% printed out, is given a "default name" like "_67771".
% ===

count_identical_holes_in_list(Ls,Pairs) :-
   count_identical_holes_in_list_with_accum(Ls,[],PairsUnsorted),
   keysort(PairsUnsorted,Pairs).

% ===
% Helper: Iterating/Inducting through the "Pairs" list
% ===

count_identical_holes_in_list_with_accum([],Pairs,Pairs) :- !.

count_identical_holes_in_list_with_accum([L|Ls],Psin,Psout) :-
   nonvar(L),!,
   count_identical_holes_in_list_with_accum(Ls,Psin,Psout).
   
count_identical_holes_in_list_with_accum([L|Ls],Psin,Psout) :-
   var(L),!,
   lookup_pair_using_term_equality(L,Psin,Psremovaled,Count),
   succ(Count,CountPlus),
   count_identical_holes_in_list_with_accum(Ls,[(L-CountPlus)|Psremovaled],Psout).

% ===    
% Helper: 
% lookup_pair_using_term_equality(+Var,+Pairs,-PairsWithouLookedUpPair,-CountOfPair)
% Deterministically succeeds, but returns 0 if there is no entry for "Var"
% ===

lookup_pair_using_term_equality(L,[(V-Count)|Ps],Ps,Count) :-
   assertion(var(L)),L == V,!.
      
lookup_pair_using_term_equality(L,[(V-C)|Ps],[(V-C)|Pss],Count) :-
   assertion(var(L)),L \== V,!, 
   lookup_pair_using_term_equality(L,Ps,Pss,Count).

lookup_pair_using_term_equality(_,[],[],0).
   
% ===
% testing
% ===

:- debug(ch).

:- begin_tests(count_holes).

   test(t_zero_1,[ true(T) ]) :- 
      Ls = [], 
      count_identical_holes_in_list(Ls,Pairs),
      T = (Pairs == []).

   test(t_zero_2,[ true(T) ]) :- 
      Ls = [a,b,c], 
      count_identical_holes_in_list(Ls,Pairs),
      T = (Pairs == []).
      
   test(t_var_1,[ true(T) ]) :- 
      Ls = [a,a,X,a,Y,Z,X,a], 
      count_identical_holes_in_list(Ls,Pairs),
      debug(ch,"Pairs for 2 x X, 1 x Y, 1 x Z: ~q",[Pairs]),
      lookup_pair_using_term_equality(X,Pairs,_,XCount),
      lookup_pair_using_term_equality(Y,Pairs,_,YCount),
      lookup_pair_using_term_equality(Z,Pairs,_,ZCount),      
      T = ( [XCount,YCount,ZCount] == [2,1,1] ).

   test(t_var_2,[ true(T) ]) :- 
      Ls = [X,X,X,X,X,X,X,X,Y,Y,Y,Y,Y,Y,Y,Y], 
      count_identical_holes_in_list(Ls,Pairs),
      debug(ch,"Pairs for 8 x X, 8 x Y: ~q",[Pairs]),
      lookup_pair_using_term_equality(X ,Pairs,_,XCount),
      lookup_pair_using_term_equality(Y ,Pairs,_,YCount),
      lookup_pair_using_term_equality(_Z,Pairs,_,ZCount),
      T = ( [XCount,YCount,ZCount] == [8,8,0] ).
      
:- end_tests(count_holes).

rt(_) :- run_tests(count_holes).

% ===
% example run
% ===

/*
?- rt(_).
% PL-Unit: count_holes ..
% Pairs for 2 x X, 1 x Y, 1 x Z: [_11702-2,_11714-1,_11720-1]
.
% Pairs for 8 x X, 8 x Y: [_12560-8,_12608-8]
. done
% All 4 tests passed
true.
*/
