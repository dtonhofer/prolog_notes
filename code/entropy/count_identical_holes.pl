% ===
% Count the number of "holes" ("uninstantiated variables" or rather "fresh terms") in a
% list and set up a list of pairs (X-Count) where "Count" expresses how many times 
% variable "X" appears. When printed out "X" appears with a "default name" like "_67771".
% ===

count_identical_holes_in_list(Ls,Pairs) :-
   count_identical_holes_in_list_with_accum(Ls,[],PairsUnsorted),
   keysort(PairsUnsorted,Pairs).

% ===
% Count the number of "holes" ("uninstantiated variables" or rather "fresh terms") in a
% tree instead of just a list. This is a straightforward generalization of 
% count_identical_holes_in_list(Ls,Pairs)
% ===

count_identical_holes_in_tree(Root,Pairs) :-
   count_identical_holes_in_tree_with_accum(Root,[],PairsUnsorted),
   keysort(PairsUnsorted,Pairs).
      
% ===
% Helper: Traversing the tree given by "Root" 
% If Root is a dict, we are fine, because compound_name_arguments/3
% disassembles it correctly.
% "List" is completely covered by "compound term with arity 2"
% ===

count_identical_holes_in_tree_with_accum(Root,Psin,[(Root-CountPlus)|Psremovaled]) :-
   var(Root),!,
   lookup_pair_using_term_equality(Root,Psin,Psremovaled,Count),
   succ(Count,CountPlus).      
   
count_identical_holes_in_tree_with_accum(Root,Psin,Psout) :-
   nonvar(Root),compound(Root),!,
   compound_name_arguments(Root,Functor,Args), 
   assertion(atomic(Functor)),    
   count_identical_holes_over_arglist(Args,Psin,Psout).

count_identical_holes_in_tree_with_accum(Root,Pairs,Pairs) :-
   nonvar(Root),atomic(Root).
   
% ===
% Traversing the arglist of a compound term, which cannot just be
% handled as a tree
% ===
   
count_identical_holes_over_arglist([A|Args],Psin,Psout) :-
   count_identical_holes_in_tree_with_accum(A,Psin,Pmid),
   count_identical_holes_over_arglist(Args,Pmid,Psout).

count_identical_holes_over_arglist([],Pairs,Pairs).
   
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

   test(list_zero_1,[ true(T) ]) :- 
      Ls = [], 
      count_identical_holes_in_list(Ls,Pairs),
      T = (Pairs == []).

   test(list_zero_2,[ true(T) ]) :- 
      Ls = [a,b,c], 
      count_identical_holes_in_list(Ls,Pairs),
      T = (Pairs == []).
      
   test(list_var_1,[ true(T) ]) :- 
      Ls = [a,a,X,a,Y,Z,X,a], 
      count_identical_holes_in_list(Ls,Pairs),
      debug(ch,"Pairs for list of: 2 x 'X', 1 x 'Y', 1 x 'Z': ~q",[Pairs]),
      lookup_pair_using_term_equality(X,Pairs,_,XCount),
      lookup_pair_using_term_equality(Y,Pairs,_,YCount),
      lookup_pair_using_term_equality(Z,Pairs,_,ZCount),      
      T = ( [XCount,YCount,ZCount] == [2,1,1] ).

   test(list_var_2,[ true(T) ]) :- 
      Ls = [X,X,X,X,X,X,X,X,Y,Y,Y,Y,Y,Y,Y,Y], 
      count_identical_holes_in_list(Ls,Pairs),
      debug(ch,"Pairs for list of: 8 x 'X', 8 x 'Y': ~q",[Pairs]),
      lookup_pair_using_term_equality(X ,Pairs,_,XCount),
      lookup_pair_using_term_equality(Y ,Pairs,_,YCount),
      lookup_pair_using_term_equality(_Z,Pairs,_,ZCount),
      T = ( [XCount,YCount,ZCount] == [8,8,0] ).

   test(tree_zero_1,[ true(T) ]) :- 
      count_identical_holes_in_tree(Root,Pairs),
      debug(ch,"Pairs for tree of just 'Root': ~q",[Pairs]),
      T = ( Pairs == [(Root-1)] ).

   test(tree_list,[ true(T) ]) :- 
      Root = [X,X,X,X,X,X,X,X,Y,Y,Y,Y,Y,Y,Y,Y], 
      count_identical_holes_in_tree(Root,Pairs),
      debug(ch,"Pairs for tree that is a list of: 8 x 'X', 8 x 'Y': ~q",[Pairs]),
      lookup_pair_using_term_equality(X,Pairs,_,XCount),
      lookup_pair_using_term_equality(Y,Pairs,_,YCount),
      T = ( [XCount,YCount] == [8,8] ).

   test(tree_list,[ true(T) ]) :- 
      Root = f(X,g(X,Y,Z,Z,Y),h(X)),
      count_identical_holes_in_tree(Root,Pairs),
      debug(ch,"Pairs for tree that is a term: f(X,g(X,Y,Z,Z,Y),h(X)): ~q",[Pairs]),
      lookup_pair_using_term_equality(X,Pairs,_,XCount),
      lookup_pair_using_term_equality(Y,Pairs,_,YCount),
      lookup_pair_using_term_equality(Z,Pairs,_,ZCount),
      T = ( [XCount,YCount,ZCount] == [3,2,2] ).
      
   test(difflist,[ true(T) ]) :- 
      DiffList = [1,2,3,4,5,6|T]-T,
      count_identical_holes_in_tree(DiffList,Pairs),
      debug(ch,"Pairs for tree that is a term: [1,2,3,4,5,6|T]-T: ~q",[Pairs]),
      lookup_pair_using_term_equality(T,Pairs,_,TCount),
      T = ( [TCount] == [2] ).
      
      
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
