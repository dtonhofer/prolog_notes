% ============================================================================
% Counting the occurence of atomic terms in a list.
% ============================================================================
% Load module and run tests with:
% 
% ?- use_module(library('snippets/counted.pl')).
% true.
% 
% ?- load_test_files([]).
% true.
% 
% ?- run_tests.
% ============================================================================

:- module(snippets_counted,
          [
          counted/3
          ]).

% ===         
% "Items" is a list of atomic (and thus ground) terms
% that will appear as keys in an SWI-Prolog dict.
%
% "CountsDict" is an SWI-Prolog dict mapping every
% "Item" encountered in "Items" to its occurrence count.
% The tag of "CountsDict" is (unified with) "Tag".
%
% counted(+Items,-CountsDict,+Tag)
% ===

counted(Items,CountsDict,Tag) :-
   dict_create(EmptyDict,Tag,[]),
   foldl(inc_for_key,Items,EmptyDict,CountsDict).

% ---
% Private helper
% ---

inc_for_key(Item,DictIn,DictOut) :-
   assertion(atomic(Item)),     % lookup in a dict works via unification of atomic terms; make sure it's atomic
   (get_dict(Item,DictIn,Count) % how efficient is lookup in a dict (is it a linear scan for an unifiying element?)
    ->
    succ(Count,CountNext)
    ;
    CountNext=1),
   put_dict(Item,DictIn,CountNext,DictOut).


