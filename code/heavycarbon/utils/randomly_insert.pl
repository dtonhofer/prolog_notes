:- module(heavycarbon_randomly_insert,
          [
             randomly_insert/3   % randomly_insert(@Element,+List,-ListNew)
          ]).

% ===
% Insert "Element" at a random position in "List", yielding "ListNew".
% ===

randomly_insert(Element,List,ListNew) :-
   must_be(list,List),
   length(List,L),
   random_between(0,L,Pos),                     % If L is 0, Pos will be necessarily 0
   length(Prefix,Pos),                          % *Creates* a list of "Pos" fresh variables
   append(Prefix,Suffix,List),                  % Run append/3 in reverse, splitting List (one solution only)
   append([Prefix,[Element],Suffix],ListNew).   % Run append/2 forward, unify with ListNew
