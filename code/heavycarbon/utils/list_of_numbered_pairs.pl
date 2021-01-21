:- module(heavycarbon_list_of_numbered_pairs,
   [
   list_of_numbered_pairs/2  % -Pairs,+Length
   ]).

% A quick hack working in one direction for now.
%
% ?- list_of_numbered_pairs(3,N).
% N = [0-_11910, 1-_11898, 2-_11886].

list_of_numbered_pairs(Pairs,Length) :-
   var(Pairs),
   !,
   must_be(nonneg,Length), % also checked in length/2
   length(Pairs,Length),   % create list Pairs of length "Length", possibly the empty list
   ((Length>0)             % fill with pairs, numbered with 0..Length-1 on the pairs' first argument
    -> (succ(Max,Length),
        findall(N-_,between(0,Max,N),Pairs))
    ;  Pairs=[]).

