:- module(replace0,
          [
             replace0/5    % replace0(List, N, NewItem, OldItem, NewList)
          ]).

% ===
% replace0/5 is simply based on splinter0/5
%
% "Replace operation at 0-based position "N" in "List" with "NewItem", 
%  gives "NewList", with the old item in "OldItem"
% ===

replace0(List, N, NewItem, OldItem, NewList) :-
   splinter0(List, N, Prefix, OldItem, Suffix),
   append([Prefix, [NewItem], Suffix], NewList).


