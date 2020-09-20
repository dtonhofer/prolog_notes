:- module(heavycarbon_in_prefix, 
   [
    in_prefix/2                % in_prefix(Element,ListOrOpenList)
   ,in_prefix_with_stoplist/3  % in_prefix(Element,StopList,ListOrOpenList)
   ]).

% ---
% Is some element (not necessarily an atom) in a prefix of a proper list or 
% an open list, where the list possibly contains variables that should not be
% unified?
%
% No unfications are made. Disjoint unbound variables are considered
% "not the same". An unbound variable X is in prefix of L iff
% X indeed occurs in L.
%
% Fails if the second argument is not actually a list, does not throw.
%
% *** Useful for looking for options in options lists ***
%
% ---
%
% This is not a logic-releated predicate: The question is not about
% logic-related objects ("for what variable values is this true"), but about
% computation-related objects ("for these computation-related objects, 
% is it true that ... ")
%
% Compare:
%
%    ?- in_prefix(foo,[1,2,X,Y]).
%    false.
% 
%    ?- member(foo,[1,2,X,Y]).
%    X = foo ;
%    Y = foo.
%
%    ?- memberchk(foo,[1,2,X,Y]).
%    X = foo.
%
% And:
%
%    ?- in_prefix(K,[1,2,X,Y]).
%    false.
% 
%    ?- member(K,[1,2,X,Y]).
%    K = 1 ;
%    K = 2 ;
%    K = X ;
%    K = Y.
%
%    ?- memberchk(K,[1,2,X,Y]).
%    K = 1.
% 
% And:
%
%   ?- in_prefix(K,[1,2|X]).
%   false.
%
%   ?- member(K,[1,2|X]).
%   K = 1 ;
%   K = 2 ;
%   X = [K|_6726] ;
%   X = [_6724, K|_7544] ;
%   X = [_6724, _7542, K|_8362] ;
%   X = [_6724, _7542, _8360, K|_9180] .
%
%   ?- memberchk(K,[1,2|X]).
%   K = 1.
% ---

in_prefix(_,[]) :-
   !,false.

in_prefix(_,L) :-
   var(L),!,false.

in_prefix(X,[Y|_]) :-
   X==Y,!,true.

in_prefix(X,[Y|L]) :-
   X\==Y,!,in_prefix(X,L).

% ---
% Look for Element in the prefix of ListOrOpenList but also stop at the
% first occurrent of an element from StopList.
%
% *** Useful for looking for options in options lists where the first
%     element from the stoplist wins ***
% ---

in_prefix_with_stoplist(_,_,[]) :-
   !,false.

in_prefix_with_stoplist(_,_,L) :-
   var(L),!,false.

in_prefix_with_stoplist(X,_,[Y|_]) :-
   X==Y,!,true.

in_prefix_with_stoplist(X,SL,[Y|_]) :-
   X\==Y,
   in_prefix(Y,SL),
   !,
   false.

in_prefix_with_stoplist(X,SL,[Y|L]) :-
   X\==Y,
   \+in_prefix(Y,SL),
   !,
   in_prefix_with_stoplist(X,SL,L).

