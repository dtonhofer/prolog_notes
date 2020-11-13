% Playing around with a booby-trapped "iterator" implementation
% 
% The predicate-to-call takes a list, the elements of which are then 
% "generated" (i.e. emitted) one-by-one by an "iterator" and printed
% to stdout by the with_write/1 "master". However, certain elements
% make the from_list/1 goal behave in unruly ways.
% 
% Change the contents of the list passed to `run/1` to elicit some 
% occurrences of interest from `reset/3`:

/* 
?- run([a,b,c]).
% Iterator yielded a
% Iterator yielded b
% Iterator yielded c
% Iterator succeeded
true.

?- run([a,fail,c]).
% Iterator yielded a
false.

?- run([a,throw,c]).
% Iterator yielded a
ERROR: Domain error: `this' expected, found `that'

?- run([a,badshift,c]).
% Iterator yielded a
ERROR: reset/3 `bad(badshift)' does not exist

?- run([a,recur,c]).
% Iterator yielded a
% Iterator yielded sub1
% Iterator yielded sub2
% Iterator yielded sub3
% Iterator succeeded
% Iterator yielded c
% Iterator succeeded
true.
*/

:- debug(iterator).

% ==
% Main predicate, call from toplevel
% ==

run(L) :-
   must_be(list,L),
   with_write(from_list(L)).

% ===
% The "master predicate": It writes the values yielded by the "iterator predicate" 
% (represented by "Goal") to stdout.
% ===

with_write(Goal) :-
  reset(Goal,yield(X),Cont),
  branch(Cont,yield(X)).

branch(0,_) :- !,
   debug(iterator,"Iterator succeeded",[]).

branch(Cont,yield(X)) :-
   debug(iterator,"Iterator yielded ~q",[X]),
   with_write(Cont).

% ===
% The "iterator predicate", generating successive values (read from a list in
% this case) that are communicated to the master predicate by a call to shift/1.
% However, the "iterator" behaves in specific ways depending on the
% element encountered because ... we want to see what happens next!!
% ==

from_list([X|Xs]) :-
   ((X == fail; X == false)         % elicit failure
   -> fail
   ;
   (X == throw)                     % elicit exception
   -> domain_error(this,that)
   ;
   (X == badshift)                  % elicit shift not unifiable by reset/3 call
   -> shift(bad(X))
   ;
   (X == recur)                     % elicit matrioshka reset/3 call
   -> with_write(from_list([sub1,sub2,sub3]))
   ;
   shift(yield(X))),                % bravely default
   from_list(Xs).                   % tail recursive loop

from_list([]).


