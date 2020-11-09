% ===
% The "master coroutine": It writes the values yielded by the "iterator"
% represented by "Goal" to stdout.
% ===

with_write(Goal) :-
  reset(Goal,yield(X),Cont),
  branch(Cont,yield(X)).

branch(0,_) :- !,
   format("Iterator succeeded\n").

branch(Cont,yield(X)) :- 
   format("Iterator yielded ~q\n",[X]),
   with_write(Cont).

% ===
% The "iterator", generating successive values (read from a list in
% this case) that are communicated to the master coroutine by a call
% to shift/1.
% However, the "iterator" behaves in peculiar ways depending on the
% element encountered because we want to see what happens exactly.
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

% ==
% Main predicate, call from toplevel
% ==

run(L) :-
   must_be(list,L),
   with_write(from_list(L)).

