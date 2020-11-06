with_write(IteratorGoal) :-
  reset(IteratorGoal,yield(X),IteratorCont),
  tag_cont(IteratorCont,TaggedCont),
  branch(TaggedCont,yield(X)).

branch(zero,_) :-
   format("Iterator succeeded\n").

branch(cont(IteratorCont),yield(X)) :-
   format("Iterator yielded ~q\n",[X]),
   with_write(IteratorCont).

% helper to tag a continuation

tag_cont(0,zero) :- !.
tag_cont(Cont,cont(Cont)).

% the iterator, generating successive values that are communicated
% to the manager by a call to shift/1

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

% main predicate, call from toplevel

run(L) :-
   must_be(list,L),
   with_write(from_list(L)).

