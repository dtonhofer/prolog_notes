% ===
% Prolog coroutines using Delimited Continuations
%
% Code inspired by "Delimited Continuations for Prolog" Schrijvers et al., 2013, page 4
% ===
%
% Coroutines that suspend to output data are called *iterators*.
% They are created by *generators* that use the *yield* keyword to suspend and
% return an intermediate value before continuing with the generation of more values.
% In a sense yield/1 generalizes Prolog's write/1 built-in: the coroutine runs
% in a context that consumes its output in a _user-defined_ way.

% ===
% Iterator
% ===

% Helper: Tag "Cont" for pattern-directed branching.
% - "zero vs cont(_)" is better than
% - "0 vs some unknown structure that can be used as a goal"

tag_cont(0,zero) :- !.
tag_cont(Cont,cont(Cont)).

% Write every payload yielded by Goal to STDOUT

with_write(Goal) :-
  reset(Goal,Loot,Cont),         % Loot is the full term yieled, should be "yield(X)"
  tag_cont(Cont,TgCont),         % this just tags the continutation "Cont" into "TgCont" for ease-of-matching
  branch(TgCont,Loot).

branch(zero,_).                  % successful "end of loop" because Goal succeeded
branch(cont(Cont),yield(X)) :- 
   write(X),write('.'),          % output yielded X
   with_write(Cont).             % tail-recursively loop around into a new context where Cont is the new Goal

% ===
% Two example generators which call yield(X) for each value generated until success
% ===

% ---
% "yield/1" is just a "shift" of the "command term" carrying the value that shall be output
% ---

yield(X) :- shift(yield(X)).     % a "logical interpretation" of this is hard to find...

% ---
% predicate which yields the values from a list
% ---

from_list([X|Xs]) :-
   !,
   yield(X),                     % instead of write/1
   from_list(Xs).

from_list([]).

% ---
% predicate which yields the values [L..U-1], L>=0
% ---

from_interval(L,U) :- 
   L<U,!,
   yield(L),                     % instead of write/1
   succ(L,Lp),
   from_interval(Lp,U).

from_interval(X,X).

% ===
% Run it
% ===

run(one) :- with_write(from_interval(0,10)).
run(two) :- with_write(from_list([a,b,c,d,e,f])).
