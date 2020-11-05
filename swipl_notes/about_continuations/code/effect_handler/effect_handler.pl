% =============================================================================
% "Effect Handlers" using Delimited Continuations
% =============================================================================
% Code inspired by:
%
%  "Delimited Continuations for Prolog" (2013)
%  Tom Schrijvers, Bart Demoen, Benoit Desouter, Jan Wielemaker
%  https://www.swi-prolog.org/download/publications/iclp2013.pdf
%  (the "effect handler" example is on page 2)
% =============================================================================

% ~~~~~~
% Effect handler expressing the "state monad", i.e. a series of state changes
% by functions which receive a "monad value" describing a state t and from that
% create a new "monad value" describing a state t+1.
%
% At page 2 of the cited paper, we read [text below has been adapted]:
%
% Plotkin and Pretnar (Plotkin, G and Pretnar, M, 2009: "Handlers of Algebraic
% effects". In ESOP '09) have recently formulated a particularly insightful
% class of applications: "effect handlers". Effect handlers are an elegant way
% to add many kinds of side-effectful operations to language and far less
% intrusive than monads (Moggi, E. 1991: "Notions of computation and monads".
% Inf. Comput. 93, 1). [Other authors might disagree]
%
% Implicit State Passing. [Example] defines an effect handler for an implicit
% state passing feature. The feature provides two primitive operations:
% [get_state/1] for reading the implicit state, and [put_state/1] for writing
% it. [...]
%
% The effect handler decouples the syntax of the new operations from their
% semantics. The put_state/1 and get_state/1 predicates are all syntax and no
% semantics; they simply _shift_ their own term representation [communicating
% with the effect handler predicate which holds the current state representation
% in the current activation, which is is replaced by a recursive call].
% The semantics is supplied by the handler predicate run_state/3. This handler
% predicate runs a goal and interprets the two primitive operations whenever
% they are shifted. For the interpretation, run_state recursively "threads
% a state".
% ~~~~~~

% We pack the "effect handler" into a module for reuse

:- module(effect_handler,
          [ get_state/1
           ,put_state/1
           ,with_state/3 ]).

% Debugging output is on by default

:- debug(with_state).

% ===
% The put_state/1 and get_state/1 predicates are "all syntax and no semantics".
% They simply "shift" their own term representations, thus passing execution
% stream to the predicate run_state/3.
% ===

get_state(S) :- shift(get_state(S)).
put_state(S) :- shift(put_state(S)).

% ===
% The effect handler predicate, running a tail-recursive loop, threading state.
%
% Note the cut on the second clause of branch/2 to make branch/2 deterministic
% (there is no indexing on the second argument, so this is needed). Without the
% cut, you may well run out of global stack!
% ===

with_state(Goal,StateCur,StateOut) :-
   debug(with_state,"Effect handler: calling reset/3",[]),
   reset(Goal,Cmd,Cont),                     % "Cmd" is the command term: get_state/1 or put_state/1
   tag_cont(Cont,TaggedCont),                % This just tags the continuation term "Cont".
   branch(TaggedCont,Cmd,StateCur,StateOut). % This will result in success or a tail-recursive call.

branch(zero,_,State,State) :-
   debug(with_state,"Effect handler: continuation is 0; goal succeeded",[]).

branch(cont(Cont),get_state(StateCur),StateCur,StateOut) :-
   !,
   debug(with_state,"Effect handler: return from reset/3 with get_state/1. Unifying command variable with ~q",[StateCur]),
   with_state(Cont,StateCur,StateOut).

branch(cont(Cont),put_state(StateNew),_,StateOut) :-
   debug(with_state,"Effect handler: return from reset/3 with put_state/1. Received ~q from command variable",[StateNew]),
   with_state(Cont,StateNew,StateOut).

% ===
% Tag the continuation "Cont" for easy pattern-directed branching.
% "zero" vs. "cont(_)" is better to make a branching descision than
% "0" vs "some unknown structure that can be used as a goal"
% ===

tag_cont(0,zero) :- !.
tag_cont(Cont,cont(Cont)).

