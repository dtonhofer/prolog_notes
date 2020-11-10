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
% Effect handler expressing the "state monad", i.e. a sequence of state changes
% through functions which receive a "monad value" describing a state at step t
% and from that create a new "monad value" describing a state at step t+1.
%
% At page 2 of the cited paper, we read [text below has been adapted]:
%
% Plotkin and Pretnar (Plotkin, G and Pretnar, M, 2009: "Handlers of Algebraic
% effects". In ESOP '09) have recently formulated a particularly insightful
% class of applications: "effect handlers". Effect handlers are an elegant way
% to add many kinds of side-effectful operations to language and far less
% intrusive than monads (Moggi, E. 1991: "Notions of computation and monads".
% Inf. Comput. 93, 1). 
%
% [Other authors might disagree about whether monads are less intrusive]
%
% Implicit State Passing. [Example] defines an effect handler for an implicit
% state passing feature. The feature provides two primitive operations:
% [get_state/1] for reading the implicit state, and [put_state/1] for writing
% it. 
%
% [...]
%
% The effect handler decouples the syntax of the new operations from their
% semantics. The put_state/1 and get_state/1 predicates are all syntax and no
% semantics; they simply _shift_ their own term representation. 
%
% [...communicating with the effect handler which always holds the currently
% valid state representation. The state representation is updated naturally 
% by change the context of the effect handler via a tail-recursive call]
%
% The semantics is supplied by the handler predicate run_state/3. This handler
% predicate runs a goal and interprets the two primitive operations whenever
% they are shifted. For the interpretation, run_state recursively "threads
% a state".
%
% [...but there seems to be a problem: the client code to which the effect 
% handler switches via reset/3 must be unique - one cannot generalize to 
% several client code routines. The client can always push state to the effect
% handler via put_state/1 but for get_state/1 to work, there must be certainty
% that the S in get_state(S) is still valid when execution flow switches back 
% to the requesting routine via reset/3 - however if there are more than 
% one routine, arbitrary put_state/1 may happen before that reset/3, and S
% may be very stale when the execution flow switches back, having long been
% replaced by fresher S. The only way to be sure to "get the latest state"
% seems to be to use "global variables" with nb_setval/2, nb_getval/2
% (https://eu.swi-prolog.org/pldoc/man?section=gvar) or to have every routine
% keep a variable to an open list to which a representation of the latest 
% state is appended on each put_state/1, and thus can be found.]
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
   reset(Goal,Cmd,Cont),               % "Cmd" is the command term: get_state/1 or put_state/1
   branch(Cont,Cmd,StateCur,StateOut). % This will result in success or a tail-recursive call.

% A continuation equal to the number 0 means the goal called by reset/3 succeeded. 
% We don't bother with backtracking into that goal (there is nothing anyway).
% And thus we have with_state/3 succeed, too. The final "StateOut" is unified with "StateIn".
% The "!" commits to the case of "Cont" == 0 (so no need to check "Cont" \== 0 below).
 
branch(0,_,State,State) :- !,
   debug(with_state,"Effect handler: continuation is 0; goal succeeded",[]).

% The "Cmd" received unifies with get_state(X). The X is an unbound variable
% held by the client code. Unifying it with the current state thus communicates
% the current state to the client code.
% After that, perform a tail-recursive loop that does not change the state
% of the new with_state/3 activation.
% The "!" makes sure the choicepoint is dropped, which is vital for long-running
% programs: the Prolog Processor can clear resources after the recursive call.

branch(Cont,get_state(StateCur),StateCur,StateOut) :- !,
   debug(with_state,"Effect handler: return from reset/3 with get_state/1. Unifying command variable with ~q",[StateCur]),
   with_state(Cont,StateCur,StateOut).

% The "Cmd" received unifies with put_state(X). The X is an bound variable
% holding the new intended state communicated to the effect handler by the client code. 
% After that, perform a tail-recursive loop that changes the state
% of the new with_state/3 activation to the received state.

branch(Cont,put_state(StateNew),_,StateOut) :-
   debug(with_state,"Effect handler: return from reset/3 with put_state/1. Received ~q from command variable",[StateNew]),
   with_state(Cont,StateNew,StateOut).


