% ===
% Prolog coroutines using Delimited Continuations
%
% Code inspired by "Delimited Continuations for Prolog" Schrijvers et al., 2013, page 3
% ===
%
% Effect handler expressing the state monad, illustrating usage
% of delimited continuations.

:- debug(count/0).
:- debug(markov/0).
:- debug(with_state/3).
:- debug(top).

% ===
% Effect handler
% ===

% Available effect handler commands. 
% The put and get predicates are all syntax and no semantics; 
% they simply "shift" their own term representations. 

get_state(S) :- shift(get_state(S)).
put_state(S) :- shift(put_state(S)).  

% Helper: Tag "Cont" for pattern-directed branching.
% - "zero vs cont(_)" is better than
% - "0 vs some unknown structure that can be used as a goal"

tag_cont(0,zero) :- !.
tag_cont(Cont,cont(Cont)).

% Manage state in a tail-recursive loop

with_state(Goal,Scur,Sout) :-         % a stateful context for "Goal" with current state "Scur"
   reset(Goal,Ball,Cont),             % run Goal & reappear here on shift/1 or success
   tag_cont(Cont,TgCont),
   branch(TgCont,Ball,Scur,Sout).  

% Note the cut on the second clause to make branch/2 deterministic (there
% is as yet no indexing on the second argument). Without the cut, you 
% may well run out of global stack!
   
branch(zero,_,Scur,Scur) :- 
   debug(with_state/3,"After return from reset/3: continuation is 0; goal succeeded",[]).
   
branch(cont(Cont),get_state(Scur),Scur,Sout) :- !,
   debug(with_state/3,"After return from reset/3: ~q",[get_state(Scur)]),
   with_state(Cont,Scur,Sout). % continuation becomes the new goal, same state
   
branch(cont(Cont),put_state(S),_,Sout) :- 
   debug(with_state/3,"After return from reset/3: ~q",[put_state(S)]),
   with_state(Cont,S,Sout).    % continuation becomes the new goal, new state

% ===
% Example worker implementing a probabilistic state automaton / markov state
% ===
   
pass(s0 , s1).
pass(s0 , s0).
pass(s0 , s2).

pass(s1 , s2).
pass(s1 , end).

pass(s2 , s1).
pass(s2 , s0).

% probabilities are uniform

select(CurState,NextState) :-
   bagof(SX,pass(CurState,SX),Bag),
   random_member(NextState,Bag).
   
markov :- 
   get_state(S),
   S = [CurState,History],
   debug(markov/0,"Got state ~q",[S]),
   (
      CurState == end -> true % stop the loop, we are done
      ;
      select(CurState,NextState),
      NS = [NextState,[NextState|History]],
      debug(markov/0,"Putting state ~q",[NS]),
      put_state(NS),
      markov % loop around
   ).

% ===
% Example worker counting up. State is [Val,Count], we want to
% generate the states [Val+1,Count-1],...,[Val+Count,0].
% ===

count :- 
   get_state(S),
   S = [Val,Count],
   debug(count/0,"Got state ~q",[S]),
   (
      Count == 0 -> true % stop the loop, we are done
      ;
      succ(Val,NewVal),
      succ(NewCount,Count),
      NS = [NewVal,NewCount],
      debug(count/0,"Putting state ~q",[NS]),
      put_state(NS),
      count % loop around
   ).

% ===
% Call this at the toplevel.
% ===

run(markov) :- with_state(markov,[s0,[s0]],Out),debug(top,"Final state is ~q",[Out]).
run(count)  :- with_state(count,[10,5],Out),debug(top,"Final state is ~q",[Out]).

