% ===
% State monad (or rather, effect handler expressing the state monad), illustrating usage
% of delimited continuations.
% See "Delimited Continuations for Prolog", Schrijvers & al., 2012, p. 3
% ===

:- debug(count/0).
:- debug(run_with_state/3).

% ===
% Call this at the toplevel. We request 5 loops through count. 
% The final state will be unified with "Out"
% ===

run(Out) :- 
   run_with_state(count,[0,5],Out).

% ===
% The worker predicate, which gets/sets state held by the (invisible) context using
% provided commands
% ===

count :- 
   debug(count/0,"In count/0",[]),
   %%
   get_state(S),
   %%
   S = [Val,Count],
   debug(count/0,"In count/0: got Val=~q, Count=~q",S),
   (
      Count == 0 
      -> 
      % stop the loop, we are done counting
      true    
      ;
      NewVal   is Val   + 1, 
      NewCount is Count - 1,
      NewS     = [NewVal,NewCount],
      debug(count/0,"In count/0: putting Val=~q, Count=~q",NewS),
      %%
      put_state(NewS),
      %%
      % tail-recursive call to perform looping
      count 
   ).

% ===
% Available effect handler commands. 
% The put and get predicates are all syntax and no semantics; they simply "shift" 
% their own term representations. 
% ===

get_state(S) :- shift(get_state(S)).
put_state(S) :- shift(put_state(S)).  

% ===
% Effect handler
% ===

run_with_state(Goal,Scur,Sout) :-     % a stateful context for "Goal" with current state "Scur"
   reset(Goal,Ball,Cont),             % run Goal & reappear here on shift/1 or success
   (Cont == 0 -> 
      TaggedCont = zero ;             % tag for easier branching
      TaggedCont = cont(Cont)),       % tag for easier branching
   branch(TaggedCont,Ball,Scur,Sout).  

% Note the cut on the second clause to make branch/2 deterministic (there
% is as yet no indexing on the second argument). Without the cut, you 
% may well run out of global stack!
   
branch(zero,_,Scur,Scur) :- 
   debug(run_with_state/3,"After return from reset/3: continuation is 0; goal succeeded",[]).
   
branch(cont(Cont),get_state(Scur),Scur,Sout) :- !,
   debug(run_with_state/3,"After return from reset/3: ~q",[get_state(Scur)]),
   run_with_state(Cont,Scur,Sout). % continuation becomes the new goal, same state
   
branch(cont(Cont),put_state(S),_,Sout) :- 
   debug(run_with_state/3,"After return from reset/3: ~q",[put_state(S)]),
   run_with_state(Cont,S,Sout).    % continuation becomes the new goal, new state
   
