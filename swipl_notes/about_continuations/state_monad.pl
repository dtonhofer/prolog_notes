% ===
% State monad, illustration delimited continuations
% See "Delimited Continuations for Prolog", Schrijvers & al.
% ===

:- nodebug(count/0).
:- nodebug(run_with_state/3).

% ===
% The worker predicate, which gets/sets surrounding 
% state using commands
% ===

count :- 
   debug(count/0,"In count/0",[]),
   get_state(S),
   S = [Val,Count],
   debug(count/0,"In count/0: got Val=~q, Count=~q",S),
   (
      Count == 0 -> true % stop the loop
      ;
      NewVal   is Val   + 1, 
      NewCount is Count - 1,
      NewS     = [NewVal,NewCount],
      debug(count/0,"In count/0: putting Val=~q, Count=~q",NewS),
      put_state(NewS),
      count % recursive call
   ).

% ===
% Main call
% ===

run(Out) :- 
   run_with_state(count,[0,100000000],Out).

% ===
% Available commands for state handling loop (pure syntax)
% ===

get_state(S) :- shift(get_state(S)).
put_state(S) :- shift(put_state(S)).  

% ===
% State handling
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
   debug(run_with_state/3,"After return from reset/3: continuation is 0; goal succeeded",_).
   
branch(cont(Cont),get_state(Scur),Scur,Sout) :- !,
   debug(run_with_state/3,"After return from reset/3: ~q",[get_state(Scur)]),
   run_with_state(Cont,Scur,Sout). % continuation becomes the new goal, same state
   
branch(cont(Cont),put_state(S),_,Sout) :- 
   debug(run_with_state/3,"After return from reset/3: ~q",[put_state(S)]),
   run_with_state(Cont,S,Sout).    % continuation becomes the new goal, new state
   
