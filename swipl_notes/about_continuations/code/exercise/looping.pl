% In an unexiting development, we can build a loop that calls `reset/3` to activate a 
% loop-worker-predicate on each loop passage instead of just *calling* the predicate.
% directly. To make things more interesting, we can store the continuations of 
% loop-worker-predicate obtained from `reset/3` and call them for finalization at the end.
     
:- debug(loop).

loop(N) :-
   loop_2(N,[],WConts),
   maplist(
      [WCont]>>(reset(WCont,_,C),assertion(C==0)), % use a library(yall) lambda
      WConts).

loop_2(0,WConts,WConts) :- 
   !,
   debug(loop,"Loop is done",[]).

loop_2(N,WContsDown,[WCont|WContsUp]) :-   % accumulate worker continuations in-order
   assertion(N>0),
   debug(loop,"Switching to loop_worker(~d)",[N]),
   reset(loop_worker(N),mine,WCont),
   debug(loop,"Back in loop",[]),
   Nm is N-1,
   loop_2(Nm,WContsDown,WContsUp).         % tail-recursive call
       
loop_worker(N) :-
  debug(loop,"In loop_worker(~d)",[N]),
  shift(mine),
  debug(loop,"Finalizing loop_worker(~d)",[N]).

/*
And so:

?- loop(4).
% Switching to loop_worker(4)
% In loop_worker(4)
% Back in loop
% Switching to loop_worker(3)
% In loop_worker(3)
% Back in loop
% Switching to loop_worker(2)
% In loop_worker(2)
% Back in loop
% Switching to loop_worker(1)
% In loop_worker(1)
% Back in loop
% Loop is done
% Finalizing loop_worker(4)
% Finalizing loop_worker(3)
% Finalizing loop_worker(2)
% Finalizing loop_worker(1)
true.
*/
