% Trying SWI-Prolog delimited continuations
%
% This is a looping observer/1 predicate which has the tip of an open list
% and can observe it growing as it is called (via reset/3) by the looping
% predicate appender/4 which appends new elements to the open list.
%
% The list becomes an inter-process-communication device between a single 
% producer appender/4 and a (non-)consumer, observer/1. 
%
% In the end, appender/4 tells the observer/1 to stop looping and succeed
% by instantiation the variable it has been passed by shift/1 to 'stop'.

:- debug(observer).

starter :- 
   Tip=Fin,                                    % The Tip and Fin of an open list, initialy the sam unbound variable
   reset(observer(Tip),_,ObsCont),             % Call the observer with the Tip of the open list; it always references the same "cell" in memory
   appender([a,b,c,d],Fin,ObsCont,_).          % Append the given list to the open list at Fin (which references different cells over time)

appender([X|Xs],Fin,ObsCont,_) :-
   Fin=[X|NewFin],                             % Append X to the open list, giving a new fin, NewFin
   reset(ObsCont,obs(Stop),ObsContNew),        % The "Stop" will be communicated to the next activation of appender/4
   appender(Xs,NewFin,ObsContNew,Stop).        % Loop around for more growing (or for closing the list)
   
appender([],Fin,ObsCont,Stop) :-               % To make dataflow clear, we dont cram everything into the head
   Fin = [],                                   % Close list
   Stop = stop,                                % This will tell observer/1 to stop on next reset call as it binds the variable of the _previous_ reset
   reset(ObsCont,_,N),                         % On return, N will be 0 because the observer succeeded
   assertion(N==0).
   
observer(Tip) :-
  debug(observer,"Observer sees: ~q",[Tip]),
  shift(obs(Stop)),
  ((Stop == stop)
   -> debug(observer,"Observer received stop. Last observation: ~q",[Tip])
   ;  (debug(observer,"Observer loops",[]),
       observer(Tip))).

/*
And so:

?- starter.
% Observer sees: _10550
% Observer loops
% Observer sees: [a|_14110]
% Observer loops
% Observer sees: [a,b|_14350]
% Observer loops
% Observer sees: [a,b,c|_14590]
% Observer loops
% Observer sees: [a,b,c,d|_14830]
% Observer received stop. Last observation: [a,b,c,d]
true.
*/
