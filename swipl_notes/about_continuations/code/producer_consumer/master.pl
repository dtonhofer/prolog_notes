% =============================================================================
% Coroutines implementing a Producer-Consumer pair.
% =============================================================================
%
% The "producer" and "consumer" goals/coroutines are managed by a "master" 
% goal. They communicate over a "pipe" which is naturally implemented using an
% open list:
% 
% - The producer appends to the open list (appends at the FIN) and closes it 
%   (sets the FIN to []) when there is nothing left to produce. 
% - The consumer takes reads a single element at the head of the list
%   (reads the TIP and moves forward). Once it reaches [], it knows the
%   list is closed and all elements have been consumed.
%
% Alternatively one could shift the execution flow between "producer" and 
% "consumer" directly - but having a central "master" goal feels more natural.

% ~~~
% Helper with transforms Goal(foo,bar) into Goal(foo,bar,Arg)

goal_extended_by_arg(Goal,Arg,NewGoal) :-
   compound_name_arguments(Goal,Name,Args),
   append(Args,[Arg],MoreArgs),
   compound_name_arguments(NewGoal,Name,MoreArgs).
 
   

with_producer_consumer(ProzGoal,ConzGoal) :-
   Tip=Fin,                               % An initially empty "open list" Tip-Fin
   with_producer_consumer_2(
      _{ prozGoal:call(ProzGoal,Fin)      % Literally build a new goal calling the ProzGoal with 1 argument more
        ,conzGoal:call(ConzGoal,Tip)      % Literally build a new goal calling the ConzGoal with 1 argument more
        ,prozWhen:0
        ,conzWhen:0 }

% I don't want to bother with lots of arguments, so a single SWI-Prolog dict "State" it is!

with_producer_consumer_2(State) :-
   which_coroutine(State.prozWhen,State.conzWhen,Which)
   -> 
   (branch_on_which(Which,State,NewState),  % do something, building NewState
    with_producer_consumer_2(NewState))     % tail-recursive call with new context NewState
   ;
   true.                                    % failure of which_coroutine/2 means we are done here

branch_on_which(proz,State,NewState) :-
   reset(State.prozGoal,CallMeWhen,Cont),
   (Cont == 0
    -> put_dict(_{prozGoal:0   ,prozWhen:inf },State,NewState),
    ;  put_dict(_{prozGoal:Cont,prozWhen:CallMeWhen},State,NewState)).


branch_on_which(conz,State,NewState) :-
   reset(State.conzGoal,CallMeWhen,Cont),
   (Cont == 0
    -> put_dict(_{conzGoal:0   ,conzWhen:inf },State,NewState),
    ;  put_dict(_{conzGoal:Cont,conzWhen:CallMeWhen},State,NewState)).

branch_on_which(delay(Delta),State,State) :-
   sleep(Delta).

% ~~~
% Either sleep until the earliest coroutine is expected to become ready and
% then peform a tail-recursive call, or, if neither coroutine will ever
% become ready, succeed.
% ~~~

which_coroutine(inf,inf,_) :- 
   !,
   fail.

which_coroutine(inf,When,Which) :-
   When \== inf,
   !,
   branch_loop(When,Which).

which_coroutine(When,inf,Which) :-
   When \== inf,
   !,
   branch_loop(When,Which).

which_coroutine(When1,When2) :-
   When1 \== inf,
   When2 \== inf,
   When is min(When1,When2),
   get_time(Now), 
   Delta is When-Now,
   Delta > 0 -> Which = delay(Delta)
   ; proz, conz 

   sleep(Delta). % if delta is <= 0, return immediately

   

% ~~~
% Decide whether to call the producer or the consumer, selected uniformly
% at random among the coroutines which are "ready" as indicated by 
% "ProdWhen" (a timestamp saying that production should be restarted at that
% moment earliest) and "ConsWhen" (a timestamp saying that consumption should
% be restarted at that moment earliest).
% FAILS if none are ready, otherwise the choise, one of the atoms 'producer'
% or 'consumer', can be found in "Choice".
% ~~~

which_coroutine(ProzWhen,ConzWhen,Choice) :-   
   (when_ready(ProzWhen) -> Choice1 = [producer] ; Choice1 = []),
   (when_ready(ConzWhen) -> Choice2 = [consumer|Choice1] ; Choice2 = Choice1),
   random_member(X,Choice2).

when_ready(When) :- 
   When \== inf, get_time(Now), Now > When. 
   









% ===
% The "master coroutine": It writes the values yielded by "Goal" to stdout.
% ===

with_write(Goal) :-
  reset(Goal,Loot,Cont),       % "Loot" is the term yieled by the "iterator", should be "yield(X)".
  tag_cont(Cont,TaggedCont),   % This just tags the continuation term "Cont".
  branch(TaggedCont,Loot).     % This will result in success or a tail-recursive call.

branch(zero,_).                % Successful "end of loop" because "Goal" succeeded.

branch(cont(Cont),yield(X)) :-
   write(X),write('.'),        % Iterator yielded X; do something with it.
   with_write(Cont).           % Tail-recursive call into a new context where Cont is the new Goal.

% ===
% Tag the continuation "Cont" for easy pattern-directed branching.
% "zero" vs. "cont(_)" is better to make a branching descision than
% "0" vs "some unknown structure that can be used as a goal"
% ===

tag_cont(0,zero) :- !.
tag_cont(Cont,cont(Cont)).

% ===
% Two example "iterators"
% ===

% ---
% yield/1 is just a shift/1 of the "yield term" carrying the value that
% that shall be communicated to the master coroutine.
% ---

yield(X) :- shift(yield(X)).

% ---
% An "iterator" that consist in a predicate yielding values from a list.
%
% 1) "cut* in order to make sure that the system knows it can reclaim the
%    stack space during a possibly infinite iteration.
% 2) "yield" the head element of the list. This calls shift/1 and thus passes
%    control to the master coroutine with predicate with_write/1. It knows how
%    to actually output that element. Control flow continues as if it was
%    returning from the call to reset/3.
% 3) After return to to this iterator (i.e. after the matser predicate called
%    reset/3 with the appropriate continuation), whereby control flow continues
%    as if it was returning from shift/1, perform a recursive call to set a new
%    context to continue with the tail of the list.
% ---

from_list([X|Xs]) :-
   !,           % the cut is not really needed because only one clause ever matches
   yield(X),
   from_list(Xs).

from_list([]).

% ---
% An "iterator" that consist in a predicate that yields the values [L..U-1],L>=0
% ---

from_interval(L,U) :-
   L<U,
   !,          % if you don't cut you WILL get "ERROR: Stack limit exceeded" for large intervals
   yield(L),
   succ(L,Lp),
   from_interval(Lp,U).

from_interval(X,X).

% ===
% Run it
% ===

run(interval) :-
   with_write(from_interval(0,100)).

run(list) :-
   with_write(from_list([a,b,c,d,e,f])).


% With manager

manager :-
   ConsDict = _{goal:producer,when:0},
   ProdDict = _{goal:consumer,when:0},
   manager_2(ConsDict,ProdDict).
 
manager_2(ConsDict,ProdDict) :-
   get_time(Now),
   (Now > ProdDict.when) 
   -> reset(ProdDict.goal,ProdData,NewProdCont),
      
   
   
   reset(Producer,Ball,Continuation),
   reset(Consumer,Ball,Continuation),

producer :-
   is it time to produce a new value
   if not shift with sleeptime
   otherwise produce a random value
   append it to the list
   select a new sleeptime
   shift with sleeptime

consumer :-
   is there a value at the head of the list
   if yes, eat it
   otherwise shift

% Interleaved

producer(When,Fin) :-
   get_time(Now),
   (Now>When) 
   -> (M is random_float, Fin = [M|NewFin], NextWhen is floor(Now) + random(6), shift(produced(true,NewFin,NextWhen))
   ;  shift(produced(false,Fin,When)).

consumer(Head,Fin,Cont) :-
   (Fin == [])
   -> debug(consumer,"Consumer: Stream is closed")
   ;  (Head == Fin)
      debug(consumer,"Consumer: Stream is empty"),
      reset(Producer,Ball,Continuation),
      consumer(Ball,Continuation).


      

