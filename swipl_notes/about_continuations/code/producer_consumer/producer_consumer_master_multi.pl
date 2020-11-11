% =============================================================================
% Multiple Producer-Consumer coroutines working on the same pipe.
% =============================================================================
% Multiple producers write to the pipe.
% Multiple consumers read from the pipe.
% A master coroutine arbitrates and calls reset/3 on producers & consumers.
%
% The pipe has a limited number of "places". Once full, production stops.
% Once empty, consumption stops.
%
% The pipe is basically an open list to which producers append at the tail
% (the FIN) and from which consumers remove at the head (the TIP).
%
% To pack all information about the pipe into a single "object", the pipe
% is represented by an SWI-Prolog dict: _{tip:Tip, fin:Fin, len:Len}.
% 
% While all coroutines can easily build a new "pipe dict" and hand it to the
% master via shift/1 calls, coroutines have trouble obtaining the latest
% pipe dict via another shift/1 call: when they get reactivated, the pipe
% dict communicated to them via a fresh variable X in shift(get_pipe(X))
% is likely completely out of date. We could use "global variables" but 
% instead implement a stream of pipe dicts via another open list: the 
% latest pipe dict is always at the far end of the list.

% Run with ?- run_pcmulti.

:- debug(master).
:- debug(consumer).
:- debug(producer).

% In this example, we have 2 producers and 2 consumers.

run_pcmulti :-
   setup_producer_consumer_multi(
      [producer(producer1),producer(producer2)],
      [consumer(consumer1),consumer(consumer2)]).

% Some constants.

const(max_pipe_len,10).
const(producer_success_probability,0.01).

% ===
% SWI-Prolog dict representing the state of the pipe, with some (traditional,
% instead of dict-associated) predicates.
% ===

% pipe_init(+NewPipe)

pipe_init(_{tip:Tip,fin:Fin,len:0)) :-
   Tip=Fin.

% pipe_get(+PipeIn,-PipeOut,+Element); throws if pipe is empty

pipe_get(_{tip:Tip    , fin:Fin , len:Len},
         _{tip:NewTip , fin:Fin , len:NewLen},
         Element) :-
   (PipeIn.len == 0)
   -> throw("Pipe is empty, can't get!")
   ;  (Tip = [Element|NewTip], NewLen is Len-1).

% pipe_put(+PipeIn,-PipeOut,+Element); throws if the pipe is in a state so that "put" is not possible.

pipe_put(_{tip:Tip , fin:Fin   , len:Len},
         _{tip:Tip , fin:NewFin, len:NewLen},
         Element) :-
   const(max_pipe_len,MaxPipeLen),
   ((Len >= MaxPipeLen) -> throw("Pipe is already full, can't put!") ; true),
   (nonvar(Fin)         -> throw("Pipe has a nonvar FIN, can't put!") ; true),
   Fin=[Element|NewFin],  % this destroys the Fin of PipeIn!
   NewLen is Len+1.

% pipe_close(+Pipe): throws if the pipe is already closed

pipe_close(_{tip:Tip, fin:Fin, len:Len}) :-
   (nonvar(Fin) -> throw("Pipe has a nonvar FIN, can't close!") ; true),
   Fin = []. % instantiate "in-place"

% just check

pipe_closed(_{tip:Tip, fin:Fin, len:Len}) :-
   (Fin == []).

pipe_empty(_{tip:Tip, fin:Fin, len:Len}) :-
   Len == 0,
   assertion(Fin == [] ; Tip == Fin).
   
pipe_closed_and_empty(_{tip:Tip, fin:Fin, len:Len}) :-
   Len == 0, Fin == [],
   assertion(Tip == []).

pipe_open_and_empty(_{tip:Tip, fin:Fin, len:Len}) :-
   Len == 0, var(Fin),
   assertion(Tip == Fin).

pipe_full(_{tip:Tip, fin:Fin, len:Len}) :-
   const(max_pipe_len,MaxPipeLen),
   assertion(Len =< MaxPipeLen),
   Len == MaxPipeLen.

% ===
% Setting up
% ===

setup_producer_consumer_multi(Producers,Consumers) :-
   pipe_init(Pipe),
   with_producer_consumer_multi(Producers,Consumers,Pipe).

% ===
% "The keeper of the pipe". It switches between coroutines by selecting a 
% continuation at random and calling reset/3 on it.
%
% So the coroutines cannot make any assumption about call order. 
%
% The continuations of the coroutines are held in list. Once that list is 
% empty, i.e. once all the coroutines have run to success, we are done.
%
% The keeper of the pipe handles the commands to "set the pipe" emitted via 
% shift/1 by the coroutines. 
% ===

with_producer_consumer_multi([],[],Pipe) :-
   !,
   assertion(pipe_closed_and_empty(Pipe)),
   debug(master,"Master: *** Nothing left to call. Success! ***",[]).

with_producer_consumer_multi([],Consumers,Pipe) :-
   !,
   assertion(pipe_closed_and_empty(Pipe)),
 

with_producer_consumer_multi(Producers,[],Pipe) :-

with_producer_consumer_multi(Producers,Consumers,Pipe) :-



   assertion(Goals = [_|_]),
   select_goal(Goals,Goal,Rest),
   reset(Goal,Cmd,Continuation),
   branch(Continuation,Cmd,Rest,Pipe).
   
select_goal(Goals,Selected,Rest) :-
   length(Goals,NumGoals),
   random_between(1,NumGoals,Index1),
   nth1(Index1,Goals,Selected,Rest).

branch(0,_,Rest,Pipe) :-
   !,
   with_producer_consumer_multi(Rest,Pipe).

branch(Continuation,put_pipe(NewPipe),Rest,Pipe) :-
   !,
   with_producer_consumer_multi([Continuation|Rest],NewPipe).
   
branch(Continuation,get_pipe(),Rest,Pipe) :-

   consumer_cmd(Cmd,Pipe,Pipe2),

   reset(PrCont,Cmd,NewCoCont),
   producer_cmd(Cmd),
   with_state(NewCoCont,NewPrCont).

consumer_cmd(get_pipe(Pipe),Pipe,Pipe).

producer_cmd(get_pipe(Pipe),Pipe,Pipe).

consumer_cmd(set_pipe(Pipe),_,Pipe).

producer_cmd(set_pipe(Pipe),_,Pipe).

% ===
% Consumer coroutine.
% ===

consumer(Name) :-
   debug(consumer,"Consumer ~q: Shift to get pipe.",[Name]),
   shift(get_pipe(Pipe)),
   debug(consumer,"Consumer ~q: Post-shift: Obtained: ~q",[Name,Pipe.tip]),
   consumer_2(Name,Pipe).

consumer_2(Name,Pipe) :-
   pipe_empty_and_closed(Pipe),
   !,
   debug(consumer,"Consumer ~q: *** Pipe is empty and has been closed. Consumer succeeds. ***",[Name]).

consumer_2(Name,Pipe) :-
   pipe_empty_and_open(Pipe),
   !
   debug(consumer,"Consumer ~q: Pipe is empty but not closed. Nothing to do. Looping.",[Name]),
   consumer(Name).

consumer_2(Name,Pipe) :-
   assertion(\+ pipe_empty(Pipe)),
   pipe_get(Pipe,NewPipe,X),
   debug(consumer,"Consumer ~q: Consumed <--- ~q.",[Name,X]),
   debug(consumer,"Consumer ~q: Shift to put updated pipe",[Name]),
   shift(put_pipe(NewPipe)),
   debug(consumer,"Consumer ~q: Post-shift. Looping.",[Name]),
   consumer(Name).

% ===
% Producer coroutine. 
% ===

producer(Name) :-
   debug(producer,"Producer ~q: Shift to get pipe.",[Name]),
   shift(get_pipe(Pipe)),
   debug(producer,"Producer ~q: Post-shift: Obtained: ~q",[Name,NewPipe.tip]),
   producer_2(Name,Pipe).

producer_2(Name,Pipe) :-
   pipe_closed(Pipe),                                  % One of the other producers must have closed the pipe
   !
   debug(producer,"Producer ~q: Pipe is already closed. *** Predicate succeeds. ***",[Name]).

producer_2(Name,Pipe) :-
   const(producer_success_probability,PSuccess).
   (maybe(PSuccess)                                         % With small probability...
    -> producer_succeed(Name,Pipe)                          % ... succeed the predicate, i.e. terminate producer and close pipe.
    ;  (producer_produce(Name,Pipe,FinalPipe,FinalCount),   % Otherwise produce a few elements (FinalCount says how many were produced.)
        producer_loop_around(FinalPipe,FinalCount))).  % And after production, perform a shift/1 and then a tail-recursive call.

producer_loop_around(Pipe,Count) :-
   debug(producer,"Producer: Control flow goes to consumer. ~d element(s) produced.",[Count]),
   shift(produced(Pipe,Count)),
   debug(producer,"Producer: Control flow received from consumer. Asking for new pipe.",[]),
   shift(wantpipe(NewPipe)),
   debug(producer,"Producer: Control flow received from consumer. Got new pipe.",[]),
   producer(NewPipe).

producer_succeed(Name,Pipe) :-
   debug(producer,"Producer ~q: Closing pipe.",[Name]),
   pipe_close(Pipe),
   debug(producer,"Producer ~q: Shift to put updated pipe.",[Name]),
   shift(put_pipe(NewPipe)),
   debug(producer,"Producer ~q: Post-shift. *** Predicate succeeds. ***",[Name]).
   
producer_produce(Name,PipeIn,FinalPipe,FinalCount) :-
   producer_produce_2(Name,PipeIn,FinalPipe,0,FinalCount).

producer_produce_2(Name,CurPipe,CurPipe,CurCount,CurCount) :-
   pipe_full(CurPipe),
   !,
   debug(producer,"Producer ~q: Pipe is full.",[Name]).

producer_produce_2(Name,CurPipe,FinalPipe,CurCount,FinalCount) :-
   assertion(\+ pipe_full(CurPipe)),
   const(producer_production_probability,PProduction),
   (maybe(PProduction) 
    -> (producer_produce_one_element(Name,CurPipe,Pipe2,CurCount,Count2),
        producer_produce_2(Name,Pipe2,FinalPipe,Count2,FinalCount))
    ;  producer_finish(CurPipe,FinalPipe,CurCount,FinalCount)).

producer_finish(CurPipe,CurPipe,CurCount,CurCount). % just bounce the values to final

producer_produce_one_element(PipeIn,PipeOut,CountIn,CountOut) :-
   random_between(10000,99999,X),
   pipe_put(PipeIn,PipeOut,X),
   CountOut is CountIn + 1,
   debug(producer,"Producer ~q: Produced ---> ~q.",[Name,X]).



