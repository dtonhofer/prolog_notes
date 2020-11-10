% =============================================================================
% Coroutines implementing a Producer-Consumer pair.
% =============================================================================
% The "producer" and "consumer" goals/coroutines are managed by a "master" 
% goal/coroutine. "producer" and "consumer" communicate over a "pipe" which is
% naturally-for-Prolog implemented using an "open list" (i.e. a list that does 
% not have an [] at final position but an unbound variable: one can append to
% it). That list need not be known to the "master" coroutine.
% 
% - The producer appends to the open list (appends at the list's FIN position)
%   and closes it (sets the FIN to []) when there is nothing more that will be
%   producede .
% - The consumer reads a single element at the head of the list (reads the TIP
%   position and moves tailwards). Once it reaches [], it knows the list 
%   has been closed and all elements have been consumed.
%
% Alternatively one could shift the execution flow between "producer" and 
% "consumer" directly - but having a central "master" goal feels more natural.

:- debug(with_state).
:- debug(consumer).
:- debug(producer).

run :-
   producer_consumer_setup(producer,consumer). % the "Goals" are just the names of the predicates

const(max_pipe_len,10).
const(producer_success_probability,0.01).

consumer_producer :-
   pipe_init(Pipe),
   consumer(Pipe,producer).

% ===
% SWI-Prolog dict representing the pipe, with some (traditional) predicates
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
% "The keeper of the pipe" - This is basically an effect handler / state 
% handler / cmd handler. It switches between the producer and consumer 
% coroutines by selecting a continuation at random and calling reset/3 on it.
% So the coroutine predicates must be prepared to deal with that.
% The keeper of the pipe handles the commands to "get the pipe" and "set the
% pipe" emitted via shift/1 by the coroutines. 
% The continuations of the coroutines are held in list. Once that list is 
% empty, i.e. once all the coroutines have run to success, we are done.
% ===

with_pipe(Goals) :-   
   pipe_init(Pipe),
   with_pipe_2(Pipe,Goals).

with_pipe_2(_,[]) :- !.

with_pipe_2(Pipe,Goals) :-
   assertion(Goals = [_|_]),
   length(Goals,NumGoals),
   random_between(1,NumGoals,Index1),
   nth1(Index1,Goals,Goal),
   reset(Goal,Cmd,Continuation),
   (Continuation == 0) 
   -> nth1(Index1,Goals,_,NewGoals),with_pipe_2(Pipe,LessGoals)
   ;  Cmd = put_pipe(NewPipe),with_pipe_2(NewPipe,Goals)
   ;  Cmd = get_pipe(Pipe),
   

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



