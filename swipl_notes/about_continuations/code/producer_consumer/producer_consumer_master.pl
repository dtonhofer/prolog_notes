% =============================================================================
% Producer-Consumer coroutine pair via Delimited Continuations (Attempt #1)
% =============================================================================
% Run with ?- run_pcm.
%
% The "producer" and "consumer" coroutines are managed by a "master" 
% coroutine which maintains the symmetry of the whole.
%
% "producer" and "consumer" communicate over a "pipe" which is
% naturally-for-Prolog implemented using an "open list" (i.e. a list that does 
% not have an [] at final position but an unbound variable: one can append to
% it). That list need not be known to the "master" coroutine.
% 
% - The producer appends to the open list (appends at the list's FIN position)
%   and closes it (sets the FIN to []) when there is nothing more that will be
%   produced.
%
% - The consumer reads a single element at the head of the list (reads the TIP
%   position and moves tailwards). Once it reaches [], it knows the list 
%   has been closed and all elements have been consumed.
%
% (I thought that one could shift the execution flow between "producer" and 
%  "consumer" directly as is generally done in descriptions of producer-consumer
%  coroutines, but this does not seem to be the case. There is of course an 
%  asymmetry in the reset/shift pair. It turns out that the consumer will have
%  to do the reset/3 call, and the producer the shift/1 call and that the
%  code becomes problematic for initialization and for tracking state.)

:- debug(master).
:- debug(consumer).
:- debug(producer).

run_pcm :- setup_producer_consumer(producer,consumer). % the Goals are just the names of the predicates

const(producer_success_probability,0.01).
const(producer_produce_probability,0.95).

% ===
% Preparing producer and consumer by making the respective ends of the open
% list arguments of their calls.
% ===
% Instead of a list of individual arguments, we use SWI-Prolog dicts, which
% allows us to pack all the stuff into a single argument (very useful when
% you refactor as the argument lists don't change) and the now-packed arguments
% are naturally given names, which are the dict keys (very useful when reading the
% code).
   
setup_producer_consumer(ProzGoal,ConzGoal) :-
   Tip=Fin,                               % An initially empty "open list" with the same fresh variable for TIP and FIN.
   with_producer_consumer(
      _{ prozGoal:call(ProzGoal,Fin)      % The value is a new goal term calling the ProzGoal with an additional "Fin" argument
        ,conzGoal:call(ConzGoal,Tip)      % The value is a new goal term calling the ConzGoal with an additional "Tip" argument
        ,prozWhen:0                       % The time at which the master shall reset/3 to the producer: it's 0, in the past
        ,conzWhen:0 }).                   % The time at which the master shall reset/3 to the consumer: it's 0, in the past 

% ===
% The "master coroutine". It regularly calls the producer and consumer coroutines 
% via reset/3 until both have succeeded.
% ===

with_producer_consumer(State) :-
   which_coroutine(                       % Decision which coroutine to call next. Fails if there is none. Sleeps otherwise
      State.prozWhen,                     % Dict entry must exist or an exception occurs
      State.conzWhen,                     % Dict entry must exist or an exception occurs
      Which,                              % Set to either 'producer' or 'consumer'
      When)                               % Sleep till this point before calling
   ->  
   (sleep_until(Which,When),
    call_coroutine(Which,State,NewState),
    with_producer_consumer(NewState))
   ;
   debug(master,"Master: *** Nothing left to call. Success! ***",[]).
                                   
call_coroutine(producer,State,NewState) :-
   debug(master,"Master: Will call producer.",[]),
   reset(State.prozGoal,produced(Count,When),Cont),
   (Cont == 0 
    -> debug(master,"Master: Returned from producer. Producer succeeded.",[])
    ;  debug(master,"Master: Returned from producer. ~d element(s) have been produced. Call again at ~f.",[Count,When])), 
   (Cont == 0
    -> put_dict(_{prozGoal:0   ,prozWhen:inf },State,NewState)
    ;  put_dict(_{prozGoal:Cont,prozWhen:When},State,NewState)).

call_coroutine(consumer,State,NewState) :-
   debug(master,"Master: Will call consumer.",[]),
   reset(State.conzGoal,consumed(Count,When,Tip),Cont),
   (Cont == 0 
    -> debug(master,"Master: Returned from consumer. Consumer succeeded.",[])
    ;  (debug(master,"Master: Returned from consumer. ~d element(s) have been consumed. Call again at ~f.",[Count,When]), 
        debug(master,"Master: Pipe is now: ~q",[Tip]))),
   (Cont == 0
    -> put_dict(_{conzGoal:0   ,conzWhen:inf },State,NewState)
    ;  put_dict(_{conzGoal:Cont,conzWhen:When},State,NewState)).
    
% ===
% which_coroutine(+WhenToCallProducer,+WhenToCallConsumer,-Which,-SleepUntil)
% ===

% case of "producer will never be called but consumer will"

which_coroutine(inf,When,consumer,When) :-
   When \== inf,
   debug(master,"Master: Consumer ready at ~f. Producer is done.",[When]),
   !.

% case of "consumer will never be called but producer will"
% this should never occur!!

which_coroutine(When,inf,producer,When) :-
   When \== inf,
   debug(master,"Master: Producer ready at ~f. Consumer is done.",[When]),
   throw("Producer is live, but consumer is done").

% case of "producer will be called first"

which_coroutine(ProzWhen,ConzWhen,producer,ProzWhen) :-
   ProzWhen \== inf,
   ConzWhen \== inf,
   ProzWhen < ConzWhen, % on equality, take consumer first
   debug(master,"Master: Producer ready at ~f, Consumer ready later at ~f.",[ProzWhen,ConzWhen]),
   !.

% case of "consumer will be called first"

which_coroutine(ProzWhen,ConzWhen,consumer,ConzWhen) :-
   ProzWhen \== inf,
   ConzWhen \== inf,
   ProzWhen >= ConzWhen, % on equality, simply take consumer first
   debug(master,"Master: Consumer ready at ~f, Producer ready later at ~f.",[ConzWhen,ProzWhen]),
   !.

% sleeping until...

sleep_until(Which,When) :-
   get_time(Now),
   Delay is max(0, When - Now),
   debug(master,"Master: Sleeping ~f seconds until ~q is ready.",[Delay,Which]),
   sleep(Delay).   % if Delay <= 0, return immediately

% ===
% Consumer coroutine.
% It is handed the Tip of the open list. The oldest produced elements shall be retrieved from there.
% ===

consumer(Tip) :-
   var(Tip),
   !,
   debug(consumer,"Consumer: Pipe is empty. Nothing to consume.",[]),
   consumer_loop_around(0,Tip).
   
consumer(Tip) :-
   Tip == [],
   !,
   debug(consumer,"Consumer: *** Pipe has been closed. Success. ***",[]).

consumer([X|NewTip]) :-
   debug(consumer,"Consumer: Consumed <--- ~q.",[X]),
   consumer_loop_around(1,NewTip).

consumer_loop_around(Count,Tip) :-
   consumer_delay_until(Count,When),
   debug(consumer,"Consumer: Control flow goes to master.",[]),
   shift(consumed(Count,When,Tip)), % also give the tip for debug-printing
   debug(consumer,"Consumer: Control flow received from master.",[]),
   consumer(Tip).

% ~~~
% Call me again when?
% ~~~
 
% Nothing consumed: A couple of seconds delay

consumer_delay_until(0,When) :-
   random(0.5,2,Delay),                                      
   get_time(Now),
   When is Now + Delay.

% Something consumed: Call me at once

consumer_delay_until(1,Now) :-                              
   get_time(Now).

% ===
% Producer coroutine. 
% It is handed the Fin of the open list. Newly produced elements shall be appended there.
% ===

producer(Fin) :-
   const(producer_success_probability,P),
   maybe(P)                                % Decide whether to succeed (i.e. terminate the producer)
   ->
   producer_succeed(Fin)
   ;
   (producer_produce(Fin,NewFin,Count),    % Produce a single element or not, randomly 
    producer_loop_around(Count,NewFin)).

producer_succeed(Fin) :-
   debug(producer,"Producer: *** Success. ***",[]),
   Fin = [].                               % When succeeding, the producer closes the open list

producer_produce(Fin,NewFin,Count) :-
   const(producer_produce_probability,P),
   maybe(P) 
   ->
   (random_between(10000000,99999999,X),
    Fin = [X|NewFin], Count = 1,
    debug(producer,"Producer: Produced ---> ~q.",[X]))
   ;
   (NewFin = Fin, Count = 0).

producer_loop_around(Count,Fin) :-
   producer_delay_until(Count,When),
   debug(producer,"Producer: Control flow goes to master.",[]),
   shift(produced(Count,When)),
   debug(producer,"Producer: Control flow received from master.",[]),
   producer(Fin). 

% ~~~
% Call me again when?
% ~~~

% Nothing produced: A couple of seconds delay

producer_delay_until(0,When) :-
   random(0.5,2,Delay),                                      
   get_time(Now),
   When is Now + Delay.

% Something produced: Call me at once (se we get runs of produced elements, with
% run lengths distributed according to the exponential distribution)

producer_delay_until(1,Now) :-                              
   get_time(Now).

