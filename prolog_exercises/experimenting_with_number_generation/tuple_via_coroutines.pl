:- module(tuple_via_coroutines,
          [
          tuple_via_coroutines/4  % tuple_via_coroutines(Tuple:Dict,Min:Integer,Max:Integer:Dim:Integer)
          ]).
          
% =============================================================================
% Enumerate all tuples of "D" (dimensionality) integers ranged from "Min" to
% "Max" both inclusive (i.e. the numbers of D places of radix Max-Min+1)
%
% A tuple is represented by an SWI-Prolog dict where the keys are integers
% going from 0 to D-1 and the values are from [Min,Max]. 
% 
% Example
% =======
%
% On backtracking, this generates all possibilities. 
%
% Generate all 3125 entries of the set with value range 1..5, 
% dimensionalty 5:
%
/*
   ?-
   bagof(T,tuple_via_coroutines(T,1,5,5),AllStatesUnsorted),
   sort(AllStatesUnsorted,AllStates),
   length(AllStates,L).
   L = 3125
*/   
%
% Speed testing by demanding it generate something impossible:
% All dimensionality-20 binary possiblities:
%
/*
   ?-
   time(tuple_via_coroutines(foo,0,1,20)).
   18,874,416 inferences, 4.598 CPU in 4.621 seconds (100% CPU, 4105324 Lips)
*/   
%
% All dimensionality-12 quaternary possiblities:
%
/*
   ?-
   time(tuple_via_coroutines(foo,0,3,12)).
   257,250,679 inferences, 63.291 CPU in 63.642 seconds (99% CPU, 4064545 Lips)
*/    
% =============================================================================

% ---
% Top-level predicate
% ---

tuple_via_coroutines(Tuple,Min,Max,Dim) :-          % "Tuple" is a solution if...
   tuple_initial(TupleInitial,Min,Dim),             % "TupleInitial" is the intial Tuple
   ContInitial=generator(TupleInitial,Min,Max,Dim), % "ContInitial" is the intial Continuation
   emitter(ContInitial,Tuple).                      % and emitter/2 succeeds with "Tuple" (more solutions can be found on backtracking)
   
% ---
% Emitter coroutine (the "master coroutine" which calls reset/3 repeatedly)
% ---

emitter(Cont,Tuple) :-   
   reset(Cont,Response,ContNext),             % Switch to the coroutine indicated by "Cont", getting a "Response" (the "shifted term") and a new continuation "ContNext" on return
   !,                                         % If generator called shift/1 successfully, there is no need to back (which would mean backtracking into the generator), so cut.
   ((ContNext==0) -> fail ; true),            % If ContNext is 0, the coroutine succeeded with no choicepoints. This means we have found the last tuple and thus must fail in emitter/2.
   emitter_branch(ContNext,Response,Tuple).   % Otherwise there are two branches, see below.
     
emitter_branch(_Cont,solution(Tuple),Tuple).  % Dump the continuation and succeed so that tuple_via_coroutines/4 can emit the next tuple 
     
emitter_branch(Cont,_Response,Tuple) :-       % Dump the response from the generator and (recursively) call emitter/2 recursively to get the next response from the generator.
   emitter(Cont,Tuple).
   
% ---
% Collector coroutine (generate a sequence of Tuples, which are transferred to the  via shift/1)
% ---     
   
generator(Tuple,Min,Max,Dim) :-
   shift(solution(Tuple)),                    % Send the term "solution(Tuple)" to the emitter.
   !,                                         % If the emitter called reset/3 on us again, there is no need to back (which would mean backtracking into the emitter), so cut.
   (tuple_next(Min,Max,Dim,Tuple,TupleNext)   % If a "TupleNext" could be successfully computed...
    -> generator(TupleNext,Min,Max,Dim)       % Then call generator recursively with the "TupleNext" as the new tuple to shift
    ; true).                                  % Otherwise, we are at the end so "succeed with no choicepoints" (do not "fail")

% ===
% Get an initial state where everything is a "Min"
% ===

tuple_initial(Tuple,Min,Dim) :-
   DimMax is Dim-1,
   bagof(D:Min,between(0,DimMax,D),List),
   dict_create(Tuple,s,List).

% ===
% state_next(+TupleIn,?TupleOut,+Min,+Max)
%
% Advance the state by one step; fails if that is not possible.
% ===

tuple_next(Min,Max,Dim,TupleIn,TupleOut) :-
   pos_advances(0,Min,Max,Dim,TupleIn,TupleOut).

% ===
% pos_advance(+Pos,+Min,+Max,+Dim,+TupleIn,?TupleOut)
%
% Advance position Pos (0..D-1) of TupleIn "by 1", possibly carrying into higher
% positions Px > Pos, yielding TupleOut.
% ===

pos_advances(Pos,Min,Max,Dim,TupleIn,TupleOut) :-
   get_dict(Pos,TupleIn,Max),                     % carry situation: "Max" at "Pos": need to advance the next-slowest position
   !,
   PosNext is Pos+1,                              % next position is "slower"
   PosNext<Dim,                                   % fail if we fell out at the topmost position
   put_dict(Pos,TupleIn,Min,T1),                  % Value at Pos is reset to Min
   pos_advances(PosNext,Min,Max,Dim,T1,TupleOut). % Tail recursion to "ripple down" to the slower position
 
pos_advances(Pos,_Min,Max,_Dim,TupleIn,TupleOut) :-
   get_dict(Pos,TupleIn,Value),                   % fails if Pos does not exist, which means we have "fallen out at the top"
   assertion(Value < Max),                        % not carry situation: need to advance the next-slowest position
   ValueNext is Value+1,   
   put_dict(Pos,TupleIn,ValueNext,TupleOut).      % Value at Pos is set; no recursive call/moving to the next-highest position
 
 
