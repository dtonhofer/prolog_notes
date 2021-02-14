:- module(tuple_via_generation,
          [
          tuple_via_generation/4  % tuple_via_generation(Tuple:Dict,Min:Integer,Max:Integer:Dim:Integer)
          ]).
          
% =============================================================================
% Enumerate all tuples of "D" (dimensionality) integers ranged from "Min" to
% "Max" both inclusive (i.e. the numbers of D places of radix Max-Min+1)
%
% A tuple is represented by an SWI-Prolog dict where the keys are integers
% going from 0 to D-1 and the values are from [Min,Max]. 
%
% Enumerate tuples using a "generational approach", i.e. using an accumulator
% or an iteration.
%
% The idea is that the core predicate is given Tuple T, and succeeds with tuple T,
% and on backtracking increments the Tuple to TNext, then calls itself with
% Tuple T.
%
% This is quite fast.
%
% The code based on the inductive definition is looking downwards towards the
% base case:
% A solution is either the base case tuple or a tuple of a solution "nearer 
% the base case", plus 1.
% 
% The code based on the generational approach is looking upwards from a starter 
% case (not necessarily the base case, but the kickstarting predicate indeed
% sets the starter case to the base case):
% A solution is either the starter case tuple or a solution that is "higher",
% i.e. the tuple "starter case tuple + 1" or larger.
%
% Example
% =======
%
% On backtracking, this generates all possibilities. 
%
% Generate all 3125 entries of the set with value range 1..5, 
% dimensionalty 5:
%
% ?- bagof(T,tuple_via_generation(T,1,5,5),AllStatesUnsorted),
%    sort(AllStatesUnsorted,AllStates),
%    length(AllStates,L).
% L = 3125
%
% Speed testing by demanding it generate something impossible:
%
% ?- time(tuple_via_generation(foo,0,1,20)).
%    13,631,533 inferences, 2.802 CPU in 2.819 seconds (99% CPU, 4865062 Lips)
%
% All dimensionality-12 ternary possiblities:
%
% ?- time(tuple_via_generation(foo,0,3,12)).
%    55,924,081 inferences, 21.265 CPU in 21.368 seconds (100% CPU, 2629840 Lips)
%
% ?- time(tuple_via_generation(foo,0,3,12)).
%    173,364,596 inferences, 34.499 CPU in 34.681 seconds (99% CPU, 5025150 Lips)
%
% =============================================================================

% ===
% tuple_via_generation(Tuple:Dict,Min:Integer,Max:Integer:Dim:Integer)
%
% It kickstarts the generation
% ===

tuple_via_generation(Tuple,Min,Max,Dim) :-
   tuple_initial(TupleInitial,Min,Dim),
   tuple_via_generation_2(TupleInitial,Tuple,Min,Max,Dim).

% ===
% tuple_via_generation_2(+TupleIn:Dict,-TupleOut:Dict,+Min:Int,+Max:int,+Dim:int).
%
% Given a TupleIn, generate successively "later" TupleOut
% created by "incrementing" TupleIn, including incrementing it by "0".
% ===

% First case: Increment by "0" yields a successful solution.

tuple_via_generation_2(Tuple,Tuple,_Min,_Max,_Dim).

% Otherwise: Increment TupleIn by "1" (which fails if TupleIn was the last
% tuple possible) and then do a recursive call.

tuple_via_generation_2(TupleIn,TupleOut,Min,Max,Dim) :-
   tuple_next(Min,Max,Dim,TupleIn,TuplePlus),
   tuple_via_generation_2(TuplePlus,TupleOut,Min,Max,Dim).

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
 
 
